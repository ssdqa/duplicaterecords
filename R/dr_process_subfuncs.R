
#' Compute duplicate record distributions & per patient medians
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param dr_tbl A table with information about each of the definitions of duplication that should be examined
#'   in the analysis. This table should contain the following columns:
#'   - `definition_alias` | *character* | a string to identify the definition of duplication used for the analysis
#'   - `domain_tbl` | *character* | the CDM table that should be evaluated for duplicate rows
#'   - `duplicate_columns` | *character* | the name of the field where unmapped values should be identified
#'   - `include_exclude` | *character* | string to control whether the duplicate_columns are all included or all excluded to define duplicates
#'   - `concept_field` | *character* | the string name of the field in the domain table where the concepts are located (if codeset is provided)
#'   - `date_field` | *character* | the name of the field in the domain table with the date that should be used for temporal filtering
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'   - `codeset_name` | *character* | (optional) the name of the codeset that can be used to define a variable of interest (ex: evaluate unit completeness for a specific drug)
#'   - `filter_logic` | *character* | (optional) logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a dplyr::filter command in R
#' @param site_col the name of the column with site information (either site or site_summ)
#' @param grouped_list list of columns that should be used to group the analysis tables
#' @param time boolean indicating whether the analysis is being executed over time
#' @param omop_or_pcornet string indicating the data model of the underlying CDM data (either omop or pcornet)
#'
#' @importFrom purrr set_names
#' @importFrom purrr reduce
#' @importFrom rlang parse_expr
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stats median
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
#'
compute_dr <- function(cohort,
                       dr_tbl,
                       site_col = 'site',
                       grouped_list = 'site',
                       time = FALSE,
                       omop_or_pcornet = 'omop'){

  dr_list <- split(dr_tbl, seq(nrow(dr_tbl)))

  check_concepts <- list()
  pt_concepts <- list()

  for(i in 1:length(dr_list)) {

    varb <- dr_list[[i]]$definition_alias

    cli::cli_inform(paste0('Starting ', varb))

    if(omop_or_pcornet == 'omop'){
      join_cols <- purrr::set_names('concept_id', dr_list[[i]]$concept_field)
      person_col <- 'person_id'
    }else{
      join_cols <- purrr::set_names('concept_code', dr_list[[i]]$concept_field)

      if(!is.na(dr_list[[i]]$vocabulary_field)){
        join_cols2 <- purrr::set_names('vocabulary_id', dr_list[[i]]$vocabulary_field)
        join_cols <- join_cols %>% append(join_cols2)
      }

      person_col <- 'patid'
    }

    domain_tbl <- cdm_tbl(dr_list[[i]]$domain_tbl) %>%
      inner_join(cohort) %>%
      filter(!!sym(dr_list[[i]]$date_field) >= start_date &
               !!sym(dr_list[[i]]$date_field) <= end_date) %>%
      group_by(!!!syms(grouped_list))

    if(time){
      domain_tbl <- domain_tbl %>%
        filter(!!sym(dr_list[[i]]$date_field) >= time_start &
                 !!sym(dr_list[[i]]$date_field) <= time_end) %>%
        group_by(time_start, time_increment, .add = TRUE)
    }

    if(!is.na(dr_list[[i]]$filter_logic)){
      tbl_use <- domain_tbl %>%
        filter(!! rlang::parse_expr(dr_list[[i]]$filter_logic))
    }else{tbl_use <- domain_tbl}

    if(!is.na(dr_list[[i]]$codeset_name)){
      tbl_use <- tbl_use %>%
        inner_join(load_codeset(dr_list[[i]]$codeset_name), by = join_cols)
    }else{tbl_use <- tbl_use}

    ## duplicate info
    cols <- dr_list[[i]]$duplicate_columns %>%
      stringr::str_replace_all(' ', '') %>%
      stringr::str_split(., ',')
    cols <- cols[[1]]

    if(all(is.na(cols))){
      dupe_use <- tbl_use %>%
        group_by(across(everything()))
    }else{
      if(tolower(dr_list[[i]]$include_exclude) == 'include'){
        dupe_use <- tbl_use %>%
          select(!!!syms(cols)) %>%
          group_by(!!!syms(cols), .add = TRUE)

        pt_use <- tbl_use %>%
          select(!!sym(person_col), !!!syms(cols)) %>%
          group_by(!!!syms(cols), .add = TRUE)
      }else{
        dupe_use <- tbl_use %>%
          select(-c(!!!syms(cols))) %>%
          group_by(across(everything()))

        cols_pt <- cols[!cols %in% person_col]

        pt_use <- tbl_use %>%
          select(-c(!!!syms(cols))) %>%
          group_by(across(everything()))
      }
    }

    dupe_vals <- dupe_use %>%
      group_by(!!!syms(grouped_list), .add = TRUE) %>%
      summarise(row_ct = n()) %>%
      mutate(dup_yn = ifelse(row_ct > 1, T, F)) %>%
      group_by(!!!syms(grouped_list), dup_yn) %>%
      summarise(duplicate_rows = sum(row_ct)) %>%
      filter(dup_yn == T) %>%
      select(-dup_yn) %>%
      collect()

    per_pt <- pt_use %>%
      group_by(!!sym(person_col), !!!syms(grouped_list), .add = TRUE) %>%
      summarise(row_pp = n()) %>%
      mutate(dup_yn = ifelse(row_pp > 1, T, F)) %>%
      group_by(!!!syms(grouped_list), !!sym(person_col), dup_yn) %>%
      summarise(duplicate_row_pp = sum(row_pp)) %>%
      #filter(dup_yn == T) %>%
      #select(-dup_yn) %>%
      collect()

    ## proportion
    total_pts <- cohort %>%
      summarise(total_pt = n_distinct(person_id),
                duplicate_definition = varb) %>%
      collect()

    dupe_pts <- per_pt %>%
      group_by(!!!syms(grouped_list)) %>%
      filter(dup_yn == T) %>%
      summarise(duplicate_pt = n_distinct(person_id))

    total_rows <- tbl_use %>%
      summarise(
        duplicate_definition = varb,
        total_rows = n()
      ) %>% collect()

    if(nrow(dupe_vals) < 1){
      dupe_vals <-
        dplyr::tibble(
          duplicate_rows = 0L,
          duplicate_definition = varb
        )
    }

    if(nrow(dupe_pts) < 1){
      dupe_pts <-
        dplyr::tibble(
          duplicate_pt = 0L,
          duplicate_definition = varb
        )
    }

    ## per patient
    per_pt <- per_pt %>%
      mutate(duplicate_row_pp = ifelse(dup_yn, duplicate_row_pp, 0L)) %>%
      select(-dup_yn)

    site_meds <- per_pt %>%
      ungroup(!!sym(person_col)) %>%
      summarise(median_site_with0s = as.numeric(median(duplicate_row_pp)),
                median_site_without0s = as.numeric(median(duplicate_row_pp[duplicate_row_pp!=0])),
                duplicate_definition = varb) %>%
      mutate(median_site_without0s = ifelse(is.na(median_site_without0s),
                                            0L, median_site_without0s))

    all_meds <- per_pt %>%
      ungroup(!!sym(person_col), !!sym(site_col)) %>%
      summarise(median_all_with0s = as.numeric(median(duplicate_row_pp)),
                median_all_without0s = as.numeric(median(duplicate_row_pp[duplicate_row_pp!=0])),
                duplicate_definition = varb) %>%
      mutate(median_all_without0s = ifelse(is.na(median_all_without0s),
                                           0L, median_all_without0s))

    ## combined
    duplicate_cts <-
      total_rows %>%
      left_join(total_pts) %>%
      left_join(dupe_vals %>% mutate(duplicate_definition = varb)) %>%
      left_join(dupe_pts %>% mutate(duplicate_definition = varb)) %>%
      left_join(all_meds) %>%
      left_join(site_meds) %>%
      mutate(
        duplicate_rows = ifelse(is.na(duplicate_rows), 0, duplicate_rows),
        duplicate_pt = ifelse(is.na(duplicate_pt), 0, duplicate_pt),
        duplicate_row_prop = round(as.numeric(duplicate_rows) / as.numeric(total_rows), 2),
        duplicate_row_prop = ifelse(is.na(duplicate_row_prop), 0, duplicate_row_prop),
        duplicate_pt_prop = round(as.numeric(duplicate_pt) / as.numeric(total_pt), 2),
        duplicate_pt_prop = ifelse(is.na(duplicate_pt_prop), 0, duplicate_pt_prop)
      )

    check_concepts[[i]] <- duplicate_cts
    pt_concepts[[i]] <- per_pt %>% mutate(duplicate_definition = varb)

  }

  check_concepts_red <- purrr::reduce(.x = check_concepts,
                                      .f = dplyr::union)
  pt_concepts_red <- purrr::reduce(.x = pt_concepts,
                                   .f = dplyr::union)

  opt <- list('summary' = check_concepts_red,
              'pt_lv' = pt_concepts_red)

  return(opt)
}


#' Single Site Anomaly Detection method for DR
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param dr_ptlv_rslt patient level results output by `dr_process`
#' @param n_sd numeric indicating the number of standard deviations away from the mean that will indicate an outlier (defaults to 2)
#'
#' @keywords internal
#'
compute_dr_ssanom <- function(cohort,
                              dr_ptlv_rslt,
                              n_sd = 2){

  n_tot <- cohort %>%
    summarise(ct = n()) %>%
    collect() %>% pull(ct)

  all_vals <- dr_ptlv_rslt %>%
    group_by(site, duplicate_definition) %>%
    summarise(mean_tot=mean(duplicate_row_pp),
              sd_tot=sd(duplicate_row_pp),
              n_tot=n_tot)

  all_vals <- dr_ptlv_rslt %>%
    left_join(all_vals) %>%
    mutate(zscore_tot = ((duplicate_row_pp - mean_tot) / sd_tot),
           abs_z = abs(zscore_tot),
           outlier = case_when(zscore_tot > n_sd ~ 1L,
                               TRUE ~ 0L)) %>%
    group_by(site, duplicate_definition, n_tot, sd_tot, mean_tot) %>%
    mutate(outlier_tot = sum(outlier),
           prop_outlier_tot = round(outlier_tot / n_tot, 3)) %>%
    select(group_vars(.), n_tot, outlier_tot, mean_tot, sd_tot, prop_outlier_tot) %>%
    ungroup() %>% distinct()

  fact_vals <- dr_ptlv_rslt %>%
    filter(duplicate_row_pp != 0) %>%
    group_by(site, duplicate_definition) %>%
    summarise(mean_fact=mean(duplicate_row_pp),
              sd_fact=sd(duplicate_row_pp),
              n_w_fact=n())

  fact_vals <- dr_ptlv_rslt %>%
    filter(duplicate_row_pp != 0) %>%
    left_join(fact_vals) %>%
    mutate(zscore_fact = ((duplicate_row_pp - mean_fact) / sd_fact),
           abs_z = abs(zscore_fact),
           outlier = case_when(zscore_fact > n_sd ~ 1L,
                               TRUE ~ 0L)) %>%
    group_by(site, duplicate_definition, n_w_fact, sd_fact, mean_fact) %>%
    mutate(outlier_fact = sum(outlier),
           prop_outlier_fact = round(outlier_fact / n_w_fact, 3)) %>%
    select(group_vars(.), n_w_fact, outlier_fact, mean_fact, sd_fact, prop_outlier_fact) %>%
    ungroup() %>% distinct()

  ## combined
  duplicate_cts <- all_vals %>% left_join(fact_vals) %>% replace(is.na(.), 0)

  return(duplicate_cts)
}
