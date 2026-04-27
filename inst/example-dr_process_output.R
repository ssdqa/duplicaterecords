
#' Source setup file
source(system.file('setup.R', package = 'duplicaterecords'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'dr_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-15000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(20000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Execute `dr_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
dr_process_example <- dr_process(cohort = cohort,
                                 multi_or_single_site = 'single',
                                 anomaly_or_exploratory = 'exploratory',
                                 time = FALSE,
                                 omop_or_pcornet = 'omop',
                                 dr_input_file = dr_input_file_omop) %>%
  suppressMessages()

dr_process_example

#' Execute `dr_output` function
dr_output_example <- dr_output(process_output = dr_process_example,
                               output_col = 'duplicate_row_prop')

dr_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(dr_output_example)
