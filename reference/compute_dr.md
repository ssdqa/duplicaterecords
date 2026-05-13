# Compute duplicate record distributions & per patient medians

Compute duplicate record distributions & per patient medians

## Usage

``` r
compute_dr(
  cohort,
  dr_tbl,
  site_col = "site",
  grouped_list = "site",
  time = FALSE,
  omop_or_pcornet = "omop"
)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, `person_id`,
  `start_date`, and `end_date`

- dr_tbl:

  A table with information about each of the definitions of duplication
  that should be examined in the analysis. This table should contain the
  following columns:

  - `definition_alias` \| *character* \| a string to identify the
    definition of duplication used for the analysis

  - `domain_tbl` \| *character* \| the CDM table that should be
    evaluated for duplicate rows

  - `duplicate_columns` \| *character* \| the name of the field where
    unmapped values should be identified

  - `include_exclude` \| *character* \| string to control whether the
    duplicate_columns are all included or all excluded to define
    duplicates

  - `concept_field` \| *character* \| the string name of the field in
    the domain table where the concepts are located (if codeset is
    provided)

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  - `codeset_name` \| *character* \| (optional) the name of the codeset
    that can be used to define a variable of interest (ex: evaluate unit
    completeness for a specific drug)

  - `filter_logic` \| *character* \| (optional) logic to be applied to
    the domain_tbl in order to achieve the definition of interest;
    should be written as if you were applying it in a dplyr::filter
    command in R

- site_col:

  the name of the column with site information (either site or
  site_summ)

- grouped_list:

  list of columns that should be used to group the analysis tables

- time:

  boolean indicating whether the analysis is being executed over time

- omop_or_pcornet:

  string indicating the data model of the underlying CDM data (either
  omop or pcornet)
