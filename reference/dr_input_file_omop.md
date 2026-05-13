# DR Sample Input File – OMOP

A sample version of the file structure expected for the dr_input_file
parameter in the `dr_process` function. The user should recreate this
file structure and include their own variables.

## Usage

``` r
dr_input_file_omop
```

## Format

### `dr_input_file_omop`

A data frame with 8 columns

- definition_alias:

  A string alias to identify the definition of duplication used for the
  analysis

- domain_tbl:

  The name of the CDM table where the variable can be found

- duplicated_columns:

  A comma separated string with columns names to be included or excluded
  to determine how duplicates are identified

- include_exclude:

  A string, either include or exclude, defining what action should be
  taken in relation to the duplicate_columns to properly define
  duplicates

- concept_field:

  The field in the domain_tbl that should be used to join to the
  codeset; only required when codeset is provided

- date_field:

  The date field in the domain_tbl that should be used to filter the
  dataset to the cohort period and for longitudinal analyses

- codeset_name:

  The name of the codeset as found in the specs directory; file
  extension should not be included

- filter_logic:

  optional; a string to be parsed as logic to filter the default_tbl and
  better identify the variable of interest

## Details

Please note that the codesets should be stored in the
`file_subdirectory` established when `ssdqa.gen::initialize_dq_session`
is executed.

Examples of appropriately structured codeset files are attached to the
ssdqa.gen package and can be accessed with `ssdqa.gen::`
