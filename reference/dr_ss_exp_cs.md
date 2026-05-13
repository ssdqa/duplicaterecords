# *Single Site, Exploratory, Cross-Sectional*

*Single Site, Exploratory, Cross-Sectional*

## Usage

``` r
dr_ss_exp_cs(
  process_output,
  output_col,
  text_wrapping_char = 60L,
  facet = NULL
)
```

## Arguments

- process_output:

  tabular output from `dr_process`

- output_col:

  name of the column from process_output to be used in the analysis. can
  be any of the numeric cols, including either proportion col or any of
  the median cols

- text_wrapping_char:

  an integer indicating the length limit for text wrapping on axis text

- facet:

  fields by which the graph should be facetted

## Value

a bar graph displaying either the proportion of unmapped rows/patients
or the median number of unmapped values per patient for each
duplicate_definition
