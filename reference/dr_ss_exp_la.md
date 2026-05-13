# *Single Site, Exploratory, Longitudinal*

*Single Site, Exploratory, Longitudinal*

## Usage

``` r
dr_ss_exp_la(
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
  be any of the proportion or median columns

- text_wrapping_char:

  an integer indicating the length limit for text wrapping on axis text

- facet:

  fields by which the graph should be facetted

## Value

a line graph showing the proportion or median per patient of unmapped
concepts per duplicate_definition across the time series
