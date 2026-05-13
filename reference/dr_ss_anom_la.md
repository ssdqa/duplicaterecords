# *Single Site, Anomaly Detection, Longitudinal*

*Single Site, Anomaly Detection, Longitudinal*

## Usage

``` r
dr_ss_anom_la(
  process_output,
  output_col = NULL,
  filter_definition = NULL,
  text_wrapping_char = 60L,
  facet = NULL
)
```

## Arguments

- process_output:

  tabular output from `dr_process`

- output_col:

  name of the column from process_output to be used in the analysis. can
  be either of the proportion columns

- filter_definition:

  the name(s) of duplicate_definitions that should be included on the
  plot

- text_wrapping_char:

  an integer indicating the length limit for text wrapping on axis text

- facet:

  fields by which the graph should be facetted

## Value

if analysis was executed by year or greater, a P Prime control chart is
returned with outliers marked with orange dots

         if analysis was executed by month or smaller, an STL regression is
         conducted and outliers are marked with red dots. the graphs representing
         the data removed in the regression are also returned
