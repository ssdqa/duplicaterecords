# *Multi Site, Exploratory, Cross-Sectional*

*Multi Site, Exploratory, Cross-Sectional*

## Usage

``` r
dr_ms_exp_cs(
  process_output,
  output_col,
  filter_definition = NULL,
  text_wrapping_char = 60L,
  facet = NULL,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  tabular output from `dr_process`

- output_col:

  name of the column from process_output to be used in the analysis. can
  be either of the proportion columns or either of the median_site\_\*
  columns

- filter_definition:

  the name(s) of duplicate_definitions that should be included on the
  plot

- text_wrapping_char:

  an integer indicating the length limit for text wrapping on axis text

- facet:

  fields by which the graph should be facetted

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

if a proportion column is selected as the output_col, a heatmap with the
proportion of unmapped values at each site for each duplicate_definition
will be returned. if a median column is selected, a dot plot comparing
site medians (with a star for the all-site median) for each
duplicate_definition will be returned.
