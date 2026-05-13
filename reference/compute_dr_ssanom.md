# Single Site Anomaly Detection method for DR

Single Site Anomaly Detection method for DR

## Usage

``` r
compute_dr_ssanom(cohort, dr_ptlv_rslt, n_sd = 2)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, `person_id`,
  `start_date`, and `end_date`

- dr_ptlv_rslt:

  patient level results output by `dr_process`

- n_sd:

  numeric indicating the number of standard deviations away from the
  mean that will indicate an outlier (defaults to 2)
