
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotBart

<!-- badges: start -->

[![R-CMD-check](https://github.com/joemarlo/plotBart/workflows/R-CMD-check/badge.svg)](https://github.com/joemarlo/plotBart/actions)
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](/LICENSE.md)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--09--30-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

plotBart is a supplemental plotting package to
[bartCause](https://github.com/vdorie/bartCause) and
[thinkCausal](https://github.com/gperrett/thinkCausal_dev).

``` r
library(plotBart)
data(lalonde, package = 'arm')
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')

# fit BART model
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none',
  verbose = FALSE
)

# plot common support
plot_diagnostic_common_support(.model = model_results)
```

<img src="man/figures/README-example-1.png" width="75%" style="display: block; margin: auto;" />

``` r
# plot overlap by variable
plot_overlap_vars(
  .data = lalonde,
  treatment_col = 'treat',
  confounder_cols = confounders,
  plt_type = 'Histogram'
) + labs(subtitle = 'My informative subtitle')
```

<img src="man/figures/README-example-2.png" width="75%" style="display: block; margin: auto;" />

## Installation

plotBart is currently in development and is available to test by
installing via:

``` r
# install.packages("devtools")
devtools::install_github("joemarlo/plotBart")
```

## Dev to-do

-   Write vignette
-   Remove functions from thinkCausal repo
-   Fix issue in plot\_variable\_importance. See TODO line 46

### Test coverage

    #> plotBart Coverage: 59.78%
    #> R/plot_cate_test.R: 0.00%
    #> R/plot_overlap_vars.R: 55.81%
    #> R/plot_overlap_pScores.R: 61.70%
    #> R/plot_variable_importance.R: 81.25%
    #> R/plot_diagnostic_common_support.R: 88.10%
    #> R/plot_trace.R: 91.67%
    #> R/plot_ITE.R: 92.86%
    #> R/plot_balance.R: 95.65%
