
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotBart

<!-- badges: start -->

[![R-CMD-check](https://github.com/joemarlo/plotBart/workflows/R-CMD-check/badge.svg)](https://github.com/joemarlo/plotBart/actions)
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](/LICENSE.md)
[![last
commit](https://img.shields.io/github/last-commit/joemarlo/plotBart)](https://github.com/joemarlo/plotBart/commits/master)
<!-- badges: end -->

plotBart is a diagnostic and plotting package for
[bartCause](https://github.com/vdorie/bartCause) and
[thinkCausal](https://github.com/gperrett/thinkCausal_dev).

``` r
library(plotBart)
data(lalonde)
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
plot_common_support(.model = model_results)
```

<img src="man/figures/README-example-1.png" width="75%" style="display: block; margin: auto;" />

``` r
# plot overlap by variable and manipulate ggplot object
plot_overlap_vars(
  .data = lalonde,
  treatment = 'treat',
  confounders = confounders,
) + 
  labs(subtitle = 'My comments on the results') +
  theme_classic()
```

<img src="man/figures/README-example-2.png" width="75%" style="display: block; margin: auto;" />

## Installation

plotBart is currently in development and is available to test by
installing via:

``` r
# install.packages("devtools")
devtools::install_github("joemarlo/plotBart")
```

### Test coverage

    #> plotBart Coverage: 31.58%
    #> R/plot_c_moderator_loess.R: 0.00%
    #> R/plot_c_pd.R: 0.00%
    #> R/plot_cate.R: 0.00%
    #> R/plot_d_moderator_density.R: 0.00%
    #> R/plot_icate.R: 0.00%
    #> R/plot_moderator_linerange.R: 0.00%
    #> R/plot_moderator_search.R: 0.00%
    #> R/plot_pate.R: 0.00%
    #> R/plot_sate.R: 0.00%
    #> R/plot_waterfall.R: 0.00%
    #> R/utils.R: 66.67%
    #> R/plot_overlap_pScores.R: 90.74%
    #> R/plot_overlap_vars.R: 93.02%
    #> R/plot_balance.R: 95.83%
    #> R/plot_common_support.R: 97.14%
    #> R/plot_trace.R: 100.00%
