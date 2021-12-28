
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotBart

<!-- badges: start -->

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](/LICENSE.md)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/joemarlo/plotBart/workflows/R-CMD-check/badge.svg)](https://github.com/joemarlo/plotBart/actions)
[![last
commit](https://img.shields.io/github/last-commit/joemarlo/plotBart)](https://github.com/joemarlo/plotBart/commits/master)
[![CodeFactor](https://www.codefactor.io/repository/github/joemarlo/plotbart/badge)](https://www.codefactor.io/repository/github/joemarlo/plotbart)
[![Codecov test
coverage](https://codecov.io/gh/joemarlo/plotBart/branch/master/graph/badge.svg)](https://codecov.io/gh/joemarlo/plotBart?branch=master)
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
  verbose = FALSE,
  keepTrees = TRUE
)

# plot common support
plot_common_support(.model = model_results)
```

<img src="man/figures/README-example-1.png" width="75%" style="display: block; margin: auto;" />

``` r
# plot CATE and manipulate ggplot object
plot_CATE(
  .model = model_results, 
  type = 'density', 
  ci_80 = TRUE, 
  ci_95 = TRUE,
  .mean = TRUE
) + 
  labs(subtitle = 'My comments on the results') +
  theme_classic()
```

<img src="man/figures/README-example-2.png" width="75%" style="display: block; margin: auto;" />

## Installation

plotBart is currently in development and is available to test by
installing via:

``` r
# install.packages("remotes")
remotes::install_github("joemarlo/plotBart")
```
