### this is a dev script that mimics the Shiny app analysis workflow
### useful for testing functions
library(tidyverse)
library(bartCause)
library()
theme_set(theme_minimal())


# setup -------------------------------------------------------------------

X <- read_csv("dev_tmp/lalonde.csv")
X <- dplyr::select(X, 'treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
X <- clean_auto_convert_logicals(X)

treatment_v <- X[, 1]
response_v <- X[, 2]
confounders_mat <- as.matrix(X[, 3:ncol(X)])

# run model
model_results <- bartCause::bartc(
  response = response_v,
  treatment = treatment_v,
  confounders = confounders_mat,
  estimand = 'ate',
  commonSup.rule = 'none'
)


# functions to test -------------------------------------------------------

plot_ITE(model_results)
# plot_cate_test(model_results, confounders_mat)
plot_diagnostic_common_support(model_results)
plot_trace(model_results)
