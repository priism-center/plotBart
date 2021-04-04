### this is a dev script that mimics the Shiny app analysis workflow
### useful for testing functions
library(tidyverse)
library(bartCause)
devtools::load_all()
theme_set(theme_minimal())


# functions ---------------------------------------------------------------

clean_auto_convert_logicals <- function(input_data){
  # function converts columns of 0:1, T:F, True:False to logicals

  for (col in colnames(input_data)){

    # is the column exclusively in list of pre-determined
    inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
    col_as_char <- as.character(input_data[[col]])
    col_cleaned <- base::tolower(unique(col_as_char))
    is_in_list <- length(setdiff(col_cleaned, inclusion_list)) == 0

    # convert column to logical
    if (isTRUE(is_in_list)){
      input_data[,col] <- readr::parse_logical(col_as_char)
    }
  }

  return(input_data)
}


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


plot_balance(X, 'treat', c('re78', 'age', 'educ'))
plot_ITE(model_results)
# plot_cate_test(model_results, confounders_mat)
plot_diagnostic_common_support(model_results)
plot_trace(model_results)
