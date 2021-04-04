#' Trace plot the estimands of a bartCause::bartc() model
#'
#' Returns a ggplot of the estimated effect over each iteration of the model fit.
#'
#' @param .model a bartCause::bartc() model, typically store$model_results
#' @author Joe Marlo, George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' data(lalonde, package = 'arm')
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_trace(.model = model_results)
plot_trace <- function(.model){

  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")

  p <- .model %>%
    bartCause::extract() %>%
    as_tibble() %>%
    mutate(index = row_number()) %>%
    ggplot(aes(x = index, y = value)) +
    geom_line() +
    labs(title = 'Diagnostics: Trace plot',
         subtitle = 'Informative subtitle to go here',
         x = 'Iteration',
         y = base::toupper(.model$estimand))

  return(p)
}
