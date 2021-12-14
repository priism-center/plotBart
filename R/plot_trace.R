#' Trace plot the estimands of a bartCause::bartc() model
#'
#' Returns a ggplot of the estimated effect over each iteration of the model fit. TODO: describe what the plot is and how it should be used
#'
#' @param .model a bartCause::bartc() model, typically store$model_results
#' @author Joe Marlo, George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' data(lalonde)
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

  # ensure model is a of class bartcFit
  validate_model_(.model)

  p <- .model %>%
    bartCause::extract('cate', combineChains = FALSE) %>%
    t() %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(1:.model$n.chains) %>%
    mutate(Chain = factor(sub('V', '', name), levels = as.character(1:10))) %>%
    ggplot(aes(x = iteration, y = value, color = Chain)) +
    geom_line(alpha = 0.8) +
    labs(title = 'Diagnostics: Trace plot',
         x = 'Iteration',
         y = toupper(.model$estimand),
         color = 'Chain')

  return(p)
}
