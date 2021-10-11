#' Plot histogram of individual treatment effects
#'
#' Returns a ggplot ITE plot. TODO: describe what the plot is and how it should be used
#'
#' @param .model a model produced by bartCause::bartc()
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_ITE(model_results)
plot_ITE <- function(.model){

  # ensure model is a of class bartcFit
  validate_model(.model)

  # calculate stats
  ites <- bartCause::extract(.model, 'icate')
  ite.m <- apply(ites, 2, mean)
  sd.ite <- apply(ites, 2, sd)
  ite.ub <- ite.m + 1.96 * sd.ite
  ite.lb <-  ite.m - 1.96 * sd.ite

  # plot it
  p <- tibble(ite.m, ite.ub, ite.lb) %>%
    ggplot(aes(x = ite.m)) +
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey60') +
    geom_histogram(alpha = 0.8) +
    labs(title = 'Individual Treatment Effects',
         x = "Individual Conditional Treatment Effects",
         y = 'Frequency')

  return(p)
}
