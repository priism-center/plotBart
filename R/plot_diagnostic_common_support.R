#' Plot common support based on the standard deviation rule, chi squared rule or both
#'
#' Returns a ggplot common support plot
#'
#' @param .model a model produced by bartCause::bartc()
#' @param .rule one of c('none', 'sd', 'chi') denoting which rule to use to identify lack of support
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr patchwork
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
#' plot_diagnostic_common_support(model_results)
plot_diagnostic_common_support <- function(.model, .rule = c('none', 'sd', 'chi')){

  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")

  # create SD plot ----------------------------------------------------------
  # calculate summary stats
  total <- sum(.model$sd.cf > max(.model$sd.obs) + sd(.model$sd.obs))
  prop <- total / length(.model$sd.cf)
  sd_test <- paste0(prop, "% of cases would have been removed by the standard deviation common support check")

  # plot it
  sd_plot <- .model$sd.cf %>%
    as_tibble() %>%
    mutate(rownumber = row_number()) %>%
    ggplot(aes(rownumber, value)) +
    geom_point(alpha = 0.8)+
    geom_hline(aes(yintercept = max(.model$sd.obs) + sd(.model$sd.obs)),
               color = 'coral3', linetype = 'dashed') +
    labs(title ="Diagnostics: Common Support Checks",
         subtitle = paste0("Standard Deviation method: ", sd_test),
         x = NULL, #"Row index",
         y = 'Counterfactual Uncertanty') +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')


  # Create Chi Sqr plot -----------------------------------------------------
  # calculate summary stats
  total <- sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841)
  prop <- total / length(.model$sd.cf)
  chi_test <- paste0(prop, "% of cases would have been removed by the chi squred common support check")

  # plot it
  chi_sqr_plot <- (.model$sd.cf / .model$sd.obs)**2  %>%
    as_tibble() %>%
    mutate(rownumber = row_number()) %>%
    ggplot(aes(rownumber, value)) +
    geom_point(alpha = 0.8) +
    geom_hline(aes(color = 'Removal threshold', yintercept = 3.841), linetype = 'dashed') +
    scale_color_manual(values = 'coral3') +
    labs(title = NULL,
         subtitle = paste0("Chi Squared method: ", chi_test),
         x = "Row index",
         y = 'Counterfactual Uncertanty') +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')


  # Combine Plots  ----------------------------------------------------------

  if(.rule == 'none') {
    return(sd_plot/chi_sqr_plot)
  }

  if(.rule == 'sd'){
    return(sd_plot)
  }

  if(.rule == 'chi'){
    return(chi_sqr_plot)
  }

}
