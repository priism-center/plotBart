#' @title Plot common support based on the standard deviation rule, chi squared rule, or both
#' @description Plot common support based on the standard deviation rule, chi squared rule, or both.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param .x a character string denoting which covariate to use a the x axis default is propensity score
#' @param rule one of c('both', 'sd', 'chi') denoting which rule to use to identify lack of support
#'
#' @details Sufficient overlap/common support is an assumption of causal inference.
#' BART models use the uncertainty of counter factual uncertainty.
#' When the posterior distribution of an individual's counterfactual prediction extends beyond a specified cut-point, that point likely has insufficient common support.
#' 'bartCause' model offer the option to automatically remove points without common support from analyses, however, this must be specified during model fitting.
#' Cut-points are determined through one of two rules: the standard deviation (sd) or chi-squared (chi).
#' Under the standard deviation rule, a point has weak common support if its posterior distribution of the counterfactual deviation is greater than the maximum posterior of the observed predictions with 1 standard deviation of the distribution of standard deviations for each individual's predicted outcome under the observed assignment.
#' Under the chi-squared rule, a point is discarded if the variance between its counterfactual prediction over observed prediction are statistically different under a chi-squared distribution with 1 degree of freedom. For more details on discard rules see Hill and Su 2013.
#'
#' When called this plot will show how many points would have been removed under the standard deviation and chi-squared rules. This plot should be used as a diagnostic for 'bartCause' models fit without a common-support rule.
#'
#' @references
#' Hill, J., & Su, Y. S. (2013).
#' Assessing lack of common support in causal inference using Bayesian nonparametrics: Implications for evaluating the effect of breastfeeding on children's cognitive outcomes.
#' The Annals of Applied Statistics,
#' 1386-1420.
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_common_support(model_results)
#' plot_common_support(model_results, .x = 'age')
#' }
plot_common_support <- function(.model, .x = 'Propensity Score',  rule = c('both', 'sd', 'chi')){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  rule <- rule[1]
  if (rule %notin% c('both', 'sd', 'chi')) stop('rule must be one of c("both", "sd", "chi")')
  if (rule == 'both') rule <- c('sd', 'chi')

  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  total_sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )

  inference_group <- switch (.model$estimand,
                             ate = length(.model$sd.obs[!.model$missingRows]),
                             att = length(.model$sd.obs[!.model$missingRows] & .model$trt == 1),
                             atc = length(.model$sd.obs[!.model$missingRows] & .model$trt == 0)
  )
  prop_sd <- round((total_sd / inference_group)*100 , 2)
  text_sd <- paste0('Standard deviation rule: ', prop_sd, "% of cases would have been removed")

  # calculate summary stats
  total_chi <- sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841)
  prop_chi <- round(total_chi / length(.model$sd.cf), 5)*100
  text_chi <- paste0('Chi-squared rule: ', prop_chi, "% of cases would have been removed")

  # create dataframe of the sd and chi values
  dat <- as_tibble(.model$data.rsp@x) %>%
    rename(`Propensity Score` = ps)

  dat.sd <- dat %>%
    mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
           removed = if_else(.model$sd.cf > sd.cut, 'Removed', 'Included'),
           support_rule = 'sd',
           stat = .model$sd.cf,
           sd.cf = .model$sd.cf,
           support_rule_text = text_sd) %>%
    select(-sd.cut)


  dat.chi <- dat %>%
    mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 'Removed', 'Included'),
           support_rule = 'chi',
           stat = (.model$sd.cf / .model$sd.obs) ** 2,
           sd.cf = .model$sd.cf,
           support_rule_text = text_chi)

  dat <- rbind(dat.sd, dat.chi)

  if(.model$estimand == 'att') dat <- dat[.model$trt == 1,]
  if(.model$estimand == 'atc') dat <- dat[.model$trt == 0,]


  # plot it
  p <- dat %>%
    filter(support_rule %in% rule) %>%
    ggplot(aes(x = !!rlang::sym(.x), y = sd.cf, color = removed)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c(1, 2)) +
    facet_wrap(~support_rule_text, ncol = 1, scales = 'free_y') +
    labs(title ="Common support checks",
         x = .x,
         y = 'Predicted counterfactual standard deviation',
         color = NULL) +
    theme(legend.position = 'bottom',
          strip.text = element_text(hjust = 0))

  return(p)
}
