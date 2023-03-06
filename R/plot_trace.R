#' @title Trace plot the estimands of a `bartCause::bartc()` model
#' @description Returns a ggplot of the estimated effect over each iteration of the model fit. This is used to visually assess the convergence of Markov chain Monte Carlo (MCMC) sampling. Chains should be well mixed such that no single color is notably separate from others.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type parameter to plot options are average treatment effects: `cate`, `sate` and `pate` as well as posterior predicitve uncertainty `sigma`
#' @author Joseph Marlo, George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
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
#'  commonSup.rule = 'none'
#' )
#' plot_trace(.model = model_results)
#' }
plot_trace <- function(.model, type = c('cate', 'sate', 'pate', 'sigma')){
  # ensure model is a of class bartcFit
  validate_model_(.model)
  n_chains <- seq_len(.model$n.chains)
  # get type if null
  type <- type[1]
  if (type %notin% c('cate', 'sate', 'pate', 'sigma')) stop("type must be 'cate', 'sate', 'pate' or 'sigma'")

  # axis name
  y_axis <- ifelse(type == 'sigma', 'sigma', toupper(.model$estimand))

  p <- .model %>%
    bartCause::extract(type, combineChains = FALSE) %>%
    t() %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(n_chains) %>%
    mutate(Chain = factor(sub('V', '', name), levels = as.character(n_chains))) %>%
    ggplot(aes(x = iteration, y = value, color = Chain)) +
    geom_line(alpha = 0.8) +
    labs(title = 'Diagnostics: Trace plot',
         x = 'Iteration',
         y = y_axis,
         color = 'Chain')

  return(p)
}

#' @title Trace rank plot the estimands of a `bartCause::bartc()` model
#' @description Trace plots may occlude convegence issues within Markov Chains. Trace rank plots present an alternartive convergence diagnostic of MCMC convergence. Trank plots are described in detail in Vehtari et al. (2021).
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type parameter to plot options are average treatment effects: `cate`, `sate` and `pate` as well as posterior predicitve uncertainty `sigma`
#' @author George Perrett
#'
#'
#' @references Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2021). Rank-normalization, folding, and localization: An improved R ̂ for assessing convergence of MCMC (with discussion). Bayesian analysis, 16(2), 667-718.
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
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
#'  commonSup.rule = 'none'
#' )
#' plot_trank(.model = model_results)
#' }
#'

plot_trank <- function(.model, type = c('cate', 'sate', 'pate', 'sigma')){
  # ensure model is a of class bartcFit
  validate_model_(.model)
  n_chains <- seq_len(.model$n.chains)

  # get type info
  type <- type[1]
  if (type %notin% c('cate', 'sate', 'pate', 'sigma')) stop("type must be 'cate', 'sate', 'pate' or 'sigma'")
  .subtitle <- ifelse(type == 'sigma', 'sigma', toupper(.model$estimand))

  # rank data
  plt_dat <- .model %>%
    bartCause::extract(type, combineChains = FALSE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(n_chains) %>%
    mutate(Chain = factor(sub('V', '', name), levels = as.character(n_chains))) %>%
    mutate(rank = rank(value)) %>%
    mutate(cut = cut(rank, 20)) %>%
    group_by(cut) %>%
    mutate(bin_start = min(rank)) %>%
    ungroup() %>%
    group_by(Chain, bin_start) %>%
    mutate(count = n())

  # plot it
  p <- plt_dat %>%
    filter(rank == max(rank)) %>%
    mutate(bin_start = rank) %>%
    ungroup() %>%
    bind_rows(plt_dat) %>%
    ggplot() +
    aes(x = bin_start, y =  count, color = Chain) +
    geom_step() +
    labs(title = 'Diagnostics: Trace Rank',
         subtitle = .subtitle,
         x = 'Rank',
         y = NULL
    )

  return(p)
}

