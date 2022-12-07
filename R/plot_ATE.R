#' @title Plot the histogram or density of the Conditional Average Treatment Effect
#' @description Plot the conditional average treatment effect (CATE) of a 'bartCause' model.
#' The conditional average treatment effect is derived from taking the difference between
#' predictions for each individual under the control condition and under the treatment condition averaged over the population.
#' Means of the CATE distribution will resemble SATE and PATE but the CATE distribution accounts for more uncertainty than SATE and less uncertainty than PATE.
#'
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_CATE(model_results)
#' }
plot_CATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model, 'cate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, 0.9)
  lb <- quantile(pate$pate, 0.1)
  ub.95 <- quantile(pate$pate, 0.975)
  lb.95 <- quantile(pate$pate, 0.025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', ccolor = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density',
           linetype = NULL)

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}


#' @title Plot Individual Conditional Average Treatment effects
#' @description Plots a histogram of Individual Conditional Average Treatment effects (ICATE).
#' ICATEs are the difference in each individual's predicted outcome under the treatment and predicted outcome under the control averaged over the individual.
#' Plots of ICATEs are useful to identify potential heterogeneous treatment effects between different individuals. ICATE plots can be grouped by discrete variables.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param .group_by a grouping variable as a vector
#' @param n_bins number of bins
#' @param .alpha transparency of histograms
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr bartCause
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
#' plot_ICATE(model_results, lalonde$married)
#' }
plot_ICATE <- function(.model, .group_by = NULL, n_bins = 30, .alpha = .7){

  validate_model_(.model)
  if (!is.null(.group_by)) is_discrete_(.group_by)

  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))

  # adjust value based on estimand
  .group_by <- adjust_for_estimand_(.model, .group_by)

  # create base plot
  p <- ggplot(icates, aes(x = value)) +
    geom_histogram(bins = n_bins, color = 'black')

  # add grouping
  if(!is.null(.group_by)){
    p <- ggplot(data = icates,
                aes(x = value, fill = as.factor(.group_by))) +
      geom_histogram(position = 'identity', bins = n_bins, alpha = .alpha, col = 'black')
  }

  # add labels
  p <- p +
    labs(title = NULL,
         x = NULL,
         y = 'Count',
         fill = NULL)

  return(p)
}

#' @title Plot histogram or density of Population Average Treatment Effect
#' @description Plot shows the Population Average Treatment Effect which is derived from the posterior predictive distribution of the difference between \eqn{y | z=1, X} and \eqn{y | z=0, X}.
#' Mean of PATE will resemble CATE and SATE but PATE will account for more uncertainty and is recommended for informing inferences on the average treatment effect.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_PATE(model_results)
#' }
plot_PATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){
  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Sample Average Treatment Effect",
    att = "Posterior of Sample Average Treatment Effect of the Treated",
    atc = "Posterior of Sample Average Treatment Effect of the Control"
  )

  # calculate stats
  y.1 <- extract(.model, 'y.1')
  y.0 <- extract(.model, 'y.0')
  pate.samples <- t(y.1 - y.0)

  # check overlap
  pate_overlap <- apply_overlap_rules(.model)

  # get different pates if warning is activeated
  pates <- tibble(none = apply(pate.samples, 2, mean))

  if (pate_overlap$sum_sd_removed > 0 | pate_overlap$sum_chisq_removed > 0) {
    pates$sd <- apply(pate.samples[!pate_overlap$ind_sd_removed,], 2, mean)
    pates$chisq <-apply(pate.samples[!pate_overlap$ind_chisq_removed,], 2, mean)
  }


  # pivot to long form now we just have name(what type of sate) and value
  pates <- pivot_longer(pates, cols = 1:length(pates))
  # caclulate bounds for each type of sate (no overlap, sd and chisq)
  ub <- tapply(pates$value, pates$name, function(i){quantile(i, .9)})
  lb <- tapply(pates$value, pates$name, function(i){quantile(i, .1)})
  lb.95 <- tapply(pates$value, pates$name, function(i){quantile(i, .025)})
  ub.95 <- tapply(pates$value, pates$name, function(i){quantile(i, .975)})

  # calculate densities and use bind_rows() to roll into a single df for ggplot
  dd <- tapply(pates$value, pates$name, density)
  dd <-
    lapply(1:length(dd), function(i) {
      data.frame(x = dd[[i]]$x,
                 y = dd[[i]]$y,
                 name = names(dd)[i],
                 lb.95 = lb.95[[names(dd)[i]]],
                 ub.95 = ub.95[[names(dd)[i]]],
                 lb = lb[[names(dd)[i]]],
                 ub = ub[[names(dd)[i]]])
    }) %>%
    bind_rows()


  # build base plot
  p <- ggplot(pates, aes(value)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # facet if removal rules would create different results
  if(length(unique(pates$name)) > 1){
    .facet_lab <- c(
      `none` = "No overlap rule applied: 0 cases (0%) were removed",
      `sd` = paste0("Standard deviation overlap rule applied: ", pate_overlap$sum_sd_removed, ' cases (',round((pate_overlap$sum_sd_removed/nrow(pate.samples)*100), 2) ,'%) were removed'),
      `chisq` = paste0("Chi-squard overlap rule applied: ", pate_overlap$sum_chisq_removed, ' cases (',round((pate_overlap$sum_chisq_removed/nrow(pate.samples)*100), 2) ,'%) were removed')
    )
    p <- p +
      facet_wrap(~ factor(name,
                          levels = c('none', 'sd', 'chisq')),
                 ncol = 1, labeller = as_labeller(.facet_lab))
  }
  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', color = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y, group = name),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y, group = name),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pates, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pates, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}

#' @title Plot histogram or density of Sample Average Treatment Effects
#' @description Plot a histogram or density of the Sample Average Treatment Effect (SATE). The Sample Average Treatment Effect is derived from taking the difference of each individual's observed outcome and a predicted counterfactual outcome from a BART model averaged over the population.
#' The mean of SATE will resemble means of CATE and PATE but will account for the least uncertainty.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this x-axis value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#' @param check_overlap TRUE/FALSE include sensitivity analyses of overlap removal
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#' @import ggplot2 bartCause
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
#' plot_SATE(model_results)
#' }
plot_SATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE, check_overlap = F){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Sample Average Treatment Effect",
    att = "Posterior of Sample Average Treatment Effect of the Treated",
    atc = "Posterior of Sample Average Treatment Effect of the Control"
  )

  .sign <- switch (.model$estimand,
    ate = (2 * .model$trt - 1),
    att = (2 * .model$trt[.model$trt == 1] - 1),
    atc = (2 * .model$trt[.model$trt == 0] - 1)
  )

  # calculate stats
  mu_obs <- extract(.model, 'mu.obs')
  y_cf <- extract(.model, 'y.cf')
  sate.samples <- t(mu_obs - y_cf)*.sign

  # check overlap
  sate_overlap <- apply_overlap_rules(.model)

  # get different sates

  sates <- tibble(none = apply(sate.samples, 2, mean))

  if (sate_overlap$sum_sd_removed > 0 | sate_overlap$sum_chisq_removed > 0) {
    sates$sd <- apply(sate.samples[!sate_overlap$ind_sd_removed,], 2, mean)
    sates$chisq <-apply(sate.samples[!sate_overlap$ind_chisq_removed,], 2, mean)
  }

  # pivot to long form now we just have name(what type of sate) and value
  sates <- pivot_longer(sates, cols = 1:length(sates))
  # caclulate bounds for each type of sate (no overlap, sd and chisq)
  ub <- tapply(sates$value, sates$name, function(i){quantile(i, .9)})
  lb <- tapply(sates$value, sates$name, function(i){quantile(i, .1)})
  lb.95 <- tapply(sates$value, sates$name, function(i){quantile(i, .025)})
  ub.95 <- tapply(sates$value, sates$name, function(i){quantile(i, .975)})

  # for ci of histogrms
  if(type == 'histogram' & (isTRUE(ci_95)| isTRUE(ci_80))){
    sates <- sates %>%
      group_by(name) %>%
      mutate(ub = quantile(value, .9),
             lb = quantile(value, .1),
             ub.95 = quantile(value, .975),
             lb.95 = quantile(value, .025)) %>%
      ungroup()
  }

  # calculate densities and use bind_rows() to roll into a single df for ggplot
  dd <- tapply(sates$value, sates$name, density)
  dd <-
    lapply(1:length(dd), function(i) {
      data.frame(x = dd[[i]]$x,
                 y = dd[[i]]$y,
                 name = names(dd)[i],
                 lb.95 = lb.95[[names(dd)[i]]],
                 ub.95 = ub.95[[names(dd)[i]]],
                 lb = lb[[names(dd)[i]]],
                 ub = ub[[names(dd)[i]]])
    }) %>%
    bind_rows()


  # build base plot
  p <- ggplot(sates, aes(value)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # facet if removal rules would create different results
  if(length(unique(sates$name)) > 1 & isTRUE(check_overlap)){
    .facet_lab <- c(
      `none` = "No overlap rule applied: 0 cases (0%) were removed",
      `sd` = paste0("Standard deviation overlap rule applied: ", sate_overlap$sum_sd_removed, ' cases (',round((sate_overlap$sum_sd_removed/nrow(sate.samples)*100), 2) ,'%) were removed'),
      `chisq` = paste0("Chi-squard overlap rule applied: ", sate_overlap$sum_chisq_removed, ' cases (',round((sate_overlap$sum_chisq_removed/nrow(sate.samples)*100), 2) ,'%) were removed')
    )
    p <- p +
      facet_wrap(~ factor(name,
                          levels = c('none', 'sd', 'chisq')),
                 ncol = 1, labeller = as_labeller(.facet_lab))
  }

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', color = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_80)) p <- p + geom_segment(aes(x = lb, xend = ub, y = 0, yend = 0), size = 3, color = 'grey10')
    if (isTRUE(ci_95)) p <- p + geom_segment(aes(x = lb.95, xend = ub.95, y = 0, yend = 0), size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y, group = name),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y, group = name),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }
  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = sates, aes(xintercept = mean(sate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = sates, aes(xintercept = median(sate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}
