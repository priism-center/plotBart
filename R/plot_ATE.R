#' Plot histogram or density of individual treatment effects
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_CATE(model_results)
plot_CATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  # to satisfy CMD CHECK
  x <- y <- NULL

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
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
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
      geom_histogram(fill = 'grey60') +
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


#' Plot icates
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param group.by a grouping variable as a vector
#' @param nbins number of bins #TODO describe
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
plot_ICATE <- function(.model, group.by = NULL, nbins = 30, .alpha = .7){

  # to satisfy CMD CHECK
  value <- NULL

  validate_model_(.model)
  # TODO: check grouping var is valid

  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))

  # adjust value based on estimand
  group.by <- adjust_for_estimand_(.model, group.by)

  # create base plot
  p <- ggplot(icates, aes(x = value)) +
    geom_histogram(bins = nbins)

  # add grouping
  if(!is.null(group.by)){
    p <- ggplot(icates,
                aes(x = value, fill = as.factor(group.by))) +
      geom_histogram(position = 'identity', bins = nbins, alpha = .alpha)
  }

  # add labels
  p <- p +
    labs(title = NULL,
         x = NULL,
         y = 'count')

  return(p)
}

#' Plot histogram or density of individual treatment effects
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_PATE(model_results)
plot_PATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  # to satisfy CMD CHECK
  x <- y <- NULL

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
  pate <- bartCause::extract(.model)
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
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
      geom_histogram(fill = 'grey60') +
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

#' Plot histogram or density of individual treatment effects
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
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
#' plot_SATE(model_results)
plot_SATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  # to satisfy CMD CHECK
  x <- y <- NULL

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
  pate <- bartCause::extract(.model, 'sate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
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
      geom_histogram(fill = 'grey60') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1.5, color = 'grey25')
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
