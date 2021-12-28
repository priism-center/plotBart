#' Plot the icates conditional on a continuous moderator
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param moderator the moderator as a vector
#' @param n_bins number of bins to cut the moderator with. Defaults to the lesser of 15 and number of distinct levels of the moderator
#' @param legend legend position. One of c('none', 'right', 'top', 'bottom')
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom stats density median na.omit predict quantile sd
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none',
#'  keepTrees = TRUE
#' )
#' plot_moderator_c_pd(model_results, lalonde$age)
plot_moderator_c_pd <- function(.model, moderator, n_bins = NULL, legend = c('none', 'right', 'top', 'bottom')){

  # to satisfy CMD CHECK
  ci_2.5 <- ci_97.5 <- ci_10 <- ci_90 <- NULL

  validate_model_(.model)

  # validate n_bins argument
  n_mod_levels <- n_distinct(moderator)
  if (n_mod_levels <= 1) stop('dplyr::n_distinct(moderator) must be at least 2')
  if (is.null(n_bins)) n_bins <- pclamp_(15, 2, n_mod_levels)
  if (!(n_bins > 1 & n_bins <= n_mod_levels)) stop("n_bins must be greater than 1 and less than or equal to dplyr::n_distinct(moderator)")

  legend <- legend[1]

  # extract data from model
  new_data <- as_tibble(.model$data.rsp@x)

  # create dataframe where all observations are treated
  new_data_z1 <- new_data
  name_trt <- .model$name.trt
  new_data_z1[, name_trt] <- 1
  new_data_z1 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z1)

  # create dataframe where all observations are control
  new_data_z0 <- new_data
  new_data_z0[, name_trt] <- 0
  new_data_z0 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z0)

  # locate the moderator in bartc data
  search_moderator <- function(x) sum(moderator - x)
  index <- which(lapply(new_data_z0, search_moderator) == 0)
  if (!isTRUE(index > 0)) stop('Cannot find moderator in original data. Is moderator within the original dataframe used to fit the .model?')

  # get range for predictions
  cut <- n_bins-1
  p <- seq(min(moderator), max(moderator), (max(moderator) - min(moderator))/cut)
  if (length(p) < n_distinct(moderator)) {
    .range <- p
  } else{
    .range <- unique(moderator)[order(unique(moderator))]
  }

  # predict new data with overridden treatment columns
  # TODO: this is slow AF
  cates <- lapply(.range, fit_pd_, z1 = new_data_z1, z0 = new_data_z0, index = index, .model = .model)
  names(cates) <- seq_along(cates)
  cates <- bind_cols(cates)
  cates.m <- apply(cates, MARGIN = 2, FUN = mean)
  cates.m <- bind_cols(cates.m = cates.m, .range = .range)

  # get credible intervals
  ci_range <- c(0.025, 0.1, 0.9, 0.975)
  cates.ci <- as_tibble(t(apply(cates, MARGIN = 2, FUN = ci_, probs = ci_range)))
  cates_plot <- bind_cols(cates.m, cates.ci)
  indices <- 2 + seq_along(ci_range)
  colnames(cates_plot)[indices] <- paste0('ci_', as.character(ci_range * 100))

  # plot it
  p <- ggplot(cates_plot) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_2.5, ymax = ci_97.5, fill = '95% ci')) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_10, ymax = ci_90, fill = '80% ci')) +
    scale_fill_manual(values = c('grey40', 'grey60')) +
    geom_point(aes(x = .range, y = cates.m), size = 2) +
    geom_line(aes(x = .range, y = cates.m)) +
    labs(title = NULL,
         x = NULL,
         y = 'CATE') +
    theme(legend.position = legend)

  return(p)
}


#' Plot the icates conditional on a continuous moderator
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param moderator the moderator as a vector
#' @param line_color the color of the loess line
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_c_loess(model_results, lalonde$age)
plot_moderator_c_loess <- function(.model, moderator, line_color = 'blue'){

  # to satisfy CMD CHECK
  value <- NULL

  validate_model_(.model)
  is_numeric_vector_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, rowMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- moderator[order(moderator)]
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(moderator, value)) +
    geom_point() +
    geom_smooth(method = 'loess',
                formula = y ~ x,
                se = TRUE,
                size = 1.5,
                color = line_color) +
    labs(title = NULL,
         x = NULL,
         y = 'icate')

  return(p)
}

#' Plot the icates conditional on a discrete moderator
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param facet TRUE/FALSE. Create panel plots of each moderator level?
#' @param .ncol number of columns to use when faceting
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d_density(model_results, lalonde$educ)
plot_moderator_d_density <- function(.model, moderator, .alpha = 0.7, facet = FALSE, .ncol = 1){

  # to satisfy CMD CHECK
  value <- NULL

  validate_model_(.model)

  # TODO
  # is_discrete(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(value, fill = moderator)) +
    geom_density(alpha = .alpha) +
    labs(title = NULL,
         x = 'CATE',
         y = NULL) +
    theme(legend.position = 'bottom')

  # add faceting
  if(isTRUE(facet)){
    p <- p + facet_wrap(~moderator, ncol = .ncol)
  }

  return(p)
}


#' Plot the icates conditional on a discrete moderator
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param horizontal flip the plot horizontal?
#'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d_linerange(model_results, lalonde$educ)
plot_moderator_d_linerange <- function(.model, moderator, .alpha = 0.7, horizontal = FALSE){

  # to satisfy CMD CHECK
  value <- point <- .min <- .max <- NULL

  validate_model_(.model)

  # TODO
  # is_discrete(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior  %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # tidy up the data
  dat <- dat %>%
    group_by(moderator) %>%
    mutate(.min = quantile(value, .025),
           .max = quantile(value, .975),
           point = mean(value)) %>%
    dplyr::select(-value) %>%
    arrange(desc(point)) %>%
    ungroup() %>%
    distinct()

  # plot it
  p <- ggplot(dat, aes(x = moderator, y = point, color = moderator)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = .min, ymax = .max), alpha = .alpha) +
    labs(title = NULL,
         x = element_blank(),
         y = 'CATE') +
    theme(legend.position = 'bottom')

  if (horizontal) p <- p + coord_flip()

  return(p)
}

#' Plot a single regression tree for exploratory heterogeneous effects
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @param depth number of node levels within the tree. 2 is recommended
#' @param type type of plot to draw. See \link[rpart.plot]{rpart.plot} for details
#' @param extra extra information to display. See \link[rpart.plot]{rpart.plot} for details
#'
#' @author George Perrett, Joe Marlo
#'
#' @importFrom rpart rpart
#' @importFrom rpart.plot rpart.plot
#'
#' @return rpart plot
#' @export
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_search(model_results)
plot_moderator_search <- function(.model, depth = 2, type = c(2, 0, 1, 3, 4, 5), extra = list(1, 'all', 0)){

  #TODO: do we really need the type and extra parameters?

  validate_model_(.model)

  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)
  .type <- type[1]
  .extra <- extra[[1]]

  # pull data from model and create a matrix of confounders
  .data <- as.data.frame(.model$data.rsp@x)

  # adjust data for estimand
  if (.model$estimand == 'ate') {
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else if (.model$estimand == 'att') {
    .data <- .data[.model$trt == 1,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else{
    .data <- .data[.model$trt == 0,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  }

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = depth)
  p <- rpart.plot(cart, type = .type, extra = .extra, branch = 1, box.palette = 0)

  return(p)
}
