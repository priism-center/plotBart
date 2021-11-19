#' Plot the icates conditional on a continuous moderator
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param moderator the moderator as a vector
#' @param n_bins number of bins
#' @param legend legend position. One of c('none', 'right', 'top', 'bottom')
#'
#' @author George Perrett
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
plot_moderator_c_pd <- function(.model, moderator, n_bins = 15, legend = c('none', 'right', 'top', 'bottom')){

  validate_model(.model)
  if (n_bins < 1) stop("n_bins must be a number greater than 1")

  # TODO: error handling

  # extract data
  new_data <- as_tibble(.model$data.rsp@x.test)
  new_data_z1 <- new_data
  new_data_z1$treat <- 1
  new_data_z1 <- cbind(.model$fit.rsp$y, new_data_z1)
  names(new_data_z1)[1] <- as.character(.model$call$response) # TODO: this seems wrong

  new_data_z0 <- new_data
  new_data_z0$treat <- 0
  new_data_z0 <- cbind(.model$fit.rsp$y, new_data_z0)
  names(new_data_z0)[1] <- as.character(.model$call$response) # TODO: this seems wrong

  # locate the moderator in bartc data
  search <- function(x) sum(moderator - x)
  index <- which(sapply(new_data_z0, search) == 0)

  # get range for predictions
  cut <- n_bins-1
  p <- seq(min(moderator), max(moderator), (max(moderator) - min(moderator))/cut)
  if (length(p) < length(unique(moderator))) {
    range <- p
  } else{
    range <- unique(moderator)[order(unique(moderator))]
  }

  fit_pd <- function(x){
    new_data_z1[, index] <- x
    new_data_z0[, index] <- x
    preds.1 <- predict(.model, newdata = new_data_z1)
    preds.0 <- predict(.model, newdata = new_data_z0)
    preds <- preds.1 - preds.0

    cate <- apply(preds, 1, mean)
    return(cate)
  }

  # TODO: this is slow AF
  # cates <- purrr::map(range, fit_pd)
  cates <- lapply(range, fit_pd)
  cates <- bind_cols(cates)
  cates.m <- apply(cates, 2, mean)
  cates.m <- as_tibble(cbind(cates.m, range))
  ci <- function(x) quantile(x, probs = c(.025, .1, .9, .975))
  cates.ci <- as_tibble(t(apply(cates, 2, ci)))
  cates_plot <- cbind(cates.m, cates.ci)
  names(cates_plot)[3:6] <- paste0('ci_', names(cates_plot)[3:6])
  names(cates_plot)[3:6] <- substr(names(cates_plot)[3:6], 1, nchar(names(cates_plot)[3:6]) - 1)

  # plot it
  p <- ggplot(cates_plot) +
    geom_ribbon(aes(x = range, y = cates.m, ymin = ci_2.5, ymax = ci_97.5, fill = '95% ci')) +
    geom_ribbon(aes(x = range, y = cates.m, ymin = ci_10, ymax = ci_90, fill = '80% ci')) +
    scale_fill_manual(values = c('grey40', 'grey60')) +
    geom_point(aes(range, cates.m),size = 2) +
    geom_line(aes(range, cates.m)) +
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
#' @param line.color the color of the loess line
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
#' plot_moderator_c_loess(model_results, lalonde$married)
plot_moderator_c_loess <- function(.model, moderator, line.color = 'blue'){

  validate_model(.model)
  is_numeric_vector(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_moderator_for_estimand(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, rowMeans)

  # unlist into a data.frame for ploting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- moderator[order(moderator)]
  rownames(dat) <- 1:nrow(dat)

  # plot it
  p <- ggplot(dat, aes(moderator, value)) +
    geom_point() +
    geom_smooth(method = 'loess', se = FALSE,
                size = 1.5, color = line.color) +
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
#' @param facet create panel plots of each moderator level?
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

  validate_model(.model)

  # TODO
  # is_discrete(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_moderator_for_estimand(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- 1:nrow(dat)

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
#' plot_moderator_d_linerange(model_results, lalonde$educ)
plot_moderator_d_linerange <- function(.model, moderator, .alpha = 0.7, horizontal = FALSE){

  validate_model(.model)

  # TODO
  # is_discrete(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_moderator_for_estimand(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <-  posterior  %>%
    t() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- 1:nrow(dat)

  # tidy up the data
  dat <- dat %>%
    group_by(moderator) %>%
    mutate(.min = min(value),
           .max = max(value),
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
plot_moderator_search <- function(.model, depth = 2, type = c(2, 0, 1, 3, 4, 5), extra = list(1, 'all', 0)){

  validate_model(.model)

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
    .data <- .data[.data$treat == 1,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else{
    .data <- .data[.data$treat == 0,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  }

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = depth)
  p <- rpart.plot(cart, type = .type, extra = .extra, branch = 1, box.palette = 0)

  return(p)
}
