#' @title Plot the model residual
#' @description Visualize balance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .model a model produced by `bartCause::bartc()`
#'
#' @author George Perrett & Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stats rnorm
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
#' plot_residual_density(model_results)
#' }
plot_residual_density <- function(.model){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  # extract the covariates
  dat <- as.data.frame(.model$data.rsp@x)
  # add observed y
  dat$y.obs <- .model$data.rsp@y

  if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }


  # add predicted y
  dat$y.hat.mean <- apply(bartCause::extract(.model, "mu.obs"), 2, mean)

  # add residual
  dat$residual <- dat$y.hat.mean - dat$y.obs

  dat$reference <- rnorm(n = nrow(dat), 0, sd(dat$residual))

  dat <- dat %>%
    tidyr::pivot_longer(cols = c('residual', 'reference'))

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value, color = name)) +
    ggplot2::geom_density() +
    ggplot2::scale_color_manual(values = c(2, 1)) +
    ggplot2::labs(x = "Residual", y = "Density")

  return(p)
}

# plot residual vs predicted y
plot_residual_predicted <- function(.model, covariate = NULL){
  # check if model is bartCause
  validate_model_(.model)

  # extract the covariates
  dat <-  as.data.frame(.model$data.rsp@x)

  # add observed y
  dat$y.obs <- .model$data.rsp@y

  # filter to estimand
  if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }

  # add predicted y
  dat$y.hat.mean <- apply(bartCause::extract(.model, 'mu.obs'), 2, mean)

  # add residual
  dat$residual <- dat$y.hat.mean - dat$y.obs

  if(is.null(covariate)){
    p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) +
      geom_point()
  }else{
    # ensure the input variable is within the dataset
    index <- which(colnames(dat) == covariate)
    if (!isTRUE(index > 0)) stop('Cannot find variable in original data. Is variable within the original dataframe used to fit the .model?')

    p <- ggplot(data = dat, aes(x = !!rlang::sym(covariate), y = residual)) +
      geom_point()

    # categorical <- isTRUE(is_categorical(dat[[covariate]]))
    # binary <- isTRUE(clean_detect_logical(dat[[covariate]]))
    #
    # if(categorical | binary){ # color by a categorical or logical variable
    #   p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) +
    #     geom_point(aes(colour = factor(!!rlang::sym(covariate))))
    # }else{ # color by a continuous variable in gradient
    #   p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) +
    #     geom_point(aes(colour = !!rlang::sym(covariate)))
    # }
  }
  if(rlang::is_null(covariate)){
    p <- p + geom_hline(yintercept = 0) +
      labs(x = "Predicted Y", y = "Residual")
  }else{
    p <- p + geom_hline(yintercept = 0) +
      labs(x = covariate, y = "Residual")
  }
  return(p)
}
