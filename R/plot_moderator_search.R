#' Single Regression Tree for exploratory heterogenious effects
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @param confounders matrix of confounders
#' @author George Perrett, Joe Marlo
#' @return a ggplot object of a single regression tree
#' @export 

plot_moderator_search <- function(.model, depth = 2, type = c(2,0,1, 3, 4, 5), extra = list(1, 'all', 0)){
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
  if (!is.matrix(confounders)) stop("confounders must be of class matrix")
  
  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)
  .type <- type[1]
  .extra <- extra[[1]]

  # pull data from model and create a matrix of confounders 
  .data <- as.data.frame(.model$data.rsp@x)
  
  # adjust data for estimnd
  if(.model$estimand == 'ate'){
    confounders <- as.matrix(.data[, c(-1, -(length(.data)))])
  }
  else if(.model$estimand == 'att'){
    .data <- .data[.data$treat ==1,]
    confounders <- as.matrix(.data[, c(-1, -(length(.data)))])
  }
  
  else{
    .data <- .data[.data$treat ==0,]
    confounders <- as.matrix(.data[, c(-1, -(length(.data)))])
  }

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = depth)
  
  p <- rpart.plot(cart, type = .type, extra = .extra, branch = 1, box.palette = 0)
  return(p)
}

