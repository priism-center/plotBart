#' Variable importance of Bayesian Additive Regression Trees
#'
#' Fit single regression tree on bartc() icates to produce variable importance plot & table.
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#' @param confounders a character list of column names which should be considered the confounders. Must match the column names used to original fit .model.
#' @param out type of output. One of c('all', 'plot', 'table').
#' @author George Perrett, Joe Marlo
#'
#' @return a list containing variable importance plot & ordered table of confounders by scaled importance
#' @export
#'
#' @import ggplot2 dplyr bartCause
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
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
#' plot_variable_importance(model_results,  c('age', 'educ'))
plot_variable_importance <- function(.model, confounders, out = c('all', 'plot', 'table')){

  if (!methods::is(.model, "bartcFit")) stop(".model must be of class bartcFit")

  out <- out[1]

  # extract individual conditional effects
  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)
  icate.sd <- apply(icate, 2, sd)

  # pull data from model
  .data <- as.data.frame(.model$data.rsp@x)
  if (!all(confounders %in% colnames(.data))) stop("confounders must be within the original data used to fit .model")
  confounders <- as.matrix(.data[, confounders])

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ confounders)
  # TODO: theres an issue here with some variables getting dropped

  # save variable importance
  importance <- cart$variable.importance / sum(cart$variable.importance)*100
  names(importance) <- sub("confounders", "", names(importance))

  # enframe and clean data
  importance_table <- importance %>%
    as_tibble() %>%
    mutate(Variable = names(importance)) %>%
    dplyr::select(Variable, value) %>%
    rename(Importance = value) %>%
    arrange(desc(Importance))

  # plot variable importance
  importance_table_plt <- importance_table[1:20,]
  p1 <- ggplot(importance_table_plt,
               aes(x = Importance, y = stats::reorder(Variable, Importance))) +
    geom_segment(aes(xend = 0, yend = Variable))  +
    geom_point(size = 4) +
    labs(title = 'Potential Moderators',
         x = 'Importance',
         y = 'Variable')

  if (out == 'all'){
    results <- list(p1, importance_table)
  } else if (out == 'plot'){
    results <- p1
  } else if(out == 'table'){
    results <- importance_table
  }

  return(results)
}
