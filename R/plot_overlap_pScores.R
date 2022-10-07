#' @title Plot the overlap via propensity score method
#' @description Plot histograms showing the overlap between propensity scores by treatment status.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param min_x numeric value specifying the minimum propensity score value to be shown on the x axis
#' @param max_x numeric value specifying the maximum propensity score value to be shown on the x axis
#' @param plot_type the plot type, one of c('Histogram', 'Density')
#' @param pscores propensity scores. If not provided, then propensity scores will be calculated using BART
#' @param \dots additional arguments passed to `dbarts::bart2` propensity score calculation
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{plot_overlap_vars}}
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' plot_overlap_pScores(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'histogram',
#'  pscores = NULL,
#'  seed = 44
#')
#'}
plot_overlap_pScores <- function(.data, treatment, confounders, plot_type = c("histogram", "density"),min_x = NULL, max_x = NULL, pscores = NULL, ...) {

  plot_type <- tolower(plot_type[[1]])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (!is.null(pscores) & !inherits(pscores, 'numeric')) stop('propensity_scores must be a numeric vector')

  # calculate propensity scores from bart model
  if (is.null(pscores)){
    pscores <- propensity_scores(
      .data = .data,
      treatment = treatment,
      confounders = confounders,
      ...
    )
  }

  dat <- data.frame(Z = coerce_to_logical_(.data[[treatment]]),
                    pscores = pscores)

  if(!is.null(min_x)) {dat <- dat %>% filter(pscores >= min_x)}
  if(!is.null(max_x)) {dat <- dat %>% filter(pscores <= max_x)}


  if (plot_type == 'histogram'){

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(data = filter(dat, Z == 1),
                     aes(x = pscores, y = ..count.., fill = Z),
                     alpha = 0.8) +
      geom_histogram(data = filter(dat, Z == 0),
                     aes(x = pscores, y = -..count.., fill = Z),
                     alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4,2)) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = "Treatment")

    }

  if (plot_type == 'density') {

      p <- ggplot() +
        geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
        geom_density(data = filter(dat, Z == 1),
                       aes(x = pscores, y = ..density.., fill = Z),
                       alpha = 0.8) +
        geom_density(data = filter(dat, Z == 0),
                       aes(x = pscores, y = -..density.., fill = Z),
                       alpha = 0.8) +
        scale_y_continuous(labels = function(lbl) abs(lbl)) +
        scale_fill_manual(values = c(4,2)) +
        labs(title = "Overlap by treatment status",
             x = NULL,
             y = 'Count',
             fill = "Treatment")

    }

  return(p)
}

#' @title Calculate propensity scores using BART
#' @description Calculates propensity scores using Bayesian Additive Regression Trees via `bartCause::bartc()`.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param \dots additional arguments passed to `dbarts::barts()`
#'
#' @return a numeric vector of propensity scores
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dbarts bart2
#'
#' @seealso \code{\link{plot_overlap_pScores}}
propensity_scores <- function(.data, treatment, confounders, ...){

  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  # run the Bart model
  confounders_mat <- as.matrix(.data[, 3:ncol(.data)])
  dim.red_results <- dbarts::bart2(.data[[treatment]] ~ as.matrix(.data[confounders]))
  p.score <- apply(dbarts::extract(dim.red_results, 'ev'), 2, mean)

  return(p.score)
}
