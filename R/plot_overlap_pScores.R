#' Plot the overlap via propensity score method
#'
#' Plot histograms showing the overlap between propensity scores by treatment status. TODO: reference?
#'
#' @param .data dataframe
#' @param treatment name of the treatment column within .data
#' @param response name of the response column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param plot_type the plot type, one of c('Histogram', 'Density')
#' @author George Perrett, Joe Marlo
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
#' data(lalonde, package = 'arm')
#' plot_overlap_pScores(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  response = 're78',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'histogram'
#')
plot_overlap_pScores <- function(.data, treatment, response, confounders, plot_type = c("histogram", "density")) {

  plot_type <- tolower(plot_type[[1]])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (response %notin% colnames(.data)) stop('response not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical(.data[[treatment]])

  # run the Bart model
  confounders_mat <- as.matrix(.data[, 3:ncol(.data)])
  dim.red_results <- bartCause::bartc(response = .data[[response]],
                                      treatment = .data[[treatment]],
                                      confounders = as.matrix(.data[confounders]))

  # pull the propensity scores
  pscores <- dim.red_results$p.score

  # clean and combine data into new dataframe
  dat <- .data[treatment]
  colnames(dat) <- "Z"
  dat$Z <- as.logical(dat$Z)
  dat$pscores <- pscores

  if (plot_type == 'histogram'){

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(data = dat %>% filter(Z == 1),
                     aes(x = pscores, y = ..count.., fill = Z),
                     alpha = 0.8) +
      geom_histogram(data = dat %>% filter(Z == 0),
                     aes(x = pscores, y = -..count.., fill = Z),
                     alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      labs(title = "Overlap by treatment status",
           subtitle = 'Data should ideally be balanced vertically',
           x = NULL,
           y = 'Count',
           fill = "Treatment")

    }

  if (plot_type == 'density') {

      p <- ggplot() +
        geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
        geom_density(data = dat %>% filter(Z == 1),
                       aes(x = pscores, y = ..density.., fill = Z),
                       alpha = 0.8) +
        geom_density(data = dat %>% filter(Z == 0),
                       aes(x = pscores, y = -..density.., fill = Z),
                       alpha = 0.8) +
        scale_y_continuous(labels = function(lbl) abs(lbl)) +
        scale_fill_manual(values = c('#bd332a', '#262991')) +
        labs(title = "Overlap by treatment status",
             subtitle = 'Data should ideally be balanced vertically',
             x = NULL,
             y = 'Count',
             fill = "Treatment")

    }

  return(p)
}
