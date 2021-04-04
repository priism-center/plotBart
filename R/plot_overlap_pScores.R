#' Plot the overlap via propensity score method
#'
#' Plot histograms showing the overlap between propensity scores by treatment status.
#'
#' @param .data dataframe
#' @param treatment_col name of the treatment column within .data
#' @param response_col name of the response column within .data
#' @param confounder_cols character list of column names denoting confounders within .data
#' @param plt_type the plot type, one of c('Histogram', 'Density')
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
#'  treatment_col = 'treat',
#'  response_col = 're78',
#'  confounder_cols = c('age', 'educ'),
#'  plt_type = 'Histogram'
#')
plot_overlap_pScores <- function(.data, treatment_col, response_col, confounder_cols, plt_type = c("Histogram", "Density")) {

  # coerce 01 to logical
  treat_levels <- as.character(unique(.data[[treatment_col]]))
  if (length(setdiff(treat_levels, as.character(0:1))) == 0){
    .data[[treatment_col]] <- dplyr::recode(.data[[treatment_col]], `0` = FALSE, `1` = TRUE)
  }
  if (!is.logical(.data[[treatment_col]])) stop("treatment_col must be logical")

  # run the Bart model
  confounders_mat <- as.matrix(.data[, 3:ncol(.data)])
  dim.red_results <- bartCause::bartc(response = .data[[response_col]],
                                      treatment = .data[[treatment_col]],
                                      confounders = as.matrix(.data[confounder_cols]))

  # pull the propensity scores
  pscores <- dim.red_results$p.score

  # clean and combine data into new dataframe
  dat <- .data[treatment_col]
  colnames(dat) <- "Z"
  dat$Z <- as.logical(dat$Z)
  dat$pscores <- pscores

  if (plt_type == 'Histogram'){

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

    } else if (plt_type == 'Density') {

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

    } else stop("plt_type must be 'Histogram or 'Density'")

  return(p)
}
