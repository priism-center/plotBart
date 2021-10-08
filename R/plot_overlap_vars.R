#' Plot the overlap of variables
#'
#' Plot histograms showing the overlap between variables by treatment status.
#'
#' @param .data dataframe
#' @param treatment_col name of the treatment column within .data
#' @param confounder_cols character list of column names denoting confounders within .data
#' @param plt_type the plot type, one of c('histogram', 'density'). Defaults to 'histogram'
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{plot_overlap_pScores}}
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' data(lalonde, package = 'arm')
#' plot_overlap_vars(
#'  .data = lalonde,
#'  treatment_col = 'treat',
#'  confounder_cols = c('age', 'educ'),
#'  plt_type = 'Histogram'
#')
plot_overlap_vars <- function(.data, treatment_col, confounder_cols, plt_type = c("histogram", "density")){

  plt_type <- tolower(plt_type[1])
  if (treatment_col %notin% colnames(.data)) stop('treatment_col not found in .data')

  # coerce treatment column to logical
  .data[[treatment_col]] <- coerce_to_logical(.data[[treatment_col]])


  .data <- .data[, c(treatment_col, confounder_cols)]
  colnames(.data) <- c("Z_treat", confounder_cols)

  # pivot the data
  dat_pivoted <- pivot_longer(.data, cols = -Z_treat)

  if (plt_type == 'histogram'){

    # histograms showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(data = filter(dat_pivoted, Z_treat == 1),
                     aes(x = value, y = ..count.., fill = Z_treat),
                     alpha = 0.8)+
      geom_histogram(data = filter(dat_pivoted, Z_treat == 0),
                     aes(x = value, y = -..count.., fill = Z_treat),
                     alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           subtitle = 'Informative subtitle to go here',
           x = NULL,
           y = 'Count',
           fill = "Treatment")

  } else if (plt_type == 'density') {

    # density plots showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_density(data = filter(dat_pivoted, Z_treat == 1),
                   aes(x = value, y = ..density.., fill = Z_treat),
                   alpha = 0.8)+
      geom_density(data = filter(dat_pivoted, Z_treat == 0),
                   aes(x = value, y = -..density.., fill = Z_treat),
                   alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           subtitle = 'Informative subtitle to go here',
           x = NULL,
           y = 'Density',
           fill = "Treatment")

  } else stop("plt_type must be 'Histogram or 'Density'")

  return(p)
}
