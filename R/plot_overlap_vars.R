#' @title Plot the overlap of variables
#' @description Plot histograms showing the overlap between variables by treatment status.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param min_x numeric value specifying the minimum value to be shown on the x axis
#' @param max_x numeric value specifying the maximum value to be shown on the x axis
#' @param plot_type the plot type, one of c('histogram', 'density'). Defaults to 'histogram'
#' @author George Perrett, Joseph Marlo
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
#' data(lalonde)
#' plot_overlap_vars(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'Histogram'
#')
plot_overlap_vars <- function(.data, treatment, confounders, plot_type = c("histogram", "density"), min_x = NULL, max_x = NULL){

  plot_type <- tolower(plot_type[1])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  # extract the relevant columns and rename treatment column
  .data <- .data[, c(treatment, confounders)]
  colnames(.data) <- c("Z_treat", confounders)

  if(!is.null(min_x)) {.data <- .data[.data[[confounders]] >= min_x, ]}
  if(!is.null(max_x)) {.data <- .data[.data[[confounders]] <= max_x, ]}

  # pivot the data
  dat_pivoted <- pivot_longer(.data, cols = -Z_treat)

  if (plot_type == 'histogram'){
    # histograms showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60')

    if(is.numeric(dat_pivoted$value)){
      p <- p + geom_histogram(data = filter(dat_pivoted, Z_treat == 1),
                       aes(x = value, y = after_stat(count), fill = Z_treat),
                       alpha = 0.8, color = 'black') +
        geom_histogram(data = filter(dat_pivoted, Z_treat == 0),
                       aes(x = value, y = after_stat(count), fill = Z_treat),
                       alpha = 0.8, color = 'black')
    }else{
      p <- p + geom_bar(data = filter(dat_pivoted, Z_treat == 1),
                              aes(x = value, y = after_stat(count), fill = Z_treat),
                              alpha = 0.8, color = 'black') +
        geom_bar(data = filter(dat_pivoted, Z_treat == 0),
                       aes(x = value, y = -after_stat(count), fill = Z_treat),
                       alpha = 0.8, color = 'black')
    }
     p <- p +  scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4, 2)) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = 'Treatment')

  }

  if (plot_type == 'density') {
    if (is.character(dat_pivoted$value)) stop('Density plots are unavalable for character variables')

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
      scale_fill_manual(values = c(4, 2)) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Density',
           fill = "Treatment")

  }

  return(p)
}
