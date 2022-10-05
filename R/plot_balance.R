#' @title Plot the balance
#' @description Visualize balance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param compare character of either means or variance denotes what to compare balance on
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about

#' @author George Perrett & Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @examples
#' data(lalonde)

#' plot_balance(lalonde, 'treat', c('re78', 'age', 'educ'), compare = 'means', estimand = 'ATE') + labs(title = 'My new title')
plot_balance <- function(.data, treatment, confounders, compare = c('means', 'variance', 'covariance'), estimand = c('ATE', 'ATT', 'ATC')){
  if(missing(treatment)) stop('enter a string indicating the name of the treatment variable')
  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  if(is.logical(.data[[treatment]])) .data[[treatment]] <- as.numeric(.data[[treatment]])
  compare <- match.arg(compare)
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")
  if(compare == 'variance')x_var <- 'variance' else x_var <- 'means'

  if(compare == 'covariance'){
    # gets interactions of all columns
    cov_dat <- combn(.data[, confounders], 2, FUN = Reduce, f = `*`)

    # get column names so we know what is what
    colnames(cov_dat) <- paste(
      combn(names(.data[, confounders]), 2)[1, ],
      combn(names(.data[, confounders]), 2)[2, ],
      sep = '*')
    # add back treatment
    .data <- cbind.data.frame(cov_dat, treatment = .data[[treatment]])

  }else{
    .data <- .data %>%
      dplyr::select(all_of(c(confounders, treatment)))
  }

   .data <- .data %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    mutate(means =
             case_when(
               estimand == 'ATE' ~ (mean_1 - mean_0) / sqrt((variance_1 + variance_0) /2),
               estimand == 'ATT' ~ (mean_1 - mean_0) / sqrt(variance_1),
               estimand == 'ATC' ~ (mean_1 - mean_0) / sqrt(variance_0)
             ),
           variance = sqrt(variance_1/variance_0)
           ) %>%
    mutate(flag_means = if_else(means > 2.5 | means < -2.5, 1, 0),
           flag_variance = if_else(variance > 2 | variance < .5, 1, 0),
           flag = if(compare == 'variance') flag_variance else  flag_means,
           means = if_else(means > 2.5, 2.5, means),
           means = if_else(means < -2.5, -2.5, means),
           variance = if_else(variance > 1, 1, variance),
           variance = if_else(variance < .5, .5, variance))

    p <- ggplot(.data, aes(
      x = get(x_var),
      y = reorder(name, abs(get(x_var))),
      col = as.factor(flag)
    )) +
    geom_vline(
      xintercept = ifelse(x_var == 'means', 0, 1),
      linetype = 'dashed',
      color = 'gray60'
    ) +
    geom_point(size = 4) +
    scale_color_manual(values = c('black', 'red')) +
    labs(
      title = 'Balance',
      x = case_when(
        compare == 'means' ~ 'Scaled mean difference',
        compare == 'variance' ~ 'Ratio of variance',
        compare == 'covariance' ~ 'Scaled mean difference of interactions (balance of covaraince)'
      ),
      y = NULL,
      color = NULL
    ) +
    theme(legend.position = 'none')

  if (x_var == 'means')
    p <-p + coord_cartesian(xlim = c(-2.5, 2.5))
    else
      p <- p + coord_cartesian(xlim = c(.5, 2)) + scale_x_continuous(trans='log10')

  if(max(.data$flag) == 0)
    p <- p + labs(subtitle = 'points represent the treatment group')
    else
      p <- p + labs(subtitle = if_else(compare == 'variance',
                                       'points represent the treatment group\nred points have a variance ratio that is more than 2 times bigger or smaller than the control group',
                                       'points represent the treatment group\nred points have standardized means that are more than 2.5 standard deviations different than the control group'))

  return(p)

}

#' @title Print balance statistics
#' @description See balance statisitics of variables between treatment and control groups.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about
#' @author George Perrett
#'
#' @return tibble
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stats var
#'
#' @examples
#' data(lalonde)
#' print_balance(lalonde, 'treat', confounders = c('re78', 'age', 'educ'), estimand = 'ATE')

print_balance <- function(.data, treatment, confounders, estimand = 'ATE'){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  if(is.logical(.data[[treatment]])) .data[[treatment]] <- as.numeric(.data[[treatment]])
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")

  classes <- sapply(.data[, confounders], class)

  table <- .data %>%
    dplyr::select(all_of(c(confounders, treatment))) %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    mutate(
      raw_means = mean_1 - mean_0,
      means =
        case_when(
          estimand == 'ATE' ~ (mean_1 - mean_0) / sqrt((variance_1 + variance_0) /
                                                         2),
          estimand == 'ATT' ~ (mean_1 - mean_0) / sqrt(variance_1),
          estimand == 'ATC' ~ (mean_1 - mean_0) / sqrt(variance_0)
        ),
           variance = sqrt(variance_1/variance_0)
    ) %>%
    dplyr::select(name, raw_means, means, variance) %>%
    rename(variable = name,`difference in means` = raw_means, `standardized difference in means` = means, `ratio of the variance` = variance) %>%
    mutate(across(where(is.numeric), round, 2))
    table$`ratio of the variance` <- as.character(table$`ratio of the variance`)
    table[classes != 'numeric', 4] <- '--'

    return(table)

}


