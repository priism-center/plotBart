#' @title Plot the balance
#' @description Visualize balance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param compare character of either means or variance denotes what to compare balance on
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about
#' @author Joseph Marlo & George Perrett
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
plot_balance <- function(.data, treatment, confounders, compare = c('means', 'variance'), estimand = c('ATE', 'ATT', 'ATC')){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  compare <- match.arg(compare)
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (compare %notin% c('means', 'variance')) stop("compare must be either: means or variance")
  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")

  p <- .data %>%
    dplyr::select(all_of(c(confounders, treatment))) %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treat, values_from = c(variance, mean)) %>%
    mutate(means =
             case_when(
               estimand == 'ATE' ~ (mean_1 - mean_0) / sqrt((variance_1 + variance_0) /2),
               estimand == 'ATT' ~ (mean_1 - mean_0) / sqrt(variance_1),
               estimand == 'ATC' ~ (mean_1 - mean_0) / sqrt(variance_0)
             ),
           variance = sqrt(variance_1/variance_0)
           ) %>%
    ggplot(aes(x = get(compare), y = name)) +
    geom_vline(xintercept = ifelse(compare == 'means', 0, 1), linetype = 'dashed', color = 'gray60') +
    geom_point(size = 4) +
    labs(title = 'Balance',
         subtitle = 'points represent the treatment group',
         x = ifelse(compare == 'means', 'Scaled mean difference', 'Ratio of variance'),
         y = NULL,
         color = NULL) +
    theme(legend.position = 'none')

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
#' print_balance(lalonde, 'treat', c('re78', 'age', 'educ'), estimand = 'ATE')

print_balance <- function(.data, treatment, confounders, estimand = 'ATE'){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
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
    pivot_wider(names_from = treat, values_from = c(variance, mean)) %>%
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


