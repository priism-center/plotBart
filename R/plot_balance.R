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
#' @import ggplot2 dplyr patchwork
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn
#' @importFrom stats reorder
#' @examples
#' data(lalonde)

#' plot_balance(lalonde, 'treat', c('re78', 'age', 'educ'),
#' compare = 'means', estimand = 'ATE') +
#' labs(title = 'My new title')
#'

plot_balance <- function(.data, treatment, confounders, compare = c('means', 'variance', 'covariance'), estimand = c('ATE', 'ATT', 'ATC')){
  if(missing(treatment)) stop('enter a string indicating the name of the treatment variable')
  if('factor' %in% sapply(.data[, confounders], class)) stop('factor variables must be converted to numeric or logical indicator variables')
  if('character' %in% sapply(.data[, confounders], class)) stop('factor variables must be converted to numeric or logical indicator variables')

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

    # make sure arguments are set
  compare <- match.arg(compare)
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")
  if(compare == 'variance')x_var <- 'variance' else x_var <- 'means'

  # prep the df
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
      dplyr::select(all_of(c(confounders, treatment))) %>%
      rename(`treatment` = treatment)

  }

  data_types <- apply(.data, 2, function(i) identical(unique(i)[order(unique(i))], c(0, 1))) %>%
    as.data.frame()
  data_types$name<- rownames(data_types)
  names(data_types)[1] <- 'type'

  data_types <- data_types %>%
    mutate(type = if_else(type, 'binary/categorical', 'continuous')) %>%
    arrange(name) %>%
    filter(name != 'treatment')

    # calculate and plot it
   .data <- .data %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(
      variance = var(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    arrange(name)

    .data <- cbind.data.frame(.data, type = data_types$type)

   .data <- .data %>%
    mutate(means =
             case_when(
               estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
               estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
               estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
             ),
           variance = sqrt(variance_TRUE/variance_FALSE),
           means = if_else(type != 'continuous', mean_TRUE - mean_FALSE, means)
           )

   # calculate the order before rounding
   if(compare == 'variance'){
     .data <- .data %>%
       mutate(order = if_else(variance < 1, 1/variance, variance))
   }else{
     .data <- .data %>%
       mutate(order = abs(means))
   }

   .data <- .data %>%
    mutate(flag_means = if_else(means > 2 | means < -2, 1, 0),
           flag_variance = if_else(variance > 4 | variance < .25, 1, 0),
           flag = if(compare == 'variance') flag_variance else  flag_means,
           means = if_else(means > 2, 2, means),
           means = if_else(means < -2, -2, means),
           variance = if_else(variance > 4, 4, variance),
           variance = if_else(variance < .25, .25, variance)) %>%
     na.omit()

   p1 <- .data %>%
     filter(type != 'continuous') %>%
     ggplot(aes(
       x = get(x_var),
       y = reorder(name, order))) +
     geom_vline(
       xintercept = ifelse(x_var == 'means', 0, 1),
       linetype = 'dashed',
       color = 'gray60'
     ) +
     geom_point(size = 4) +
     labs(
       x = case_when(
         compare == 'means' ~ 'Mean difference (Proportion)',
         compare == 'variance' ~ 'Ratio of variance (log scale)',
         compare == 'covariance' ~ 'Scaled mean difference of interactions (balance of covaraince)'
       ),
       y = NULL,
       color = NULL
     ) +
     coord_cartesian(xlim = c(-1, 1)) +
     theme(legend.position = 'none')

   p2 <- .data %>%
     filter(type == 'continuous') %>%
     ggplot(aes(
       x = get(x_var),
       y = reorder(name, order),
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
       x = case_when(
         compare == 'means' ~ 'Standardized mean difference',
         compare == 'variance' ~ 'Ratio of variance (log scale)',
         compare == 'covariance' ~ 'Standardized mean difference of interactions (balance of covaraince)'
       ),
       y = NULL,
       color = NULL
     ) +
     theme(legend.position = 'none')

  if (compare == 'variance'){
    p <- p2 +
      coord_cartesian(xlim = c(.25, 4)) +
      scale_x_continuous(trans='log10') +
      labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 4 times different',
          'points represent the treatment group'
        )
      )
  }else{
    p2 <-p2 + coord_cartesian(xlim = c(-2, 2))
    if(length(unique(.data$type)) > 1){
      p1 <- p1 + facet_wrap(~type)
      p2 <- p2 + facet_wrap(~type)

      p1 <- p1 + labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 2 standard deviations.',
          'points represent the treatment group'
        )
      )

      p <- p1 + p2

    }else if(unique(.data$type) == 'continuous'){
      p <- p2 + labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 2 standard deviations.',
          'points represent the treatment group'
        )
      )
    }else{
      p <- p1 + labs(
        title = 'Balance',
        subtitle =  'points represent the treatment group')
    }

  }

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

print_balance <- function(.data, treatment, confounders, estimand = c('ATE', 'ATT', 'ATC')){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")

  data_types <-
    apply(.data[, confounders], 2, function(i)
      identical(unique(i)[order(unique(i))], c(0, 1))) %>%
    as.data.frame()

  data_types$name<- rownames(data_types)
  names(data_types)[1] <- 'type'

  data_types <- data_types %>%
    mutate(type = if_else(type, 'binary/categorical', 'continuous')) %>%
    arrange(name)

  table <- .data %>%
    dplyr::select(all_of(c(confounders, treatment))) %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    dplyr::mutate(
      raw_means = mean_TRUE - mean_FALSE,
      means =
        dplyr::case_when(
          estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
          estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
          estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
        ),
           variance = sqrt(variance_TRUE/variance_FALSE)
    ) %>%
    na.omit() %>%
    dplyr::select(name, raw_means, means, variance) %>%
    dplyr::rename(variable = name,
           `difference in means` = raw_means,
           `standardized difference in means` = means,
           `ratio of the variance` = variance) %>%
    dplyr::mutate(across(where(is.numeric), round, 2)) %>%
    dplyr::arrange(variable)

    table$`standardized difference in means` <- as.character(table$`standardized difference in means`)
    table$`ratio of the variance` <- as.character(table$`ratio of the variance`)
    table[data_types$type != 'continuous', 3] <- '--'
    table[data_types$type != 'continuous', 4] <- '--'

    return(table)

}


#' @title Print covariance statistics
#' @description See balance statistics of covariance for specified variables between treatment and control groups.
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
#' print_covariance(lalonde, 'treat', confounders = c('re78', 'age', 'educ'), estimand = 'ATE')

print_covariance <- function(.data, treatment, confounders, estimand = c('ATE', 'ATT', 'ATC')){

  if(missing(treatment)) stop('enter a string indicating the name of the treatment variable')
  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)
  classes <- sapply(.data[, confounders], class)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")
    # gets interactions of all columns
    cov_dat <- combn(.data[, confounders], 2, FUN = Reduce, f = `*`)

    # get column names so we know what is what
    colnames(cov_dat) <- paste(
      combn(names(.data[, confounders]), 2)[1, ],
      combn(names(.data[, confounders]), 2)[2, ],
      sep = '*')
    # add back treatment
    .data <- cbind.data.frame(cov_dat, treatment = .data[[treatment]])

  table <- .data %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    mutate(
      raw_means = mean_TRUE - mean_FALSE,
      means =
             case_when(
               estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
               estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
               estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
             ),
      variance = sqrt(variance_TRUE/variance_FALSE)
    ) %>%
    na.omit() %>%
    dplyr::select(name, means) %>%
    arrange(desc(abs(means))) %>%
    rename(variable = name,`standardized difference in means` = means) %>%
    mutate(across(where(is.numeric), round, 2))

    return(table)

}


#' @title Plot the covariance
#' @description Visualize balance of the covariance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest

#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @examples
#' data(lalonde)

#' plot_covariance(lalonde, 'treat', c('re75','re74' , 'age', 'educ')) + labs(title = 'My new title')

plot_covariance <- function(.data, treatment, confounders){
  .data$treatment <- .data[[treatment]]
  #.data[, confounders] <- apply(.data[, confounders], 2, function(i) (i - mean(i))/sd(i))
  .data %>%
    GGally::ggpairs(
      upper = list(continuous = "density", combo = "box_no_facet"),
      lower = list(continuous = "points",  combo = "box_no_facet"),
      columns = confounders,
      aes(colour= as.factor(treatment), alpha = .7)) +
    scale_color_manual(values = c('blue', 'red')) +
    scale_fill_manual(values = c('blue', 'red'))
}

