
#' Plot a waterfall
#'
#' TODO: description
#'
#' @param .model a model produced by bartCause::bartc()
#' @param descending order the icates by value?
#' @param .order a vector representing a customer order
#' @param .color a vector representing colors
#' @param .alpha transparency value [0, 1]
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_waterfall(model_results)
plot_waterfall <- function(.model, descending = TRUE, .order = NULL, .color = NULL, .alpha = 0.5){

  # TODO: descending = FALSE fails

  # to satisfy CMD CHECK
  icate.o <- NULL

  validate_model_(.model)
  if(!is.null(.color)){
    if (!is.vector(.color)) stop("color must be a vector")
    if (nrow(.model$data.rsp@x) != length(.color)) stop(paste("color must be a vector of length", nrow(.model$data.rsp@x)))
  }

  # calculate stats
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as_tibble()

  icate.m <- apply(posterior, 1, mean)
  icate.sd <- apply(posterior, 1, sd)
  icate.uci <- icate.m + icate.sd * 1.96
  icate.lci <- icate.m - icate.sd * 1.96

  dat <- tibble(icate.m, icate.lci, icate.uci)

  if(!is.null(.color)){
    .color <- adjust_for_estimand_(.model, .color)
    dat$.color <- .color
  }
  # specify order of icates on x axis
  if(isTRUE(descending)){
    dat <- dat %>% arrange(desc(icate.m))
  } else if(!is.null(.order)){
    if(isTRUE(descending)){
      dat <- arrange(dat, desc(.order))
    } else{
      dat <- arrange(dat, .order)
    }
  } else{
    dat <- arrange(dat, icate.m)
  }

  dat <- mutate(dat, icate.o = row_number())

  # create base plot
  p <- ggplot(dat, aes(x = icate.o, y = icate.m)) +
    geom_linerange(aes(ymin = icate.lci, ymax = icate.uci),
                   alpha = .alpha) +
    geom_point() +
    labs(title = NULL,
         x = 'Ordered icates',
         y = 'icate') +
    theme(legend.position = 'top')

  # add color
  if(!is.null(.color)){
    p <- p +
      aes(color = as.character(.color)) +
      labs(title = NULL,
           x = 'Ordered icates',
           y = 'icate')
  }

  # apply custom order
  if(!is.null(.order)){
    .order <- adjust_for_estimand_(.model, .order)
    p <- p +
      aes(x = .order, y = icate.m) +
      labs(title = NULL,
           x = 'ordered icates',
           y = 'icate')
  }

  return(p)
}
