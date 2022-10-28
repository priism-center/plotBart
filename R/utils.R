
`%notin%` <- Negate(`%in%`)

# coerce_to_logical_(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical_ <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

is_numeric_vector_ <- function(x){
  if (!inherits(x, 'numeric')) stop('moderator must be numeric vector')
}

is_discrete_ <- function(x){
  # must be more than one level and all levels can't be unique
  is_discrete <- length(unique(x)) > 1 && length(unique(x)) < length(x)
  if (!isTRUE(is_discrete)) stop('moderator must be discrete')
}

# adjust [moderator] to match estimand
adjust_for_estimand_ <- function(.model, x){
  validate_model_(.model)

  out <- switch(
    .model$estimand,
    ate = x,
    att = x[.model$trt == 1],
    atc = x[.model$trt != 1]
  )

  return(out)
}

# used within plot_moderator_c_pd()
fit_pd_ <- function(x, z1, z0, index, .model){
  z1[, index] <- x
  z0[, index] <- x
  preds.1 <- predict(.model, newdata = z1)
  preds.0 <- predict(.model, newdata = z0)
  preds <- preds.1 - preds.0

  cate <- apply(preds, 1, mean)
  return(cate)
}


#
# rpart_ggplot_overlap <- function(.model){
#
#   # remove depth information from model so resulting plot is easy to read
#   .model$frame$dev <- 1
#
#   # extract data to construct dendrogram
#   fitr <- ggdendro::dendro_data(.model)
#   n_leaf <- .model$frame$n[.model$frame$var == '<leaf>']
#   n_split <- .model$frame$n[.model$frame$var != '<leaf>']
#   pred_split <- round(.model$frame$yval[.model$frame$var != '<leaf>'], 1)
#   terminal_leaf_y <- 0.1
#   leaf_labels <- tibble(
#     x = fitr$leaf_labels$x,
#     y = terminal_leaf_y,
#     label = paste0(
#       'y = ', fitr$leaf_labels$label,
#       '\nn = ', n_leaf)
#   )
#   yes_no_offset <- c(0.7, 1.3)
#   yes_no <- tibble(
#     x = c(fitr$labels$x[[1]] * yes_no_offset[1],
#           fitr$labels$x[[1]] * yes_no_offset[2]),
#     y = rep(fitr$labels$y[[1]], 2),
#     label = c("yes", "no")
#   )
#   split_labels <- tibble(
#     x = fitr$labels$x,
#     y = fitr$labels$y + 0.085,
#     label = paste0(
#       'y = ', pred_split,
#       '\nn = ', n_split
#     )
#   )
#
#   # set terminal segments to y = terminal_leaf_y
#   initial_node_y <- fitr$labels$y[[1]]
#   fitr$segments <- fitr$segments %>%
#     mutate(y_new = ifelse(y > yend, y, yend),
#            yend_new = ifelse(yend < y, yend, y)) %>%
#     select(n, x, y = y_new, xend, yend = yend_new) %>%
#     mutate(y = ifelse(y > initial_node_y, terminal_leaf_y, y),
#            yend = ifelse(x == xend & x == round(x) & y > yend, terminal_leaf_y, yend))
#
#   # set plot constants
#   label_text_size <- 3
#   x_limits <- c(0.5, nrow(fitr$leaf_labels) + 0.5)
#   y_limits <- c(min(fitr$segments$y) - 0.05,
#                 max(fitr$segments$y) + 0.15)
#
#   # plot it
#   p <- ggplot() +
#     geom_segment(data = fitr$segments,
#                  aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_label(data = yes_no,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = leaf_labels,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = split_labels,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = fitr$labels,
#                aes(x = x, y = y, label = label),
#                label.size = NA, fontface = 'bold') +
#     expand_limits(x = x_limits,
#                   y = y_limits) +
#     scale_x_continuous(labels = NULL, breaks = NULL) +
#     scale_y_continuous(labels = NULL, breaks = NULL) +
#     labs(title = 'Exploratory non-overlap covariates',
#          x = NULL,
#          y = NULL) +
#     theme(panel.background = element_blank())
#
#   return(p)
# }


pclamp_ <- function(x, x_min, x_max) pmin(x_max, pmax(x, x_min))

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'x',
      'xend',
      'y',
      'yend',
      'y_new',
      'yend_new',
      'name',
      'value',
      'support_rule',
      'index',
      'threshold',
      'point',
      '.min',
      '.max',
      'label',
      'Z',
      'Z_treat',
      '..count..',
      '..density..',
      'iteration',
      'Chain',
      'icate.o',
      'ci_2.5',
      'ci_97.5',
      'ci_10',
      'ci_90'
    )
  )
}

