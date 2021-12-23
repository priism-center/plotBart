
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

fit_pd_ <- function(x, z1, z0, index, .model){
  z1[, index] <- x
  z0[, index] <- x
  preds.1 <- predict(.model, newdata = z1)
  preds.0 <- predict(.model, newdata = z0)
  preds <- preds.1 - preds.0

  cate <- apply(preds, 1, mean)
  return(cate)
}

ci_ <- function(x) quantile(x, probs = c(0.025, 0.1, 0.9, 0.975))

clamp <- function(x, x_min, x_max) pmin(x_max, pmax(x, x_min))
