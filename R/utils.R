
`%notin%` <- Negate(`%in%`)

# coerce_to_logical(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

# validate the model is a bartc model
validate_model <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

is_numeric_vector <- function(x){
  if (!inherits(x, 'numeric')) stop('moderator must be numeric vector')
}

# adjust [moderator] to match estimand
adjust_for_estimand <- function(.model, x){
  validate_model(.model)

  out <- switch(
    .model$estimand,
    ate = x,
    att = x[.model$trt == 1],
    atc = x[.model$trt != 1]
  )

  return(out)
}
