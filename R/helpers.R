
`%notin%` <- Negate(`%in%`)

# coerce_to_logical(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

validate_model <- function(.model){
  if (!is(.model, "bartcFit")) stop(".model must be of class bartcFit")
}

