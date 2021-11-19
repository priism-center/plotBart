plot_icate <- function(.model, 
                       group.by = NULL, bin = 30){
  
  
  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))
  
  if(.model$estimand == 'att'){
    group.by <- group.by[.model$trt == 1]
  }
  
  if(.model$estimand == 'atc'){
    group.by <- group.by[.model$trt == 0]
  }
  
  # base plot 
  p <- ggplot(icates, aes(value)) + 
    geom_histogram(bins = bin)
  
  if(!is.null(group.by)){
    p <- ggplot(icates, aes(value, fill = as.factor(group.by))) + 
      geom_histogram(position = 'identity', bins = bin)
  }
  

  
  return(p)
}

