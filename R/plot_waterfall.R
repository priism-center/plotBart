plot_waterfall <- function(.model, descending = T,order = NULL, color = NULL,  alpha = .5){
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  if(!is.null(color)){
    if (!is(color, "vector")) stop("color must be a vector")
  }
  if(!is.null(color)){
    if (nrow(.model$data.rsp@x) != length(color)) stop(paste("color must be a vector of length", nrow(.model$data.rsp@x)))
  }

  .color <- color
  .alpha <- alpha
  .order <- order
  # calculate stats
  posterior <- bartCause::extract(.model, 'icate')
  posterior <-  posterior  %>% 
    t() %>% 
    as_tibble()
  
  icate.m <- apply(posterior, 1, mean)
  icate.sd <- apply(posterior, 1, sd)
  icate.uci <- icate.m + icate.sd*1.96
  icate.lci <- icate.m - icate.sd*1.96
  
  dat <- tibble(icate.m, icate.lci, icate.uci)
  
  
  # specify order of icates on x axis
  if(isTRUE(descending)){
    dat <- dat %>% arrange(desc(icate.m))
  }
  else if(!is.null(order)){
    if(isTRUE(descending)){
    dat <- dat %>% arrange(desc(.order))
    }
    else{
    dat <- dat %>% arrange(.order) 
    }
  }
  else{
    dat <- dat %>% arrange(icate.m)
  }
  
  dat <- dat %>% mutate(icate.o = row_number())
  
  # base plot 
  p <- ggplot(dat, aes(icate.o, icate.m)) + 
    geom_linerange(aes(ymin = icate.lci, ymax = icate.uci), alpha = .alpha) + 
    geom_point() + 
    labs(x = 'ordered icates', y = 'icate') + 
    theme(legend.position = 'top')
  
  # add color 
  if(!is.null(.col)){
    if(.model$estimand == 'att'){
      .col <- .col[, .model$trt == 1]
    }
    if(.model$estimand == 'atc'){
      .col <- .col[, .model$trt != 1]
    }
    
    p <- p + aes(color = as.character(.col)) + theme(legend.title = element_blank())
  }
  
  # apply custom order 
  if(!is.null(order)){
    p <- p + aes(.order, icate.m)
  }
  
  return(p)
}  
