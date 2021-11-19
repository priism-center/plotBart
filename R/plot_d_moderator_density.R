plot_d_moderator_density <- function(.model, moderator, alpha = .7, facet = F, ncol = 1){
  .alpha <- alpha 
  .ncol <- ncol
  # adjust moderator to match estimand
  if(.model$estimand == 'att'){
    moderator <- moderator[.model$trt == 1]
  }
  
  else if(.model$estimand == 'atc'){
    moderator <- moderator[.model$trt != 1]
  }
  
  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <-  posterior  %>% 
    t() %>% 
    as_tibble()
  
  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)
  
  # unlist into a data.frame for ploting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- 1:nrow(dat)
  
  p <- ggplot(dat, aes(value, fill = moderator)) + 
    geom_density(alpha = .alpha) + 
    labs(x = 'CATE') + 
    theme(legend.position = 'top', 
          legend.title = element_blank())
    
    if(isTRUE(facet)){
      p <- p + facet_wrap(~moderator, ncol = .ncol)
    }
    
    return(p)
  }

