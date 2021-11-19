plot_moderator_linerange <- function(.model, moderator, alpha = .7, horizontal = F){
  .alpha <- alpha 

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
  
  dat <- dat %>% 
    group_by(moderator) %>% 
    mutate(.min = min(value), 
           .max = max(value), 
           point = mean(value)) %>% 
    dplyr::select(-value) %>% 
    arrange(desc(point)) %>% 
    ungroup() %>% 
    distinct()
  
  
  if(isFALSE(horizontal)){
    p <-  p <- ggplot(dat, aes(moderator, point, color = moderator)) + 
      geom_point(size = 2) + 
      geom_linerange(aes(ymin = .min, ymax = .max), alpha = .alpha) + 
      theme(legend.title = element_blank(), 
            legend.position = 'bottom') + 
      labs(x = element_blank(), y = 'CATE')
  }
  else{
    p <-  p <- ggplot(dat, aes(point, moderator, color = moderator)) + 
      geom_point(size = 2) + 
      geom_linerange(aes(xmin = .min, xmax = .max), alpha = .alpha) + 
      theme(legend.title = element_blank(), 
            legend.position = 'bottom') + 
      labs(x = element_blank(), y = 'CATE')
  }
  
  return(p)
  
}

