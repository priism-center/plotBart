plot_c_moderator_loess <- function(.model, moderator, line.color = 'blue'){
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
  posterior_means <- lapply(split_posterior, rowMeans)
  
  # unlist into a data.frame for ploting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- moderator[order(moderator)]
  rownames(dat) <- 1:nrow(dat)
  
  # plot it
  p <- ggplot(dat, aes(moderator, value)) + 
    geom_point() + 
    geom_smooth(method = 'loess', se = F, size = 1.5, color = line.color) + 
    labs(y = 'icate', x = element_blank())
  
    
    return(p)
  }
plot_moderator_c_loess(bart.fit, moderator = ihdp$bw)
plot_moderator_d_linerange(bart.fit, moderator = ihdp$cig, horizontal = T)
plot_moderator_d_density(bart.fit, moderator = ihdp$cig)

