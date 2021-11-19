plot_c_moderator_pd <- function(.model, moderator, bins = 15, legend = c('none', 'right', 'top', 'bottom')){
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  if (bins < 1) stop("bins must be a number greater than 1")
  
  new_data <- as_tibble(.model$data.rsp@x.test)
  new_data_z1 <- new_data
  new_data_z1$treat <- 1
  new_data_z1 <- cbind(.model$fit.rsp$y, new_data_z1)
  names(new_data_z1)[1] <- as.character(.model$call$response)
  
  new_data_z0 <- new_data
  new_data_z0$treat <- 0
  new_data_z0 <- cbind(.model$fit.rsp$y, new_data_z0)
  names(new_data_z0)[1] <- as.character(.model$call$response)
  
  # locate the moderator in bartc data
  search <- function(x){
    out <- sum(moderator - x)
    return(out)
  }
  
  index <- which(sapply(new_data_z0, search) == 0)
  
  # get range for predictions 
  cut <- bins-1
  p <- seq(min(moderator), max(moderator),  (max(moderator) - min(moderator))/cut )
  if(length(p) < length(unique(moderator))){
    range <- p
  }
  
  else{
    range <- unique(moderator)[order(unique(moderator))]
  }
  
  fit_pd <- function(x){
    new_data_z1[, index] <- x
    new_data_z0[, index] <- x
    preds.1 <- predict(.model, newdata = new_data_z1)
    preds.0 <- predict(.model, newdata = new_data_z0)
    preds <- preds.1 - preds.0
    
    cate <- apply(preds, 1, mean)
    return(cate)
  }
  
  cates <- map(range, fit_pd)
  cates <- bind_cols(cates)
  cates.m <- apply(cates, 2, mean)
  cates.m <- as_tibble(cbind(cates.m, range))
  ci <- function(x){
    out <- quantile(x, probs = c(.025, .1, .9, .975))
    return(out)
  }
  cates.ci <- as_tibble(t(apply(cates, 2, ci)))
  cates_plot <- cbind(cates.m, cates.ci)
  names(cates_plot)[3:6] <- paste0('ci_', names(cates_plot)[3:6])
  names(cates_plot)[3:6] <- substr(names(cates_plot)[3:6], 1, nchar(names(cates_plot)[3:6]) - 1)
  
  p <- ggplot(cates_plot) + 
    geom_ribbon(aes(x = range, y = cates.m, ymin = ci_2.5, ymax = ci_97.5, fill = '95% ci')) + 
    geom_ribbon(aes(x = range, y = cates.m, ymin = ci_10, ymax = ci_90, fill = '80%ci')) + 
    scale_fill_manual(values = c('grey40', 'grey60')) + 
    geom_point(aes(range, cates.m),size = 2) +
    geom_line(aes(range, cates.m)) + 
    labs(y = 'CATE', x = element_blank()) + 
    theme(legend.title = element_blank(), legend.position = legend)
  
  return(p)

}

plot_c_moderator_pd(.model, dat$bw)
