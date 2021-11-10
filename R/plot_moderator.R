library(tidyverse)
library(bartCause)
ihdp <- read_csv('~/Dropbox/thinkCausal_dev/thinkCausal/data/IHDP_observational.csv')
ihdp <- ihdp %>% select(-c(yc1hat, yc0hat))
bart.fit <- bartc(y.obs, treat, ., ihdp, estimand = 'att')
.model <- bart.fit
plot_sub <- function(.model, 
                     sub_group, data_type, 
                     .alpha = .7, 
                     facet = F, .ncol = 1){
  
  if(.model$estimand == 'att'){
    sub_group <- sub_group[.model$trt == 1]
  }
  
  else if(.model$estimand == 'atc'){
    sub_group <- sub_group[.model$trt == 0]
  }
  
  if(data_type == 'catagorical'){
    posterior <- bartCause::extract(.model, 'icate')
    posterior <-  posterior  %>% 
      t() %>% 
      as_tibble()
    
    split_posterior <- split(posterior, sub_group)
    posterior_means <- lapply(split_posterior, colMeans)
    dat <- data.frame(value = unlist(posterior_means), name = sub("\\..*", '', rownames(dat)))
    rownames(dat) <- 1:nrow(dat)
    if(facet == F){
      p <- ggplot(dat, aes(value, fill = name)) + 
        geom_density(alpha = .alpha) + 
        labs(x = 'CATE')
    }
    else if(facet == T){
      #TODO add facet reorder capacity 
      p <- ggplot(dat, aes(value, fill = name)) + 
        geom_density(alpha = .alpha) + 
        facet_wrap(~name, ncol = .ncol) + 
        labs(x = 'CATE')
        
    }
      
  }
  
  return(p)
  
}
  
  
  
  
  else if(data_type = 'continuous'){
   
    posterior <- bartCause::extract(.model, 'icate')
    posterior <-  posterior  %>% 
      t() %>% 
      as_tibble() 
    
    split_posterior <- split(posterior, sub_group)
    posterior_means <- lapply(split_posterior, colMeans)
    dat <- data.frame(value = unlist(posterior_means))
    dat$sub_group <- as.numeric(sub("\\..*", '', rownames(dat)))
    rownames(dat) <- 1:nrow(dat)
    
    dat %>% 
      group_by(sub_group) %>% 
      mutate(point = mean(value)) %>% 
      select(-value) %>% 
      distinct() %>% 
      ggplot(aes(sub_group, point)) + 
      geom_point() + 
      geom_smooth()
    
    
    point_est  <- dat %>% 
      group_by(sub_group) %>% 
      mutate(point = mean(value)) %>% 
      select(-value) %>% 
      distinct() %>% 
      ungroup()

    
    dat_quantiles <- split(posterior, sub_group)
    dat_quantiles  <- lapply(dat_quantiles, t)
    dat_quantiles  <- lapply(dat_quantiles, as.vector)
    get_quant <- function(x){
      quantile(x, c(.025, .1, .9, .975))
    }
    
    dat_grouped <- lapply(dat_grouped, get_quant)
    quantiles <- tibble(
      bind_rows(dat_grouped), 
      sub_group = as.numeric(names(dat_grouped))) 
    
    dat <- point_est %>% left_join(quantiles)
    
      ggplot(dat) + 
        geom_point(aes(sub_group, point)) + 
        geom_smooth(aes(sub_group, point), se = F)
      
      ?geom_ribbon
    
      quantile(check$value, c(.1,.9 ))
      distinct() %>% 
      ggplot(aes(sub_group, point)) + 
      geom_point() + 
      geom_smooth()
    
  }
}

hist(posterior)
icate <- bartCause::extract(.model, 'icate')
ite <-  bartCause::extract(.model, 'ite')
cate <- bartCause::extract(.model, 'cate')
icate.m <- apply(icate, 1, mean)
ite.m <- apply(ite, 1, mean)
hist(icate.m)
hist(cate)
sd(icate.m)
sd(cate)
sub_group <- ihdp$work.dur
