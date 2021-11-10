plot_icate <- function(.model, 
                       group.by = NULL, group.lab = NULL){
  
  
  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))
  
  
  if(is.null(group.by)){
    p <- ggplot(icates, aes(value)) + 
    geom_histogram()
  }
  
  else if(!is.null(group.by)){
    icates$group <- group.by
    p <- ggplot(icates, aes(value, fill = group.by)) +
      geom_histogram(position = "identity", alpha = .7)
    
  }
  
  return(p)
}


plot_icate(bart.fit, group.by = ihdp$mom.hs)
