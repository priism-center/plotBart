#' Plot histogram of individual treatment effects
#'
#' Returns a ggplot ITE plot
#'
#' @param .model a model produced by bartCause::bartc()
#' @author George Perrett, Joe Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause methods
#' @examples
#' data(lalonde, package = 'arm')
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_ITE(model_results)
plot_sate <- function(.model, 
                      type = 'Histigram', 
                      ci_80 = F, ci_95 = F,  reference = NULL,  
                      .mean = F, .median = F){
  
  if (!is(.model, "bartcFit")) stop("Model must be of class bartcFit")
  
  # calculate stats
  posterior <- bartCause::extract(.model, 'cate')
  posterior <- as_tibble(posterior)
  ub <- quantile(posterior$value, .9)
  lb <- quantile(posterior$value, .1)
  ub.95 <- quantile(posterior$value, .975)
  lb.95 <- quantile(posterior$value, .025)
  dd <- density(posterior$value)
  dd <-  with(dd, data.frame(x, y))
  
  # plot hist
  if(type == 'Histigram'){
    
    if(ci_80 == F & ci_95 == F & .mean == F & .median == F){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30', bins = 50) +
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == F){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30', bins = 50) +
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == F & .median == T){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_histogram(alpha = 0.8, fill = 'grey30', bins = 50) +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == T){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(2,3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference) +
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') +  
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    if(ci_80 == F & ci_95 == T & .mean == F & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == F & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == F & ci_95 == T & .mean == T & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1, color = 'grey30') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    if(ci_80 == T & ci_95 == F & .mean == F & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
    if(ci_80 == T & ci_95 == F  & .mean == F & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == F){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == T){
      p <- ggplot(posterior, aes(value)) + 
        geom_histogram(fill = 'grey60', bins = 50) + 
        geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 2, color = 'grey10') + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'frequency')
    }
    
    
  }
  
  if(type == 'Density'){
    
    
    if(ci_80 == F & ci_95 == F & .mean == F & .median == F){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == F){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == F & .median == T){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == F & ci_95 == F  & .mean == T & .median == T){
      p <- posterior %>%
        ggplot(aes(x = value)) +
        geom_vline(xintercept = NULL, linetype = 'dashed', color = 'grey60') +
        geom_density(alpha = 0.8, fill = 'grey30') +
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(2,3)) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density') + 
        theme(legend.title = element_blank())
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference) +
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    
    
    if(ci_80 == T & ci_95 == T  & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) +  
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == T  & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    
    if(ci_80 == F & ci_95 == T  & .mean == F & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    
    if(ci_80 == F & ci_95 == T & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == T  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == F & ci_95 == T & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    
    if(ci_80 == T & ci_95 == F & .mean == F & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    
    if(ci_80 == T & ci_95 == F  & .mean == F & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) + 
        scale_linetype_manual(values = c(3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == F){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        scale_linetype_manual(values = c(2)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
    if(ci_80 == T & ci_95 == F  & .mean == T & .median == T){
      p <- ggplot(dd, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8) + 
        geom_vline(xintercept = reference)+
        geom_vline(aes(xintercept = mean(posterior$value), linetype = 'mean')) + 
        geom_vline(aes(xintercept = median(posterior$value), linetype = 'median')) +
        scale_linetype_manual(values = c(2, 3)) + 
        theme(legend.title = element_blank()) + 
        labs(title = 'Posterior of Average Treatment Effect',
             x = toupper(.model$estimand),
             y = 'density')
    }
    
  }
  
  return(p)
}

# plot_posterior(fit, type = 'density', interval = .95, .mean = T, .median = T, reference = 0) + theme_bw() + 
#   theme(legend.position = c(0.1, 0.9), 
#         legend.title= element_blank())

#plot_posterior(fit,ci_80 = F, .mean = T, reference = NULL)

plot_icate(bart.fit)
