
load('~/Downloads/hw4.Rdata')
confounders <- c('momage', 'income', 'bw', 'dayskidh')
plot_balance(hw4, 'treat', confounders, compare = 'covariance')
hw4 <- hw4 %>%
  filter(bw < 3000)
hw4 %>% count(black, hispanic, white)
hw4 %>% mutate(test = black *white) %>% summarise(mean(test))
#.25, 4 (log scale)
hw4[, confounders] <- apply(hw4[, confounders], 2, function(i) (i - mean(i))/sd(i))
library(GGally)
hw4 %>%
  ggpairs(
    upper = list(continuous = "density"),
    lower = list(continuous = "points"),
    columns = confounders,
    aes(colour= as.factor(treat), alpha = .7)) +
  scale_color_manual(values = c('blue', 'red')) +
  scale_fill_manual(values = c('blue', 'red')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
