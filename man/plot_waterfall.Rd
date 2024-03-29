% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_waterfall.R
\name{plot_waterfall}
\alias{plot_waterfall}
\title{Plot a waterfall of the ICATEs}
\usage{
plot_waterfall(
  .model,
  descending = TRUE,
  .order = NULL,
  .color = NULL,
  .alpha = 0.5
)
}
\arguments{
\item{.model}{a model produced by `bartCause::bartc()`}

\item{descending}{order the ICATEs by value?}

\item{.order}{a vector representing a custom order}

\item{.color}{a vector representing colors}

\item{.alpha}{transparency value [0, 1]}
}
\value{
ggplot object
}
\description{
Plots the point and posterior intervals of each individual's ICATE ordered by the ICATE or a continuous variable. Points can be colored by a discrete variable.
Waterfall plots are a useful visual diagnostic of possible treatment effect heterogeneity.
A flat line implies little treatment effect heterogeneity while a steeper curve implies that the treatment effect varies across individuals in the sample. Ordering points by a continuous variable or coloring points by a discrete variable can be helpful to identify potential moderators of the treatment effect.
}
\examples{
\donttest{
data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
 response = lalonde[['re78']],
 treatment = lalonde[['treat']],
 confounders = as.matrix(lalonde[, confounders]),
 estimand = 'ate',
 commonSuprule = 'none'
)
plot_waterfall(model_results)
}
}
\author{
George Perrett
}
