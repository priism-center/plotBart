% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_balance.R
\name{print_covariance}
\alias{print_covariance}
\title{Print covariance statistics}
\usage{
print_covariance(
  .data,
  treatment,
  confounders,
  estimand = c("ATE", "ATT", "ATC")
)
}
\arguments{
\item{.data}{dataframe}

\item{treatment}{the column denoted treatment. Must be binary.}

\item{confounders}{character list of column names denoting the X columns of interest}

\item{estimand}{character of either ATE, ATT or ATC the causal estimand you are making inferences about}
}
\value{
tibble
}
\description{
See balance statistics of covariance for specified variables between treatment and control groups.
}
\examples{
data(lalonde)
print_covariance(lalonde, 'treat', confounders = c('re78', 'age', 'educ'), estimand = 'ATE')
}
\author{
George Perrett
}
