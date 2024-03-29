% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mROC.R
\name{calc_mROC_stats}
\alias{calc_mROC_stats}
\title{Calculates the absolute surface between the empirical and expected ROCs}
\usage{
calc_mROC_stats(y, p, ordered = FALSE, fast = TRUE)
}
\arguments{
\item{y}{y vector of binary responses}

\item{p}{p vector of predicted probabilities (same length as y)}

\item{ordered}{defaults to false}

\item{fast}{defaults to true}
}
\value{
Returns a list with the A (mean calibration statistic) and B (mROC/ROC equality statistic) as well as the direction of potential miscalibration (sign of the difference between the actual and predicted mean risk)
}
\description{
Calculates the absolute surface between the empirical and expected ROCs
}
