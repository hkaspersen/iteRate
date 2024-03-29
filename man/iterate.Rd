% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate.R
\name{iterate}
\alias{iterate}
\title{Run iteration analysis}
\usage{
iterate(df, runs = 1000, seed = 10, method = "mean", dist_value1,
  dist_value2)
}
\arguments{
\item{df}{A data frame to run the analysis on, with columns called "group1" and "group2" for the different groups, and "value" for the distances}

\item{runs}{How many iterations should be run?}

\item{seed}{Set seed number}

\item{method}{The summarizing method used, either "mean", "below", "above", "within", "without", "median", or "quantile"}

\item{dist_value1}{The first distance value to calculate from, usually the lower value}

\item{dist_value2}{The second distance value to calculate from, usually the higher value}
}
\description{
Run iterations over given data frame with the selected method
}
