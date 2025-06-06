% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolAggregateBrick.R
\name{toolAggregateBrick}
\alias{toolAggregateBrick}
\title{Aggregate BRICK input data to lower granularity}
\usage{
toolAggregateBrick(x, granularity = NULL, weight = NULL)
}
\arguments{
\item{x}{MagPIE object at BRICK default resolution}

\item{granularity}{character, name of BRICK granularity}

\item{weight}{magPIE object with weights, if \code{NULL} (default), an
extensive variable is assumed and values are summed instead of averaged}
}
\value{
named list with aggregated value and weights
}
\description{
Aggregate BRICK input data to lower granularity
}
\author{
Robin Hasse
}
