% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolAddThermal.R
\name{toolAddThermal}
\alias{toolAddThermal}
\title{Introduce refrigerators as appliances}
\usage{
toolAddThermal(df, mapping, fridgeShare, feOnly = TRUE, shareOf = NULL)
}
\arguments{
\item{df}{data.frame}

\item{mapping}{original EDGE-B region mapping}

\item{fridgeShare}{regional shares of refrigerators}

\item{feOnly}{are absolute values (TRUE) or shares (FALSE) considered?}

\item{shareOf}{column name to calculate share of - must be given if feOnly = FALSE}
}
\description{
Introduce refrigerators as appliances
}
\author{
Hagen Tockhorn
}
