% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcHDDCDD.R
\name{calcHDDCDD}
\alias{calcHDDCDD}
\title{Calculate Heating and Cooling Degree Days (HDD/CDD)}
\usage{
calcHDDCDD(scenario = "SSPs", fromSource = TRUE, endOfHistory = 2025)
}
\arguments{
\item{scenario}{Character vector specifying the socio-economic scenario(s), e.g., \code{"ssp2"}.}

\item{fromSource}{Logical. If \code{TRUE}, loads HDD/CDD data from source.
If \code{FALSE}, computes them using the \strong{CLIMBED} package.}

\item{endOfHistory}{Integer. Upper temporal boundary for historical data.
If it deviates from 2025, \code{fromSource} needs to be set to \code{FALSE}.}
}
\value{
A magclass object with annual degree-day values.
}
\description{
Computes or loads annual heating and cooling degree days (HDDs and CDDs)
for given socio-economic scenarios.
}
\note{
For \code{fromSource = FALSE}, the job requires a runtime of >24h and memory > 100G.
}
\author{
Hagen Tockhorn
}
