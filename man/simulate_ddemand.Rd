\name{simulate_ddemand}
\alias{simulate_ddemand}
\title{
Temperature and demand Simulation
}
\description{
Simulate the half-hourly/hourly temperature and demand
}
\usage{
simulate_ddemand(model, hdata, simyears = 1000, delta = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
The demand models
}
  \item{hdata}{
The half-hourly/hourly demand, temperature and seasonality data
}
  \item{simyears}{
The length of years of simulation
}
  \item{delta}{
The value of blockdays offset limit for bootstrap
}
}
\details{
Simulate the half-hourly/hourly temperature and demand using the historical data and the half-hourly/hourly demand model
}
\value{
An object of class \code{simdemand}, basically a list including elements
\item{hhfit}{Simulated half-hourly demand}
\item{hhres}{Simulated half-hourly residuals}
\item{ores}{Simulated half-hourly offset demand}
\item{a}{Seasonal model}
}
\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}
\author{
Rob J Hyndman and Shu Fan
}
\seealso{
\code{\link{demand_model}},
\code{\link{simulate_demand}},
\code{\link{sa}},
\code{\link{MEFM-package}} for examples.
}

\keyword{Electricity demand model}
\keyword{Temperature simulate}