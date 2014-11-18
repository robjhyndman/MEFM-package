\name{sa.econ}
\alias{sa.econ}
\docType{data}
\title{
Historical demographic & economic data for South Australia
}
\description{
Annual data for South Australia including population, GSP, residential electricity price, total electricity price and cooling/heating degree days 
}
\format{ A multivariate time series with the following columns:
\describe{
\item{pop}{State population (thousands of people)}
\item{gsp}{Gross State Product chain volume estimate (millions of dollars) ?????}
\item{resiprice}{Residential price index (cents per kWh)}
\item{totalprice}{Total price (cents per kWh)}
\item{anndemand}{Annual electricity demand (GW).}
\item{ddays}{Cooling degree days with a threshold of 18.5 degrees Celsius.}
}}

\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}

\examples{
  plot(sa.econ)
}
\keyword{datasets}
