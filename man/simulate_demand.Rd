\name{simulate_demand}
\alias{simulate_demand}
\title{
Simulate the electricity demand for the next season
}
\description{
Simulate the half-hourly/hourly, seasonal peak electricity demand for the next season
}
\usage{
simulate_demand(sim, afcast, nyears = length(sim$hhfit)/seasondays/48, periods = 48)
}
\arguments{
  \item{sim}{
The simulated half-hourly demand (normalized against seasonal average demand)
}
  \item{afcast}{
The demographic and economic forecasts for the next season
}
  \item{nyears}{
The length of years of simulation
}
  \item{periods}{
The periods within a day, choose between 48 (half-hourly data) and 24 (hourly data)
}
}	
\details{
Simulate/forecast the half-hourly/hourly, seasonal peak electricity demand for the next season by incorporating the seasonal 
demographic & economic forecasts (to be provided by the user)
}
\value{
\item{demand}{The forecasted half-hourly demand}
\item{annmax}{The forecasted seasonal maximum demand}
}
\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}
\author{
Rob J. Hyndman and Shu Fan
}
\seealso{
\code{\link{demand_model}}, 
\code{\link{simulate_ddemand}},
\code{\link{MEFM-package}} for examples.
}

\keyword{Electricity demand model}
\keyword{Forecast}
