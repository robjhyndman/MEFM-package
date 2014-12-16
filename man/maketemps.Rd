\name{maketemps}
\alias{maketemps}
\title{
Create lagged temperature variables
}
\description{
The function is used to create lagged temperature variables for model estimation.
}
\usage{
maketemps(x, temp_sites, periods = 48)
}
\arguments{
  \item{x}{
Data frame including temperature observations from 1 or 2 weather stations
}
  \item{temp_sites}{
The number of weather stations, select between 1 and 2
}
  \item{periods}{
The periods within a day, choose between 48 (half-hourly data) and 24 (hourly data)
}
}
\value{
The complete data frame that can be used for model estimation.
}
\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}
\author{
Rob J Hyndman and Shu Fan
}

\seealso{
\code{\link{sa}},
\code{\link{demand_model}}
}

\examples{
sa <- maketemps(sa,2,48)

}

\keyword{Temperature}
\keyword{Electricity demand model}
