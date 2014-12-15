\name{sa}
\alias{sa}
\docType{data}
\title{
Historical data for model estimation
}
\description{
Historical data of South Australia
}
\format{
  A data frame with 124848 observations on the following 19 variables.
  \describe{
    \item{\code{demand}}{a numeric vector}
    \item{\code{offset}}{a numeric vector}
    \item{\code{timeofday}}{a numeric vector}
    \item{\code{date}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{day}}{a factor with levels \code{Mon} \code{Tue} \code{Wed} \code{Thu} \code{Fri} \code{Sat} \code{Sun}}
    \item{\code{idate}}{a numeric vector}
    \item{\code{holiday}}{a factor with levels \code{Normal} \code{Day before} \code{Holiday} \code{Day after}}
    \item{\code{workday}}{a character vector}
    \item{\code{timeofyear}}{a ts}
    \item{\code{Year}}{a ts}
    \item{\code{fyear}}{a numeric vector}
    \item{\code{temp1}}{a numeric vector}
    \item{\code{temp2}}{a numeric vector}
    \item{\code{anndemand}}{a numeric vector}
    \item{\code{annoffset}}{a numeric vector}
    \item{\code{ddemand}}{a numeric vector}
    \item{\code{doffset}}{a numeric vector}
  }
}
\details{
Historical data for South Australia, including half-hourly demand, temperatures from 2 locations, weekday, weekend, and holiday dates.
Only data from October-March were retained for summer analysis and modelling.
}
\source{
\url{http://www.aemo.com.au/Electricity/Data/Price-and-Demand/Aggregated-Price-and-Demand-Data-Files}
}
\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}
\examples{
plot(ts(sa[,"demand"],freq=48*seasondays,start=c(2000,7)))
}
\keyword{datasets}
