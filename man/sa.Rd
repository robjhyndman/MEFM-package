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
  A data frame with 124848 half-hourly observations on the following 19 variables.
  \describe{
    \item{\code{demand}}{a numeric vector containing half-hourly electricity demand for South Australia.}
    \item{\code{offset}}{a numeric vector containing half-hourly demand from some industrial customers who are not temperature sensitive (e.g., mines and smelters).}
    \item{\code{timeofday}}{a numeric vector giving the time of day (0-47).}
    \item{\code{date}}{a numeric vector giving the date within the month (1-31).}
    \item{\code{month}}{a numeric vector giving the month (1-12).}
    \item{\code{year}}{a numeric vector giving the year (2000-2014).}
    \item{\code{day}}{a factor with levels \code{Mon} \code{Tue} \code{Wed} \code{Thu} \code{Fri} \code{Sat} \code{Sun}}
    \item{\code{idate}}{a numeric vector giving the date in days since 1 January 1900.}
    \item{\code{holiday}}{a factor with levels \code{Normal} \code{Day before} \code{Holiday} \code{Day after}.}
    \item{\code{workday}}{a character vector with values NWD (Non-WorkDay) and WD (WorkDay).}
    \item{\code{timeofyear}}{a numerical time series giving the time in days since midnight on 1 January of each year.}
    \item{\code{Year}}{a numeric time series giving the time in years.}
    \item{\code{fyear}}{a numeric vector giving the financial year (starting 1 July).}
    \item{\code{temp1}}{a numeric vector giving the temperature in Celsius at location 1}
    \item{\code{temp2}}{a numeric vector giving the temperature in Celsius at location 2.}
    \item{\code{anndemand}}{a numeric vector giving the total demand in each year.}
    \item{\code{annoffset}}{a numeric vector giving the total offset demand in each year.}
    \item{\code{ddemand}}{a numeric vector giving the normalized demand (demand/anndemand).}
    \item{\code{doffset}}{a numeric vector giving the normalized offset (offset/annoffset).}
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
