\name{obj}
\alias{obj}
\docType{data}
\title{
Historical data for model estimation
}
\description{
Historical data of South Australia
}
\usage{data(obj)}
\format{
  A data frame with 116880 observations on the following 52 variables.
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
    \item{\code{dlsave}}{a character vector}
    \item{\code{timeofyear}}{a ts}
    \item{\code{Year}}{a ts}
    \item{\code{fyear}}{a numeric vector}
    \item{\code{nye}}{a factor with levels \code{FALSE} \code{TRUE}}
    \item{\code{millenium}}{a factor with levels \code{FALSE} \code{TRUE}}
    \item{\code{temp1}}{a numeric vector}
    \item{\code{temp2}}{a numeric vector}
    \item{\code{temp}}{a numeric vector}
    \item{\code{dtemp}}{a numeric vector}
    \item{\code{prevtemp1}}{a numeric vector}
    \item{\code{prevtemp2}}{a numeric vector}
    \item{\code{prevtemp3}}{a numeric vector}
    \item{\code{prevtemp4}}{a numeric vector}
    \item{\code{prevtemp5}}{a numeric vector}
    \item{\code{prevtemp6}}{a numeric vector}
    \item{\code{day1temp}}{a numeric vector}
    \item{\code{day2temp}}{a numeric vector}
    \item{\code{day3temp}}{a numeric vector}
    \item{\code{day4temp}}{a numeric vector}
    \item{\code{day5temp}}{a numeric vector}
    \item{\code{day6temp}}{a numeric vector}
    \item{\code{prevdtemp1}}{a numeric vector}
    \item{\code{prevdtemp2}}{a numeric vector}
    \item{\code{prevdtemp3}}{a numeric vector}
    \item{\code{prevdtemp4}}{a numeric vector}
    \item{\code{prevdtemp5}}{a numeric vector}
    \item{\code{prevdtemp6}}{a numeric vector}
    \item{\code{day1dtemp}}{a numeric vector}
    \item{\code{day2dtemp}}{a numeric vector}
    \item{\code{day3dtemp}}{a numeric vector}
    \item{\code{day4dtemp}}{a numeric vector}
    \item{\code{day5dtemp}}{a numeric vector}
    \item{\code{day6dtemp}}{a numeric vector}
    \item{\code{lastmax}}{a numeric vector}
    \item{\code{lastmin}}{a numeric vector}
    \item{\code{avetemp}}{a numeric vector}
    \item{\code{anndemand}}{a numeric vector}
    \item{\code{annoffset}}{a numeric vector}
    \item{\code{ddemand}}{a numeric vector}
    \item{\code{doffset}}{a numeric vector}
    \item{\code{dayofseason}}{a ts}
  }
}
\details{
Historical data for South Australia, including half-hourly demand, temperatures from 2 locations, weekday, weekend, and holiday dates.
}
\source{
http://www.aemo.com.au/Electricity/Data/Price-and-Demand/Aggregated-Price-and-Demand-Data-Files
}
\references{
R. J. Hyndman and S. Fan, "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, Vol. 25, No.2, pp. 1142--1153, 2010
}
\examples{
data(obj)
}
\keyword{datasets}
