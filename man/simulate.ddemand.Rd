\name{simulate.ddemand}
\alias{simulate.ddemand}
\title{
Simulate the half-hourly/hourly temperature and demand
}
\description{
Simulate the half-hourly/hourly temperature and demand based on the historical data and the half-hourly/hourly demand model
}
\usage{
simulate.ddemand(model, hdata, simyears = 1000, delta = 5, periods = 48)
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
The length of years for simulation
}
  \item{delta}{
The value of blockdays offset limit
}
  \item{periods}{
The periods within a day, 48 for half-hourly, 24 for hourly
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class \code{simdemand}, basically a list including elements
\item{hhfit}{Simulated half-hourly demand}
\item{hhres}{Simulated half-hourly residuals}
\item{ores}{Simulated half-hourly offset demand}
\item{a}{Seasonal model}
}
\references{
R. J. Hyndman and S. Fan, "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, Vol. 25, No.2, pp. 1142--1153, 2010
}
\author{
R. J. Hyndman and S. Fan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{demand.model}}
\code{\link{simulate.demand}}
\code{\link{obj}}
}
\examples{
\dontrun{
simulate.ddemand(obj.model, obj, simyears=years_simu, delta=5)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Electricity demand model}
\keyword{Temperature simulate}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
