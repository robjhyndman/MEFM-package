\name{simulate.demand}
\alias{simulate.demand}
\title{
Simulate/forecast the seasonal peak electricity demand for the next season
}
\description{
Simulate/forecast the seasonal peak electricity demand for the next season by incorporating the seasonal 
demographic & economic projections (to be provided by the user)
}
\usage{
simulate.demand(sim, afcast, nyears = length(sim$hhfit)/seasondays/periods, periods = 48)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
The simulated half-hourly demand (normalized against seasonal demand)
}
  \item{afcast}{
The demograpgic and economic forecasts for the next season
}
  \item{nyears}{
The length of years of simulation
}
  \item{periods}{
The periods within a day, choose between 48 (half-hourly data) and 24 (hourly data)
}
}	
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{demand}{The forecasted half-hourly demand}
\item{annmax}{The forecasted seasonal maximum demand}
}
\references{
R. J. Hyndman and S. Fan, "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, Vol. 25, No.2, pp. 1142¨C1153, 2010
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
\code{\link{simulate.ddemand}}
}
\examples{
\dontrun{
demand <- simulate.demand(simdemand,afcast,nyears=length(simdemand$hhfit)/seasondays/48)
# illustrate the results
tmp <- density(demand$annmax,bw="SJ",na.rm=TRUE)
xlim=range(tmp$x)
ymax <- max(tmp$y[tmp$x > xlim[1]]) + 0.1
main <- "Density of seasonal maximum demand"
plot(tmp,main=main,xlab="Demand",xlim=xlim,ylim=c(0,ymax))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Electricity demand model}
\keyword{Forecast}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
