\name{demand_model}
\alias{demand_model}
%\alias{simulate_ddemand}
%\alias{simulate_demand}
\title{
Estimate the electricity demand models
}
\description{
Estimate the half-hourly/hourly and seasonal demand models.
}
\usage{
demand_model(hhdata, adata, hhoptformula, aoptformula, periods = 48)
}
\arguments{
  \item{hhdata}{
The historical half-hourly/hourly demand, temperature and seasonality data
}
  \item{adata}{
The historical seasonal (annual, summer, winter or quarterly) demographic and economic data
}
  \item{hhoptformula}{
The optimal formula for each half-hourly/hourly demand model
}
  \item{aoptformula}{
The optimal formula for seasonal demand model
}
  \item{periods}{
The periods within a day, choose between 48 (half-hourly data) and 24 (hourly data)
}
}
\details{
Estimate the demand model using the historical data, use additive model for half-hourly/hourly demand and linear model for seasonal demand, log demand is used for half-hourly/hourly model.
}
\value{
\item{hh }{half-hourly/hourly demand models}
\item{hhfits }{fitted values of half-hourly/hourly models}
\item{hhres }{half-hourly/hourly model residuals}
\item{a }{seasonal model}
\item{afits }{fitted values of seasonal model}
\item{fits }{fitted values of the entire model}
\item{res }{entire model residuals}
}
\references{
R. J. Hyndman and S. Fan, "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, Vol. 25, No.2, pp. 1142--1153, 2010
}
\author{
R. J. Hyndman and S. Fan
}
\seealso{
\code{\link{simulate_ddemand}}
\code{\link{simulate_demand}}
\code{\link{obj}}
\code{\link{obj.econ}}
}
\examples{
# optimal formula for half-hourly model, to be given by the user
formula.hh <- list()
for(i in 1:48)
	formula.hh[[i]] = as.formula(log(ddemand) ~ ns(temp, df = d, k = 3) + ns(dtemp, 3) + day + holiday + ns(dayofseason, 9) + ns(avetemp, 3) + ns(lastmin, 
		3) + ns(prevtemp1, df = d, k = 3) + ns(prevtemp2, df = d,k = 3) + ns(prevtemp3, df = d, k = 3) + ns(prevtemp4, df = d,k = 3) + ns(day1temp, df = d, k = 3) + 
		ns(day2temp, df = d,k = 3) + ns(day3temp, df = d, k = 3) + ns(prevdtemp1, 3) + ns(prevdtemp2, 3) + ns(prevdtemp3, 3) + ns(day1dtemp, 3))

# optimal formula for annual model, to be given by the user
formula.a <- as.formula("anndemand~gsp+ddays+resiprice")

obj.model <- demand_model(obj,obj.econ,formula.hh,formula.a)
summary(obj.model)
}
\keyword{Electricity demand model}
\keyword{Demand model}

