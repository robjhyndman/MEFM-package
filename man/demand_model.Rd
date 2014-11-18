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
demand_model(hhdata, adata, hhoptformula, aoptformula, periods=48)
}
\arguments{
  \item{hhdata}{
The historical half-hourly/hourly demand, temperature and seasonality data
}
  \item{adata}{
The historical seasonal (annual, summer, winter or quarterly) demographic and economic data
}
  \item{hhoptformula}{
The formula for each half-hourly/hourly demand model
}
  \item{aoptformula}{
The formula for seasonal demand model
}
  \item{periods}{
The periods within a day, choose between 48 (half-hourly data) and 24 (hourly data)
}
}
\details{
Estimate the demand model using the historical data, use additive model for half-hourly/hourly demand and linear model for seasonal demand, log demand is used for half-hourly/hourly model.
}
\value{
\item{hh}{half-hourly/hourly demand models}
\item{hhfits}{fitted values of half-hourly/hourly models}
\item{hhres}{half-hourly/hourly model residuals}
\item{a}{seasonal model}
\item{afits}{fitted values of seasonal model}
\item{fits}{fitted values of the entire model}
\item{res}{entire model residuals}
}
\references{
R. J. Hyndman and S. Fan (2010) "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, 25(2), 1142--1153.
}
\author{
Rob J Hyndman and Shu Fan
}
\seealso{
\code{\link{simulate_ddemand}},
\code{\link{simulate_demand}},
\code{\link{sa}},
\code{\link{sa.econ}}
}
\examples{
# formula for half-hourly model, to be given by the user
formula.hh <- list()
for(i in 1:48)
	formula.hh[[i]] = as.formula(log(ddemand) ~ ns(temp, df=2, k=3) + day 
    + holiday + ns(dayofseason, 9) + ns(avetemp, 3) + ns(dtemp, 3) + ns(lastmin, 3) 
    + ns(prevtemp1, df=2, k=3) + ns(prevtemp2, df=2, k=3) 
    + ns(prevtemp3, df=2, k=3) + ns(prevtemp4, df=2, k=3) 
    + ns(day1temp, df=2, k=3) + ns(day2temp, df=2, k=3) 
    + ns(day3temp, df=2, k=3) + ns(prevdtemp1, 3) + ns(prevdtemp2, 3) 
    + ns(prevdtemp3, 3) + ns(day1dtemp, 3))

# formula for annual model, to be given by the user
formula.a <- as.formula(anndemand ~ gsp + ddays + resiprice)

sa.model <- demand_model(sa, sa.econ, formula.hh, formula.a)

summary(sa.model$a)
summary(sa.model$hh[[33]]) 
# Problems with model fit ????
}

\keyword{Electricity demand model}
\keyword{Demand model}

