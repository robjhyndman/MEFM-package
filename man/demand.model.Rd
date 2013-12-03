\name{demand.model}
\alias{demand.model}
%\alias{simulate.ddemand}
%\alias{simulate.demand}
\title{
The half-hourly/hourly and seasonal electricity demand models
}
\description{
Generate the demand model, using additive model for half-hourly/hourly demand and linear model for seasonal demand, log demand is used for half-hourly/hourly model.
}
\usage{
demand.model(hhdata, adata, hhoptformula, aoptformula, periods = 48)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hhdata}{
The historical half-hourly/hourly demand, temperature and seasonality data
}
  \item{adata}{
The historical seasonal demographic and economic data
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{simulate.ddemand}}
\code{\link{simulate.demand}}
\code{\link{obj}}
\code{\link{obj.econ}}
}
\examples{
obj.model <- demand.model(obj,obj.econ,optformula,optformula.a)
summary(obj.model)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Electricity demand model}
\keyword{Demand model}

