\name{obj.econ}
\alias{obj.econ}
\docType{data}
\title{
Seasonal historical demographic & economic 
}
\description{
Seasonal historical demographic & economic data, incluing population, GSP, residential electricity price and cooling/heating degree days 
}
\usage{data(obj.econ)}
\format{
  The format is:
 mts [1:14, 1:6] 1503 1509 1518 1528 1538 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:6] "pop" "gsp" "resiprice" "totalprice" ...
 - attr(*, "tsp")= num [1:3] 2000 2013 1
 - attr(*, "class")= chr [1:2] "mts" "ts"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
R. J. Hyndman and S. Fan, "Density Forecasting for Long-term Peak Electricity Demand", IEEE Trans. Power Systems, Vol. 25, No.2, pp. 1142--1153, 2010
}
\examples{
data(obj.econ)
## maybe str(obj.econ) ; plot(obj.econ) ...
}
\keyword{datasets}
