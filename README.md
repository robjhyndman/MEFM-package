#MEFM package

The R package *MEFM* includes a set of tools for implementing the Monash Electricity Forecasting Model based on the paper by [Hyndman and Fan (2010)](http://robjhyndman.com/papers/peak-electricity-demand/).

The package requires the following data as input: half-hourly/hourly electricity demands; half-hourly/hourly temperatures at one or two locations;
seasonal demographical and economical data; public holiday data. The formats of the required data are described in the help files.

Some documentation of the underlying model is provided at [robjhyndman.com/publications/mefm/](https://robjhyndman.com/publications/mefm/)

## Installation

You can install the latest version from
[Github](https://github.com/robjhyndman/MEFM-package)

```s
# install.packages("devtools")
library(devtools)
install_github("robjhyndman/MEFM-package") 
```

## Usage

```s
library(MEFM)

formula.hh <- list()
for(i in 1:48)
  formula.hh[[i]] = as.formula(log(ddemand) ~ ns(temp, df=2) + day 
    + holiday + ns(timeofyear, 9) + ns(avetemp, 3) + ns(dtemp, 3) + ns(lastmin, 3) 
    + ns(prevtemp1, df=2) + ns(prevtemp2, df=2) 
    + ns(prevtemp3, df=2) + ns(prevtemp4, df=2) 
    + ns(day1temp, df=2) + ns(day2temp, df=2) 
    + ns(day3temp, df=2) + ns(prevdtemp1, 3) + ns(prevdtemp2, 3) 
    + ns(prevdtemp3, 3) + ns(day1dtemp, 3))

# formula for annual model, to be given by the user
formula.a <- as.formula(anndemand ~ gsp + ddays + resiprice)

# create lagged temperature variables
sa <- maketemps(sa,2,48)

sa.model <- demand_model(sa, sa.econ, formula.hh, formula.a)

summary(sa.model$a)
summary(sa.model$hh[[33]]) 

# Simulate future normalized half-hourly data
simdemand <- simulate_ddemand(sa.model, sa, simyears=10)

# seasonal economic and weather forecast, to be given by user
afcast <- data.frame(pop=1694, gsp=22573, resiprice=34.65, ddays=642)

# Simulate half-hourly data
demand <- simulate_demand(simdemand, afcast)

# Illustrate the results
plot(density(demand$annmax, bw="SJ"),
  main="Density of seasonal maximum demand", xlab="Demand")
```

## License

This package is free and open source software, licensed under GPL (>= 2).
