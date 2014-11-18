simulate_demand <-
function(sim,afcast, nyears=length(sim$hhfit)/seasondays/periods, periods=48)
{
	n <- nyears*seasondays*periods

	# forecast demand difference of annual average demand
	afit <- predict(sim$a,newdata=afcast,se=TRUE)
	avar <- afit$se^2 + summary(sim$a)$sigma^2
	# rnorm
	afit <- rep(rnorm(nyears,afit$fit,sqrt(avar)),each=seasondays*periods)

	##############################################################
	# total below should equal to sim$demand
	total <- sim$hhfit+sim$hhres
   
	# use log for both annual and half-hourly sim
	dem <- exp(total[1:n]) * afit
   
	annmax <- blockstat(dem,seasondays,max,fill=FALSE)
	return(list(demand=ts(dem,frequency=seasondays*periods,start=1),annmax=ts(annmax,frequency=1,start=1)))
}
