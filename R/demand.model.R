demand.model <-
function(hhdata,adata,hhoptformula,aoptformula,periods=48)
{
	hhmodels <- list()
   
	# half-hourly model
	for(i in 1:periods)
		hhmodels[[i]] <- lm(hhoptformula[[i]],data=hhdata[hhdata$timeofday==(i-1),],na.action=na.exclude)

	# Compute hh fits and residuals
	fits <- matrix(NA,nrow(hhdata)/periods,periods)
	for(i in 1:periods)
		fits[,i] <- fitted(hhmodels[[i]])
	fits <- c(t(fits))
	hhfits <- fits
	hhres <- ts(log(hhdata$ddemand) - fits,f=seasondays*periods,s=hhdata$Year[1])
	fits <- exp(fits)
	
	# annual model
	amodel <- lm(aoptformula, data=adata)

	# Compute annual fits and residuals
	afits <- fitted(amodel)
   
	# Compute fitted values and residuals
	fits <- ts(fits * rep(afits,c(table(hhdata$fyear))),f=seasondays*periods,s=hhdata$Year[1])
	res <- ts(hhdata$demand - fits,f=seasondays*periods,s=hhdata$Year[1])
   
	return(list(hh=hhmodels,hhfits=hhfits,hhres=hhres,a=amodel,afits=afits,fits=fits,res=res))
}
