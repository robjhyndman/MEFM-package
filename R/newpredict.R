newpredict <-
function(model,hdata,blocklength,allperiods=TRUE,delta,periods=48)
{
	# check if there is the second temperature sites
	temp_sites = 1
	if (is.element("temp2",colnames(hdata)))
		temp_sites = 2

		#############################################################################################
		# simulate temperature and residual simultaneously
		newsa<- simulate.temp(hdata,model$hhres,n=nrow(hdata)+seasondays*periods,m=20,temp_sites,delta=delta,periods=periods)
       
		hres <- newsa[,"hhres"]
		hres <- hres[-(1:(seasondays*periods))]
		newsa <- newsa[,c("temp1","temp2")]
       
		##################################################################### 
		# store the simulated temperature
		simtemp <- 0.5*(newsa[,"temp1"]+newsa[,"temp2"])
		simtemp <- simtemp[-(1:(seasondays*periods))]
		#####################################################################

		newsa <- maketemps(newsa,temp_sites,periods=periods)
		# remove the first seasondays*periods (due to the NAs in the first several days)
		newsa <- newsa[-(1:(seasondays*periods)),]
		# Patch in other variables
		bak <- hdata
		hdata[,colnames(newsa)] <- newsa
		newsa <- hdata
		hdata <- bak

		# Half-hourly model predictions
		hfits <- matrix(-10,nrow(hdata)/periods,periods)
		if(allperiods)
			j <- 1:periods
		else
			j <- 16:periods
		for(i in j)
				hfits[,i] <- predict(model$hh[[i]],newdata=newsa[newsa$timeofday==(i-1),])

		# Vectorize
		hfits <- c(t(hfits))

	# Half hourly residuals (done with the temperature simulation)
	hres <- simulate.res(hdata,model,blocklength,hfits,periods=periods) # still use the original residual simulation

	return(list(hfit=hfits, hres=hres, simtemp=simtemp))
}
