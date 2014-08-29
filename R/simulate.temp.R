simulate.temp <-
function(x,y,n=nrow(x),m=9,temp_sites=2,delta=5,periods=48)
{
	if(m > 30)
		stop("Blockdays too large")
	if(m < delta)   #change to delta, the value of blockdays offset limit
		stop("Blockdays too small")
	m <- round(m) # Must be integer

	# different numbers of temperature sites
	if (temp_sites == 1)
	{
		xtemp <- x[,"temp1",drop=FALSE]  # one-column dataframe, use drop=FALSE to avoid coercing to vector
		# add in the residuals
		xtemp <- data.frame(temp1=x$temp1,hhres=y)
		newtemp <- data.frame(temp1=temp_bootstrap(xtemp,m,delta))
	}
	if (temp_sites == 2)
	{
		# add in the residuals
		xtemp <- data.frame(temp1=x$temp1,temp2=x$temp2,hhres=y)
	
		newtemp <- temp_bootstrap(xtemp,m,delta=delta)
	}
	#############################
   
	# Repeat if necessary
	nn <- nrow(xtemp)
	if(n > nn)
	{
		if((nn %% seasondays*periods) != 0)  # partial
		{
			fullyears <- trunc(nn/(seasondays*periods))
			nn <- seasondays*periods*fullyears
			if (temp_sites == 1)
				newtemp <- data.frame(temp1=newtemp[1:nn,])   # coerce vector to dataframe if there is one-column dataframe
			else
				newtemp <- newtemp[1:nn,]
		}
		nsamp <- trunc(n/nn) + 1
		for(i in 1:nsamp)
			if (temp_sites == 1)
				newtemp <- rbind(newtemp,data.frame(temp1=temp_bootstrap(xtemp,m,delta=delta)[1:nn,]))
			else
				newtemp <- rbind(newtemp,temp_bootstrap(xtemp,m,delta=delta)[1:nn,])
	}

	# Return result as a time series
	if (temp_sites == 1)
		newtemp <- ts(data.frame(temp1=newtemp[1:n,]),start=1,frequency=seasondays*periods)
	else
		newtemp <- ts(newtemp[1:n,],start=1,frequency=seasondays*periods)
	return(newtemp)
}
