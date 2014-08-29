blockstat <-
function(x,m=14,FUN=max,...,fill=FALSE,periods=48)
{
	# fill the partial block with data from the same period in the past year, if the fill flag is TRUE
	if (fill==TRUE & trunc(length(x)/periods/seasondays+0.9999)!=trunc(length(x)/periods/seasondays))
	{
		n_days <- length(x)/periods
		n_days_w = trunc(n_days/seasondays + 0.9999) * seasondays

		x[(n_days*periods+1):(n_days_w*periods)] <- x[(n_days*periods+1):(n_days_w*periods) - seasondays]
	}
	# Arrange data in a matrix with one block per row
	if(m == 7)   # when calculating weekly index, there may be partial week to be removed at the end of the year
	{
		nbyear <- trunc(seasondays/m)
		nyears <- trunc(length(x)/periods/seasondays)
		# remove the partial week at the end of each year if existed
		if(seasondays-nbyear*m)
		{
			tmp <- rep(((nbyear*m+1):seasondays),nyears) + rep((0:(nyears-1))*seasondays,each=(seasondays-nbyear*m))
			x <- x[-(rep((tmp-1)*periods,each=periods) + rep(1:periods,length(tmp)))]
		}
		nblocks <- trunc(length(x)/periods/m)
		n <- periods*nblocks*m
	}
	else
	{
		nblocks <- trunc(length(x)/periods/m + 0.99999)
		n <- periods*nblocks*m
	}
   
	xmat <- matrix(x[1:n],ncol=m*periods,nrow=nblocks,byrow=TRUE)
	return(apply(xmat,1,FUN,na.rm=TRUE,...))
}
