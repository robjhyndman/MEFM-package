temp_bootstrap <-
function(x,m,delta=5,periods=48)
{
	x <- as.matrix(x)
	n <- nrow(x)
	nyear <- seasondays*periods             # Number of observations per year
	years <- round(n/nyear + 0.499) # Total number of years for which there is some data
	index <- 1:n
	period <- rep(1:years,each=nyear)[1:n]

	# Arrange index in a matrix with one year per row
	# Matrix has more columns than necessary
	pad <- rep(NA,seasondays*periods*years - n)
	imat <- matrix(c(index,pad),ncol=seasondays*periods,nrow=years,byrow=TRUE)
	imat <- cbind(imat,imat)
	imat[,seasondays*periods+(1:seasondays*periods)] <- imat[c(2:years,years),seasondays*periods+(1:seasondays*periods)]
	imat <- imat[,1:((seasondays+50)*periods)]

	nbperyear <- round(seasondays/(m-delta) + 0.499)    # Maximum number of blocks per year
	# generate random series of block days
	# minimum days per block: m-delta; maximum: m+delta
	if(delta==0)
		bdaysrange <- rep(m,nbperyear)
	else
		bdaysrange <- sample((m-delta):(m+delta),nbperyear,replace=TRUE)

	# find the combination of block days series with sum equal to seasondays
	bsum <- cumsum(bdaysrange)
	nbperyear <- length(bsum[bsum < seasondays]) + 1   # Number of blocks per year
	bdaysrange[nbperyear] <- seasondays-bsum[nbperyear-1]  # last block (1~max)
	blengthseries <- periods * bdaysrange[1:nbperyear]   # block length series

	# generate the start index for each block
	blockstartbasic <- cumsum(c(1,blengthseries))[1:nbperyear]
	blockstart <- rep(blockstartbasic,years)
	if(delta>0)
	{
		for (i in 1:years)
		{
			startadj <- periods*sample((-delta):delta,nbperyear,replace=TRUE)    # using delta as adjustment range, can be changed later
			# first adjustment positive, last negative
			startadj[1] <-  abs(startadj[1])
			startadj[nbperyear] =  - abs(startadj[nbperyear])
			blockstart[(i-1)*nbperyear + (1:nbperyear)] <- blockstart[(i-1)*nbperyear + (1:nbperyear)] + startadj
		}
		blockstart <- pmax(blockstart,1)
	}

	# Generate random sample of years for each block
	nblocks <- years * nbperyear
	syear <- sample(1:years,nblocks,replace=TRUE)                  # Random year for each block.

	# Fix years with partial observations
	lastobs <- (n/seasondays/periods - trunc(n/seasondays/periods)) * periods * seasondays - (m+delta*2) * periods
	j <- (syear==years & blockstart>lastobs)
	syear[j] <- sample(1:(years-1),sum(j),replace=TRUE) # aviod to sample data from the blank part of the last year

	# Sample from each block
	newindex <- numeric(seasondays*periods*years)
	for(i in 1:years)
		for (j in 1:nbperyear)
			newindex[seasondays*periods*(i-1)+blockstartbasic[j]+(1:blengthseries[j])-1] <- imat[syear[(i-1)*nbperyear+j],blockstart[(i-1)*nbperyear+j]+(1:blengthseries[j])-1]

	# Remove missing observations (from partial blocks)
	newindex <- newindex[!is.na(newindex)]
	if(length(newindex) >= n)     #n <- nrow(x)
		newindex <- newindex[1:n]
	else{
		warning("Insufficient data generated")
		n <- length(newindex)
	}

	# Create new x matrix
	# Need to modify this so the noise is added to variable blocks instead of fixed blocks.
	newx <- as.matrix(x[newindex,])
	bmax <- blockstat(newx[,1],m,fill=FALSE)
	noise <- 0.4*pmax(bmax-42,0)*rnorm(length(bmax),0,1) + rnorm(length(bmax),0,0.2)
	noise <- matrix(rep(noise,rep(m*periods,length(noise)))[1:n],nrow=n,ncol=ncol(newx))
	newx <- newx + noise

	# Return final simulated data
	return(newx)
}
