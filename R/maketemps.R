maketemps <-
function(x,temp_sites,periods=48)
{
	colnames.x <- colnames(x)
	n = nrow(x)
	
	# single temperature site
	if (temp_sites == 1)
	{
		x <- as.data.frame(cbind(x,matrix(NA,nrow=nrow(x),ncol=16)))
		colnames(x) <- c(colnames.x,"temp",
			"prevtemp1","prevtemp2","prevtemp3","prevtemp4","prevtemp5","prevtemp6",
			"day1temp","day2temp","day3temp","day4temp","day5temp","day6temp",
			"lastmax","lastmin","avetemp")
		x$temp <- x$temp1
	}
	
	# standard 2 temperature sites
	if (temp_sites == 2)
	{
		x <- as.data.frame(cbind(x,matrix(NA,nrow=nrow(x),ncol=29)))
		colnames(x) <- c(colnames.x,"temp","dtemp",
			"prevtemp1","prevtemp2","prevtemp3","prevtemp4","prevtemp5","prevtemp6",
			"day1temp","day2temp","day3temp","day4temp","day5temp","day6temp",
			"prevdtemp1","prevdtemp2","prevdtemp3","prevdtemp4","prevdtemp5","prevdtemp6",
			"day1dtemp","day2dtemp","day3dtemp","day4dtemp","day5dtemp","day6dtemp",
			"lastmax","lastmin","avetemp")

		# Create ave temp and difference temp
		x$temp <- 0.5*(x$temp1+x$temp2)
		x$dtemp <- x$temp2 - x$temp1

		x$prevdtemp1 <-  c(rep(NA,1),x$dtemp[1:(n-1)])
		x$prevdtemp2 <-  c(rep(NA,2),x$dtemp[1:(n-2)])
		x$prevdtemp3 <-  c(rep(NA,3),x$dtemp[1:(n-3)])
		x$prevdtemp4 <-  c(rep(NA,4),x$dtemp[1:(n-4)])
		x$prevdtemp5 <-  c(rep(NA,5),x$dtemp[1:(n-5)])
		x$prevdtemp6 <-  c(rep(NA,6),x$dtemp[1:(n-6)])
		x$day1dtemp <- c(rep(NA,periods),x$dtemp[1:(n-periods)])
		x$day2dtemp <- c(rep(NA,2*periods),x$dtemp[1:(n-2*periods)])
		x$day3dtemp <- c(rep(NA,3*periods),x$dtemp[1:(n-3*periods)])
		x$day4dtemp <- c(rep(NA,4*periods),x$dtemp[1:(n-4*periods)])
		x$day5dtemp <- c(rep(NA,5*periods),x$dtemp[1:(n-5*periods)])
		x$day6dtemp <- c(rep(NA,6*periods),x$dtemp[1:(n-6*periods)])
	}

	# Create lagged versions of temp variables
	x$prevtemp1 <-  c(rep(NA,1),x$temp[1:(n-1)])
	x$prevtemp2 <-  c(rep(NA,2),x$temp[1:(n-2)])
	x$prevtemp3 <-  c(rep(NA,3),x$temp[1:(n-3)])
	x$prevtemp4 <-  c(rep(NA,4),x$temp[1:(n-4)])
	x$prevtemp5 <-  c(rep(NA,5),x$temp[1:(n-5)])
	x$prevtemp6 <-  c(rep(NA,6),x$temp[1:(n-6)])
	x$day1temp <- c(rep(NA,periods),x$temp[1:(n-periods)])
	x$day2temp <- c(rep(NA,2*periods),x$temp[1:(n-2*periods)])
	x$day3temp <- c(rep(NA,3*periods),x$temp[1:(n-3*periods)])
	x$day4temp <- c(rep(NA,4*periods),x$temp[1:(n-4*periods)])
	x$day5temp <- c(rep(NA,5*periods),x$temp[1:(n-5*periods)])
	x$day6temp <- c(rep(NA,6*periods),x$temp[1:(n-6*periods)])

#    x$yesterdaymax <- rep(c(NA,daily(x$temp,max)),each=periods)[1:n]
	x$lastmax <- runmax(na.interp(x$temp),periods)
	x$lastmin <- runmin(na.interp(x$temp),periods)
	x$avetemp <- runmean(na.interp(x$temp),periods)

	return(x)
}
