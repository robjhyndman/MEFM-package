runmin <-
function(x, k)
{
	n <- length(x)
	y <- rep(NA, n)
	a <- y[k] <- min(x[1:k])
	for (i in (k+1):n)
	{
		y[i] <- min(x[i - 0:(k-1)],na.rm=TRUE) # calculate min of the window
	}
	return(y)
}
