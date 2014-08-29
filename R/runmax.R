runmax <-
function(x, k)
{
	n <- length(x)
	y <- rep(NA, n)
	a <- y[k] <- max(x[1:k])
	for (i in (k+1):n)
	{
		# there are empty values in temperature file, cause errors when excuting 'if'
		y[i] <- max(x[i - 0:(k-1)], na.rm=TRUE) # calculate max of the window
	}
	return(y)
}
