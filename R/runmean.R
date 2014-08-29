runmean <-
function(x, k)
{
	n <- length(x)
	y <- x[ k:n ] - x[ c(1,1:(n-k)) ]
	# this is a difference from the previous cell
	y[1] <- sum(x[1:k],na.rm=T) # find the first sum
	y <- cumsum(y)/k    # apply precomputed differences
	return(c(rep(NA,k-1),y))
}
