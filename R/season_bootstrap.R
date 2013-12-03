season_bootstrap <-
function(x,m,periods=48)
{
	# Arrange data in a matrix with one block per row
	n <- length(x)
	nblocks <- trunc(n/periods/m)
	n <- periods*nblocks*m
	xmat <- matrix(x[1:n],ncol=m*periods,nrow=nblocks,byrow=TRUE)

	# Permute rows
	j <- sample(1:nblocks,nblocks,replace=TRUE)

	# Return as vector
	newx <- c(t(xmat[j,]))
	return(na.omit(newx))
}
