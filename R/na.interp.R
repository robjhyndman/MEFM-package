na.interp <-
function(x)
{ 
	# interpolates missing values
	n <- length(x)
	nas <- is.na(x)
	idx <- (1:n)[!nas]
	xx <- as.ts(approx(idx,x[idx],1:n)$y)
	tsp(xx) <- tsp(x)
	return(xx)
}
