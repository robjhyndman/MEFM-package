season.bootstrap <-
function(x,m,n=length(x))
{
	tmp <- season_bootstrap(x,m)
	if(n > length(tmp))
	{
		ns <- trunc(n / length(x)) + 1
		for(i in 2:ns)
			tmp <- c(tmp,season_bootstrap(x,m))
	}
	return(ts(tmp[1:n],s=start(x),f=frequency(x)))
}
