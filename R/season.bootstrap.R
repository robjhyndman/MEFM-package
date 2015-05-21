season.bootstrap <-
function(x,m,n=length(x),periods=48)
{
	tmp <- season_bootstrap(x,m,periods)
	if(n > length(tmp)){
		ns <- trunc(n / length(x)) + 1
		for(i in 2:ns)
			tmp <- c(tmp,season_bootstrap(x,m,periods))
	}
	return(ts(tmp[1:n],start=start(x),frequency=frequency(x)))
}
