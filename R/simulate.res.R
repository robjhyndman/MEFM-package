simulate.res <-
function(hdata,model,k,hfits,periods=48)
{
	###########################################################
	# normalize the model residuals, and then bootstrap, finally add back the average vlaues
	res.ave <- blockstat(model$hhres,k,median,fill=FALSE)	# median value of each 35 days
	resave.ar <- ar(res.ave,order.max=1)		# ar: Fit Autoregressive Models
	n <- length(res.ave)
	# resave.sim: Simulate from an ARIMA Model
	resave.sim <- arima.sim(list(ar=resave.ar$ar),n=n+20, innov=rnorm(n+20,0,sqrt(resave.ar$var.pred)))[-(1:20)]
   
	resave.sim <- ts(rep(resave.sim,each=k*periods)[1:length(model$hhres)])
	res.ave <- ts(rep(res.ave,each=k*periods)[1:length(model$hhres)])
	tsp(res.ave) <- tsp(resave.sim) <- tsp(model$hhres)
	hres <- season.bootstrap(model$hhres-res.ave,k)+resave.sim
	###########################################################
	
   # Bias adjustment
   # mean adjustment
	
	return(hres)
}
