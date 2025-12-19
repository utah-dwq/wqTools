#' Calculate projected maximum concentration for reasonable potential analysis
#'
#' Calculate the projected maximum effluent concentration from a set of effluent samples for reasonable potential analysis.
#'
#' @param x Vector of effluent concentrations. Any non-detects should be set to zero.
#' @param dist Distributional method to be used in calculations. Currently only 'lognormal' is supported.
#' @param confidence Confidence interval. One of 95 or 99.
#' @examples 
#' effluent_concentrations=c(0.022,0.018,0.0105,0.0167,0.0154,0.0113,0.0108,0.0136,0.0194,0.0225,0.0165,0.0118,0.0187,0.0127,0.0103,0.0104,0.0126,0.0148,0.0091,0.01)
#' projectMax(effluent_concentrations,  dist="lognormal", confidence=95)
#'

#' @export
projectMax=function(x, dist="lognormal", confidence=95){
	
	if(!confidence %in% c(95,99)){error("Error: confidence argument must be 95 or 99.")}
	if(!dist %in% c("lognormal")){error("Error: only dist='lognormal' currently supported.")}
	if(dist=="lognormal"){x_dist=log(x)}
	
	n_count=length(x)
	mean=mean(x_dist)
	squares=(x_dist-mean)^2
	var=sum(squares)/(n_count-1)
	daily_avg=exp((mean+var)/2)
	cv=(exp(var)-1)^0.5	
	z_value=qnorm((1-confidence/100)^(1/n_count))
	if(confidence==95){zp=1.645}
	if(confidence==99){zp=2.326}	
	rp_multiplier=(exp((zp*sqrt(log(cv^2+1)))-0.5*log(cv^2+1)))/(exp((z_value*sqrt(log(cv^2+1)))-0.5*log(cv^2+1)))
	#pmax=signif(rp_multiplier*max(x),sig_figs)
	pmax=rp_multiplier*max(x)
	
	return(pmax)
}

