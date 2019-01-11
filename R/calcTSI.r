#' Calculate TSI values from input data
#'
#' This function calculates TSI values according to Utah's IR methods from input data containing values for of chlorophyll, total phosphorus, and secchi disk depth.
#' Note that inputs for these parameters must be specified in units of ug/L, mg/L, and meters, respectively.
#' @param x Input dataset
#' @param in_format One of "wide" or "flat" to specify data input format. Note that only wide format inputs are currently supported.
#' @param chl_ugL Name of chlorophyll-a variable in ug/L
#' @param TP_mgL Name of total phosphorus variable in mg/L
#' @param SD_m Name of secchi disk depth variable in m
#' @examples 
#' data(ul_trophic)
#' head(ul_trophic)
#' tsi=calcTSI(ul_trophic,chl_ugL="ChlA",TP_mgL="Phosphate.phosphorus.Total",SD_m="Depth.Secchi.disk.depth")
#' head(tsi)
#' plot(TSIchl~ChlA,tsi)

#' @export
calcTSI=function(x,in_format="wide",chl_ugL="chla",TP_mgL="TP",SD_m="SD"){
#default input units: chla (ug/L), TP (mg/L), SD (m)
	if(in_format=="wide"){
		TSIchl=9.81*log(x[,chl_ugL])+30.6
		TSItp=14.2*log(x[,TP_mgL]*1000)+4.15
		TSIsd=60-14.41*log(x[,SD_m])
		tsi=cbind(TSIchl,TSItp,TSIsd)
	}
	if(in_format=="flat"){
	stop("Only wide data inputs are currently supported.")
	#tsi=vector()
	#	for(n in 1:dim(x)[1]){
	#		val_n=x[n,val_column]
	#		type_n=x[n,type_column]
	#		if(type_n==chl){TSI_n=9.81*log(val_n)+30.6}
	#		if(type_n==TP){TSI_n=14.2*log(val_n*1000)+4.15}
	#		if(type_n==SD){TSI_n=60-14.41*log(val_n)}
	#		tsi=append(tsi,TSI_n)
	#	}
	}
	x_tsi=cbind(x,tsi)
	return(x_tsi)
}

