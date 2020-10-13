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
calcTSI=function(x,in_format="wide",chl_ugL="Chlorophyll a",TP_mgL="Phosphate-phosphorus",SD_m="Depth, Secchi disk depth", value_var='ResultMeasureValue', param_var='CharacteristicName'){
#x=trophic_data
#x=within(x, {
#	IR_Value[IR_Value == 0] = 0.001
#})
#any(x[,'IR_Value'] == 0, na.rm=T)
#chl_ugL="Chlorophyll a"
#TP_mgL="Phosphate-phosphorus"
#SD_m="Depth, Secchi disk depth"
#value_var='IR_Value'
#param_var='CharacteristicName'

#default input units: chla (ug/L), TP (mg/L), SD (m)
	if(in_format=="wide"){
		TSIchl=9.81*log(x[,chl_ugL])+30.6
		TSItp=14.2*log(x[,TP_mgL]*1000)+4.15
		TSIsd=60-14.41*log(x[,SD_m])
		tsi=cbind(x, TSIchl, TSItp, TSIsd)
	}
	if(in_format=="flat"){
		x$TSI=NA
		x$TSI[x[,param_var] == chl_ugL] = 9.81*log(x[x[,param_var]==chl_ugL,value_var])+30.6
		x$TSI[x[,param_var] == TP_mgL] =14.2*log(x[x[,param_var]==TP_mgL,value_var]*1000)+4.15
		x$TSI[x[,param_var] == SD_m] = 60-14.41*log(x[x[,param_var]==SD_m,value_var])
		tsi=x
	}
	return(tsi)
}

