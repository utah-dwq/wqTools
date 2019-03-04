#' Read EPA Water Quality Portal Data
#'
#' This function extracts water quality data from EPA's Water Quality Portal based on user arguemnt inputs. Note that connections to the WQP occassionally time out during download. This function tries to download requested files up to 10 times before exiting.
#' All arguments except type are optional, but at least one should be provided to limit download size and prevent errors connecting to WQP.
#' Note that some, but not all, special characters in characteristic names have been accounted if. If in doubt, use the WQP web interface to determine the appropriate sytax for odd characteristic names.
#' This function coerces non-numeric values in ResultMeasureValue column (for result & narrowresult type queries). This may generate NA values with a warning for special characters.
#' @param type Data type to read. One of "result", "narrowresult", "sites", "activity", or "detquantlim".
#' @param start_date Query start date in "mm/dd/yyyy" format.
#' @param end_date Query end date in "mm/dd/yyyy" format.
#' @param coerce_num Logical. If TRUE the ResultMeasureValue column in result and narrowresult type reads is coerced to numeric values. This will generate NAs in the ResultMeasureValue column for non-numeric values. Defaults to FALSE.
#' @param ... additional arguments to be passed to WQP query path. See https://www.waterqualitydata.us/portal/ for optional arguments.
#' @param print Logical. Print summary table of sites & characteristics (only for result or narrowresult types).
#' @return A data frame of WQP data
#' @examples
#' # Read some data from Mantua Reservoir (2016-2018)
#' nr=readWQP(type="narrowresult", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"),
#' 		  start_date="01/01/2016", end_date="12/31/2018")
#' 
#' # Read just Arsenic, Cadmium, and DO, all dates
#' nr=readWQP(type="narrowresult",
#' 		  siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"),
#' 		  characteristicName=c("Arsenic","Cadmium","Dissolved oxygen (DO)"))
#' 
#' # Read all Total dissolved solids statewide (2016-2018) (& note statecode for Utah)
#' tds_sw=readWQP(type="result",
#' 			  statecode="US:49",
#' 			  characteristicName="Total dissolved solids",
#' 			  start_date="01/01/2016", end_date="12/31/2018",
#' 			  print=F)
#' 
#' # Read DWQ's sites
#' sites=readWQP(type="sites", statecode="US:49", organization="UTAHDWQ_WQX", siteType=c("Lake, Reservoir, Impoundment","Stream"))
#' plot(LatitudeMeasure~LongitudeMeasure, sites[sites$LatitudeMeasure>0 & sites$LongitudeMeasure<0,])

#' @export
readWQP<-function(type="result", ..., print=TRUE, coerce_num=FALSE){
args=list(...)

#type="narrowresult"
##statecode="US:49"
##characteristicName=c("Total dissolved solids","Arsenic","Cadmium")
#siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470")
##start_date="01/01/2015"
##end_date="12/31/2018"
#args=list(siteid=siteid)

pastecollapse=function(x){
	return(paste0(names(x), "=", x, collapse="&"))
}

if(any(names(args)=="start_date")){
	args$startDateLo=format(as.Date(args$start_date, format='%m/%d/%Y'), format="%m-%d-%Y")
	args=args[names(args)!="start_date"]
}

if(any(names(args)=="end_date")){
	args$startDateHi=format(as.Date(args$end_date, format='%m/%d/%Y'), format="%m-%d-%Y")
	args=args[names(args)!="end_date"]
}


args$mimeType="csv"
args$zip="no"


if(type=="result" | type=="narrowresult"){base_path="https://www.waterqualitydata.us/data/Result/search?"}
if(type=="narrowresult"){args$dataProfile="narrowResult"}
if(type=="sites"){base_path="https://www.waterqualitydata.us/data/Station/search?"}
if(type=="activity"){
	base_path="https://www.waterqualitydata.us/data/Activity/search?"
	args$dataProfile="activityAll"
	}
if(type=="detquantlim"){base_path="https://www.waterqualitydata.us/data/ResultDetectionQuantitationLimit/search?"}

for(n in 1:length(args)){
	name=names(args)[n]
	x=unlist(args[n])
	names(x)=rep(name,length(x))
	args[n]=pastecollapse(x)
}


names(args)=NULL

arg_path=paste0(args,collapse="&")

path=paste0(base_path,arg_path)

path=gsub("US:", "US%3A", path)
path=gsub(" ", "%20", path)
path=gsub(",", "%2C", path)

n=1
while(!exists('result',inherits=F) & n<=10){
	n=n+1
	try({
		suppressWarnings({result=plyr::ldply(path,.fun=read.csv, na.strings=c(""," ","NA"),.progress="text")})
	})
}

if(print & exists('result',inherits=F) & (type=="result" | type=="narrowresult")){
	print("Queried sites and parameters:")
	print(table(result$MonitoringLocationIdentifier, result$CharacteristicName))
}
if(!exists('result',inherits=F)){stop("Error - unable to download data - no data available for selected inputs or invalid query.")}


if((type=="result" | type=="narrowresult") & class(result$ResultMeasureValue)!="numeric" & class(result$ResultMeasureValue)!="integer" & coerce_num){
	ResultMeasureValue=as.character(result$ResultMeasureValue)
	ResultMeasureValue=gsub(",","",ResultMeasureValue)
	ResultMeasureValue[ResultMeasureValue=="" | ResultMeasureValue==" "]="NA"
	rmv_num=as.numeric(ResultMeasureValue)
	result$ResultMeasureValue=rmv_num
	}

return(result)

}

