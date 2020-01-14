#' Read effluent chart data from EPA ECHO webservices
#'
#' This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters. All arguments are optional except p_id. At least one p_id must be specified.
#' @param p_id Permitted facility ID. Either a single text value (in quotes) or a vector of text values.
#' @param parameter_code Parameter code. Either a single text value (in quotes) or a vector of text values.
#' @param start_date Query start date in "mm/dd/yyyy" format.
#' @param end_date Query end date in "mm/dd/yyyy" format.
#' @param stringsAsFactors Logical. Passed to read.csv. See ?read.csv for more information.
#' @param print Logical. If TRUE (default), print summary table of facilities & parameters returned.
#' @param ... additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data reads.
#' @return A flat data frame of EPA ECHO effluent chart data
#' @importFrom plyr ldply
#' @examples
#' #Extract effluent chart data for facility UT0025241, all outfalls
#' UT0025241_ec=readECHO_ec(p_id="UT0025241", start_date="01/01/2010", end_date="01/15/2019")
#' head(UT0025241_ec)
#' 
#' # Extract effluent total phosphorus data from outfall 001 for facility UT0025241 
#' UT0025241_tp_001=readECHO_ec(p_id="UT0025241", parameter_code="00665", outfall="001")
#' UT0025241_tp_001_effluent=UT0025241_tp_001[UT0025241_tp_001$monitoring_location_desc=="Effluent Gross",]
#' head(UT0025241_tp_001_effluent)
#' 
#' # Extract flow through facility from UT0021717
#' UT0021717_flow=readECHO_ec(p_id="UT0021717", parameter_code="50050")
#' 
#' # Extract flow & TP from UT0025241 & UT0021717
#' tp_flow=readECHO_ec(p_id=c("UT0025241","UT0021717"), parameter_code=c("50050","00665"))

#' @export
readECHO_ec<-function(..., print=TRUE, stringsAsFactors = default.stringsAsFactors()){
args=list(...)

base_path="https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?"

if(!any(names(args)=="p_id")){stop("Error: at least one p_id must be provided")}

if(any(names(args)=="start_date")){
	args$start_date=gsub("/","%2F",args$start_date)
	}
if(any(names(args)=="end_date")){
	args$end_date=gsub("/","%2F",args$end_date)
	}


for(n in 1:(length(args)-1)){
	if(n==1){args_mrg=merge(args[n],args[(n+1)])
	}else{args_mrg=merge(args_mrg,args[(n+1)])}
}

pastecollapse=function(x){paste0(names(x), "=", x, collapse="&")}
arg_paths=apply(args_mrg,1,'pastecollapse')
paths_all=paste0(base_path,arg_paths)

result=plyr::ldply(paths_all,read.csv, stringsAsFactors=stringsAsFactors, .progress="win")

if(print){
	print("Queried facility IDs and parameter counts:")
	print(table(result$npdes_id,result$parameter_desc))
}

return(result)

}









