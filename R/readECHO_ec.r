#' Read effluent chart data from EPA ECHO webservices
#'
#' This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters. All arguments are optional except p_id. At least one p_id must be specified.
#' @param p_id Permitted facility ID. Either a single text value (in quotes) or a vector of text values.
#' @param parameter_code Parameter code. Either a single text value (in quotes) or a vector of text values.
#' @param start_date Query start date in "mm/dd/yyyy" format.
#' @param end_date Query end date in "mm/dd/yyyy" format.
#' @param ... additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data reads. Note that arguments for output are ignored.
#' @return A flat data frame of EPA ECHO effluent chart data
#' @importFrom plyr ldply
#' @examples
#' #Extract effluent chart data for facility UT0025241, all outfalls
#' UT0025241_ec=readECHO_ec(type="ec",p_id="UT0025241")
#' head(UT0025241_ec)
#' 
#' #Extract effluent total phosphorus data from outfall 001 for facility UT0025241 (note that monitoring_location_desc is not an available argument for download_effluent_chart ECHO web services)
#' UT0025241_tp_001=readECHO_ec(p_id="UT0025241", parameter_code="00665", outfall="001")
#' UT0025241_tp_001_effluent=UT0025241_tp_001[UT0025241_tp_001$monitoring_location_desc=="Effluent Gross",]
#' head(UT0025241_tp_001_effluent)
#' 
#' #Extract flow through facility from UT0021717
#' UT0021717_flow=readECHO_ec(p_id="UT0021717", parameter_code="50050")
#' ggplot2::qplot(as.Date(monitoring_period_end_date, '%m/%d/%Y'),dmr_value_standard_units,data=UT0021717_flow[UT0021717_flow$monitoring_location_desc=="Effluent Gross",],color=statistical_base_short_desc, ylab="Flow (mgd)", xlab="Date") +ggplot2::theme_bw()
#' 
#' #Extract flow & TP from UT0025241 & UT0021717
#' tp_flow=readECHO_ec(p_id=c("UT0025241","UT0021717"), parameter_code=c("50050","00665"))

#' @export
readECHO_ec<-function(...){
args=list(...)

base_path="https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?"

if(!any(names(args)=="p_id")){stop("Error: at least one p_id must be provided")}

if(any(names(args)=="start_date")){
	args$start_date=gsub("/","%2F",args$start_date)
	}
if(any(names(args)=="end_date")){
	args$end_date=gsub("/","%2F",args$end_date)
	}

args$output="CSV"

for(n in 1:(length(args)-1)){
	if(n==1){args_mrg=merge(args[n],args[(n+1)])
	}else{args_mrg=merge(args_mrg,args[(n+1)])}
}

pastecollapse=function(x){paste0(names(x), "=", x, collapse="&")}
arg_paths=apply(args_mrg,1,'pastecollapse')
paths_all=paste0(base_path,arg_paths)

result=plyr::ldply(paths_all,read.csv,.progress="win")
print("Queried facility IDs and parameter counts:")
print(table(result$npdes_id,result$parameter_desc))

return(result)

}









