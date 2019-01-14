#' Read effluent chart data from EPA ECHO webservices
#'
#' This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters 
#' @param p_id Permitted facility ID. Either a single text value (in quotes) or a vector of text values.
#' @param parameter_code Parameter code. Either a single text value (in quotes) or a vector of text values.
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

base_path="https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart"

if(!any(names(args)=="p_id")){stop("Error: at least one p_id must be provided")}

if(any(names(args)=="start_date")){
	args$start_date=gsub("/","%2F",args$start_date)
	}
if(any(names(args)=="end_date")){
	args$end_date=gsub("/","%2F",args$end_date)
	}

args$output="CSV"


if(any(names(args)=="parameter_code")){
	args_all=list()
	id_pc=merge(args$p_id, args$parameter_code)
	for(n in 1:dim(id_pc)[1]){
		args_n=args[names(args)!="p_id" & names(args)!="parameter_code"]
		args_n$p_id=id_pc[n,1]
		args_n$parameter_code=id_pc[n,2]
		#print(n)
		#print(args_n)
		args_all=append(args_all,list(args_n))
	}
}else{
	args_all=list()
	for(n in 1:length(args$p_id)){
		args_n=args[names(args)!="p_id"]
		args_n$p_id=args$p_id[n]
		args_all=append(args_all,list(args_n))
	}
}

length(args_all)

paths_all=list()
for(j in 1:length(args_all)){
	path=paste0(base_path, "?")
	for(n in 1:length(args_all[[j]])){
		for(i in 1:length(unlist(args_all[[j]][n]))){
			arg_ni=paste0(names(args_all[[j]][n]),"=",unlist(args_all[[j]][n])[i],"&")
			path=paste0(path,arg_ni)
		}
	}
	path_j=gsub('.{1}$', '', path)
	paths_all=append(paths_all, list(path_j))
}


result=plyr::ldply(paths_all,read.csv,.progress="text")
print("Queried facility IDs and parameter counts:")
print(table(result$npdes_id,result$parameter_desc))

return(result)

}









