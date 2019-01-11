#' Read effluent chart data from EPA ECHO webservices
#'
#' This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters 
#' @param ... additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data reads. Note that arguments for output are ignored.
#' @return A flat data frame of EPA ECHO effluent chart data

#' @export
readECHO_ec<-function(...){
args=list(...)

base_path="https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart"

p_id=c("UT0021717","UT0025241")
parameter_code=c("00665","50050")
start_date="01/01/2015"
end_date="12/31/2016"
#end_date=12%2F31%2F2016
args=list(p_id=p_id, start_date=start_date, end_date=end_date,parameter_code=parameter_code)
args=list(p_id=p_id, start_date=start_date, end_date=end_date)

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








}









#' @examples
#' #Extract effluent chart data for facility UT0025241, all outfalls
#' UT0025241_ec=readECHO(type="ec",p_id="UT0025241")
#' head(UT0025241_ec)
#' 
#' #Available parameters
#' unique(data.frame(UT0025241_ec[,c("parameter_code","parameter_desc")]))
#' 
#' #Extract effluent total phosphorus data from outfall 001 for facility UT0025241 (note that monitoring_location_desc is not an available argument for download_effluent_chart ECHO web services)
#' UT0025241_tp_001=readECHO(type="ec",p_id="UT0025241", parameter_code="00665", outfall="001")
#' UT0025241_tp_001_effluent=UT0025241_tp_001[UT0025241_tp_001$monitoring_location_desc=="Effluent Gross",]
#' head(UT0025241_tp_001_effluent)
#' 
#' #Extract flow through facility from UT0021717
#' UT0021717_flow=readECHO(type="ec",p_id="UT0021717", parameter_code="50050")
#' head(UT0021717_flow)
#' ggplot2::qplot(as.Date(monitoring_period_end_date, '%m/%d/%Y'),dmr_value_standard_units,data=UT0021717_flow[UT0021717_flow$monitoring_location_desc=="Effluent Gross",],color=statistical_base_short_desc, ylab="Flow (mgd)", xlab="Date") +ggplot2::theme_bw()
