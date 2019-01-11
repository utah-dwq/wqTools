#' Read data from EPA ECHO webservices
#'
#' This function extracts effluent chart or facility information from EPA ECHO based on input types 
#' @param type One of "ec" for effluent chart data or "fac" for facilitiy geometries. Argument for type is required.
#' @param ... additional arguments to be passed to ECHO query path. See for https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data and https://echo.epa.gov/tools/web-services/facility-search-water#!/Facility_Information/get_cwa_rest_services_get_facility_info for optional arguments for facilities. Note that arguments for output are ignored.
#' @return A flat data frame of EPA ECHO results for either effluent charts or facility information
#' @importFrom jsonlite fromJSON
#' @examples
#' #Extract facility locations for Utah
#' ut_fac=readECHO(type="fac",p_st="ut", p_act="y")
#' head(ut_fac)
#' 
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


#' @export
readECHO<-function(type="", ...){
args=list(...)

if(type!="ec" & type!="fac"){stop("Error: type must be one of 'ec' or 'fac'")}

path="https://ofmpub.epa.gov/echo/"

args$output="CSV"

if(type=="ec"){
	type_path="eff_rest_services.download_effluent_chart"
	args$output="CSV"
}
if(type=="fac"){
	type_path="cwa_rest_services.get_facility_info"
	args$output="JSON"
	}


if(any(names(args)=="parameter_code")){
	args$parameter_code=paste0(args$parameter_code,collapse="%2C")
}
	

path=paste0(path, type_path, "?")
for(n in 1:length(args)){
	for(i in 1:length(unlist(args[n]))){
		arg_ni=paste0(names(args[n]),"=",unlist(args[n])[i],"&")
		path=paste0(path,arg_ni)
	}
}
path=gsub('.{1}$', '', path)


if(type=="ec"){
	result=data.frame(read.csv(path, stringsAsFactors=F))
}else{
	fac_query=jsonlite::fromJSON(path)
	geoJSON_path=paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_geojson?output=GEOJSON&qid=",fac_query$Results$QueryID)
	fac_geoJSON=jsonlite::fromJSON(geoJSON_path)
	result=fac_geoJSON$features
}

return(result)
print(paste("Data pulled from: ", path))

}
