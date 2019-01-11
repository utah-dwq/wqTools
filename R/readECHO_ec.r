#' Read effluent chart data from EPA ECHO webservices
#'
#' This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters 
#' @param ... additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data reads. Note that arguments for output are ignored.
#' @return A flat data frame of EPA ECHO effluent chart data

#' @export
readECHO_ec<-function(...){
args=list(...)
if(length(args)!>0){stop("Error. At least one argument must be specified.")}

args$output="CSV"

base_path="https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart"






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
