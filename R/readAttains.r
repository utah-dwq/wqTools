#' Read data from EPA's ATTAINS database
#'
#' This function pulls information from EPA ATTAINS database based on submitted arguments via ATTAINS web service. Any ATTAINS web service compatible argument can be submitted to this funciton. Depending on type, at least one argument may be required. See https://www.epa.gov/waterdata/how-access-and-use-attains-web-services for a list of required and compatible arguments.
#' The function is essentially an ATTAINS specific wrapper for jsonlite::fromJSON. It generates the appropriate web path to connect to ATTAINS web service and converts JSON to R object.
#'
#' @param type ATTAINS data type to read. One of: "assessmentUnits", "assessments", or "actions".
#' @param stateCode Two letter state code
#' @param ... Additional arguments to be passed to ATTAINS web service path.

#' @importFrom jsonlite fromJSON
#' @import tidyr

#' @return Returns a data.frame or list of queried ATTAINS data. Note that some results may contain additional lists of results that can be further flattened.

#' @examples
#' #Read Utah assessment unit information
#' UT_AUs=readAttains(type="assessmentUnits", stateCode="UT")
#' 
#' #Read Utah actions
#' UT_actions=readAttains(type="actions", stateCode="UT")
#' 
#' #Read Utah assessments
#' UT_assessments=readAttains(type="assessments", stateCode="UT", reportingCycle=2020)

#' @export
readAttains=function(type="assessments", stateCode=NULL, ...){

if(missing(stateCode) & type!="domains"){
	stop("Required argument, stateCode, missing without default.")
}

if(!type %in% c("assessmentUnits", "assessments", "actions")){
	stop("type must be one of assessmentUnits, assessments, or actions.")
}

pastecollapse=function(x){paste0(names(x), "=", x, collapse="&")}

path="https://attains.epa.gov/attains-public/api/"

args=list(...)

if(type=="assessments"){
	args$state=stateCode
	args=args[names(args)!="stateCode"]
}else{args$stateCode=stateCode}

# Warn if reportingCycle not specified
if(type=="assessments" & !any(names(args)==("reportingCycle"))){
	warning("reportingCycle not specified by user. Returning most recent available assessment. Note: these may results may be draft.")
}

base_path=paste0(path, type, "?")
args_path=pastecollapse(args)
query_path=paste0(base_path, args_path)
query_result=jsonlite::fromJSON(query_path, flatten=T)$items

if(type=="assessments"){
	assessments=query_result[["assessments"]][[1]]
	assessments=assessments[,c("assessmentUnitIdentifier","agencyCode","trophicStatusCode","rationaleText","epaIRCategory","overallStatus","cycleLastAssessedText","yearLastMonitoredText")]  %>% dplyr::rename(cycleLastAssessed=cycleLastAssessedText, yearLastMonitored=yearLastMonitoredText)
	parameters_orig=tidyr::unnest(query_result[["assessments"]][[1]], cols=parameters) %>% tidyr::unnest(cols=associatedUses) %>% dplyr::rename(cycleFirstListed=impairedWatersInformation.listingInformation.cycleFirstListedText, cycleLastAssessed=cycleLastAssessedText)
	parameters=data.frame(parameters_orig[,c("assessmentUnitIdentifier","agencyCode","parameterStatusName","parameterName","associatedUseName","parameterAttainmentCode","trendCode","pollutantIndicator","cycleFirstListed","cycleLastAssessed")])
	associated_actions=tidyr::unnest(subset(parameters_orig, lengths(parameters_orig$associatedActions)>0), cols = associatedActions)
	associated_actions=data.frame(associated_actions[,c("assessmentUnitIdentifier","parameterName","associatedActionIdentifier")])
	params_actions=merge(parameters, associated_actions, all=T)
	params_actions_uses=within(params_actions, {use_param=paste0(associatedUseName,": ", parameterName)})
	d303 = subset(params_actions_uses, params_actions_uses$parameterStatusName=="Cause"&is.na(params_actions_uses$associatedActionIdentifier))
	d303_wide = d303%>%tidyr::pivot_wider(id_cols=c("assessmentUnitIdentifier"), names_from="use_param", values_from="use_param")
	d303_wide = as.data.frame(d303_wide%>%unite(col = "303D_NOT_MEETING",2:dim(d303_wide)[2],sep="; ", na.rm=T))
	tmdls_nm = subset(params_actions_uses, params_actions_uses$parameterStatusName=="Cause"&!is.na(params_actions_uses$associatedActionIdentifier))
	tmdls_nm = unique(tmdls_nm[,c("assessmentUnitIdentifier","use_param")])
	tmdls_nm_wide = tmdls_nm%>%tidyr::pivot_wider(id_cols=c("assessmentUnitIdentifier"), names_from="use_param", values_from="use_param")
	tmdls_nm_wide = as.data.frame(tmdls_nm_wide%>%unite(col = "APPRVD_TMDL_NOT_MEETING",2:dim(tmdls_nm_wide)[2],sep="; ", na.rm=T))
	tmdls_m = subset(params_actions_uses, params_actions_uses$parameterStatusName=="Meeting Criteria"&!is.na(params_actions_uses$associatedActionIdentifier))
	tmdls_m = unique(tmdls_m[,c("assessmentUnitIdentifier","use_param")])
	tmdls_m_wide = tmdls_m%>%tidyr::pivot_wider(id_cols=c("assessmentUnitIdentifier"), names_from="use_param", values_from="use_param")
	tmdls_m_wide = as.data.frame(tmdls_m_wide%>%unite(col = "APPRVD_TMDL_MEETING_CRIT",2:dim(tmdls_m_wide)[2],sep="; ", na.rm=T))
	assessments_wide=merge(assessments, d303_wide, all.x=T)
	assessments_wide=merge(assessments_wide, tmdls_nm_wide, all.x=T)
	assessments_wide=merge(assessments_wide, tmdls_m_wide, all.x=T)
	assessments_wide=within(assessments_wide,{
	  DWQ_Status = NA
	  DWQ_Status[epaIRCategory=="1"] = "Fully Supporting"
	  DWQ_Status[epaIRCategory=="2"] = "No Evidence of Impairment"
	  DWQ_Status[epaIRCategory=="3"] = "Insufficient Data"
	  DWQ_Status[epaIRCategory=="4A"] = "Approved TMDL"
	  DWQ_Status[epaIRCategory=="5"&!is.na(APPRVD_TMDL_NOT_MEETING)] = "Not Supporting but has approved TMDL for some parameters"
	  DWQ_Status[epaIRCategory=="5"&is.na(APPRVD_TMDL_NOT_MEETING)] = "Not Supporting"
	})
	result=list(assessments=assessments, parameters=parameters, associated_actions=associated_actions, assessments_wide=assessments_wide)
}

if(type=="actions"){
	actions=query_result$actions[[1]]
	result=actions
}

if(type=="assessmentUnits"){
	assessmentUnits=query_result$assessmentUnits[[1]]
	result=assessmentUnits
}

return(result)

}
