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
#' UT_assessments=readAttains(type="assessments", state="UT", reportingCycle=2020)

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
	parameters=tidyr::unnest(query_result[["assessments"]][[1]], cols=parameters) %>% tidyr::unnest(cols=associatedUses) %>% dplyr::rename(cycleFirstListed=impairedWatersInformation.listingInformation.cycleFirstListedText, cycleLastAssessed=cycleLastAssessedText)
	parameters=data.frame(parameters[,c("assessmentUnitIdentifier","agencyCode","parameterStatusName","parameterName","associatedUseName","parameterAttainmentCode","trendCode","pollutantIndicator","cycleFirstListed","cycleLastAssessed")])
	associated_actions=tidyr::unnest(query_result[["assessments"]][[1]], cols=parameters) %>% tidyr::unnest(cols=associatedActions)
	associated_actions=data.frame(associated_actions[,c("assessmentUnitIdentifier","parameterName","associatedActionIdentifier")])
	params_actions=merge(parameters, associated_actions, all=T)
	impairments=params_actions[is.na(params_actions$associatedActionIdentifier),]
	impairments=within(impairments, {use_param=paste(associatedUseName, parameterName)})
	impairments_wide=tidyr::pivot_wider(impairments, id_cols=c("assessmentUnitIdentifier"), names_from="use_param", values_from="use_param")
	impairments_wide$impairments=tidyr::unite(impairments_wide[,2:dim(impairments_wide)[2]], "impairments", sep="; ", na.rm=T)$impairments
	impairments_wide=as.data.frame(impairments_wide[,c("assessmentUnitIdentifier","impairments")])
	tmdls=associated_actions 
	tmdls$tmdl=paste0(tmdls$parameterName, " (", tmdls$associatedActionIdentifier, ")")
	tmdls_wide=tidyr::pivot_wider(tmdls, id_cols=c("assessmentUnitIdentifier"), names_from="tmdl", values_from="tmdl")
	tmdls_wide$tmdls=tidyr::unite(tmdls_wide[,2:dim(tmdls_wide)[2]], "tmdls", sep="; ", na.rm=T)$tmdls
	tmdls_wide=as.data.frame(tmdls_wide[,c("assessmentUnitIdentifier","tmdls")])
	assessments_wide=merge(assessments, impairments_wide, all.x=T)
	assessments_wide=merge(assessments_wide, tmdls_wide, all.x=T)
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
