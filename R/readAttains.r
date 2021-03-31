#' Read data from EPA's ATTAINS database
#'
#' This function pulls information from EPA ATTAINS database based on submitted arguments via ATTAINS web service. Any ATTAINS web service compatible argument can be submitted to this funciton. Depending on type, at least one argument may be required. See https://link-to-be-obtained.gov for a list of required and compatible arguments.
#' The function is essentially an ATTAINS specific wrapper for jsonlite::fromJSON. It generates the appropriate web path to connect to ATTAINS web service and converts JSON to R object.
#'
#' @param type ATTAINS data type to read. One of: "assessmentUnits", "assessments", or "actions".
#' @param stateCode Two letter state code
#' @param reportingCycle Four digit year of interest. Must be specified for type == "assessments" (NOTE: EPA working on update that will return the most recent cycle).
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
#' UT_assessments=readAttains(type="assessments", state="UT", reportingCycle=2016)

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
}else{args$stateCode=stateCode}

if(type=="assessments" & any(names(args)==("stateCode"))){
	args$state=args$stateCode
	args=args[names(args)!="stateCode"]
	}

if(type=="assessments" & !any(names(args)==("reportingCycle"))){
	stop("reportingCycle must be specified for type='assessments' reads.")
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
	result=list(assessments=assessments, parameters=parameters, associated_actions=associated_actions)
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


