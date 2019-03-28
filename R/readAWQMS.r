#' Read AWQMS Water Quality Data
#'
#' This function extracts water quality data from AWQMS based on user argument inputs.
#' All arguments except type are optional, but at least one must be provided to limit download size and prevent errors connecting to AWQMS.
#' Note that some, but not all, special characters have been accounted for (e.g. spaces, fore-slashes, and commas in characteristic names and site types).
#' @param type Data type to read. One of "projects", "sites", "metrics", "results", "continuous_results", "indexes", or "beach_actions".
#' @param start_date Query start date in "mm/dd/yyyy" format.
#' @param end_date Query end date in "mm/dd/yyyy" format.
#' @param unnest_results If TRUE (default) and type=="results", the result data frame is unnested from the query return object.
#' @param ... additional arguments to be passed to AWQMS query path. See http://www.awqms.org/files/AWQMS_Training/AWQMS%20REST%20Web%20Services%20User%20Guide.pdf for optional arguments.
#' @importFrom plyr ldply
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest

#' @return A data frame of AWQMS data
#' @examples
#' # Read a couple of sites
#' sites=readAWQMS(type='sites', MonitoringLocationIdentifiersCsv=c("4900440","4900470"))
#' # Make a map of those sites & assign assessment units & uses
#' buildMap(sites=sites)
#' sites_aus=assignAUs(sites)
#' sites_aus_uses=assignUses(sites_aus)
#' sites_aus_uses
#' 
#' # Read all results from those sites (2016-2018)
#' results=readAWQMS(type='results', MonitoringLocationIdentifiersCsv=c("4900440","4900470"), start_date="01/01/2016", end_date="12/31/2018")
#' 
#' # Read a subset of those results (just two parameters, 2016-2018)
#' results2=readAWQMS(type='results', MonitoringLocationIdentifiersCsv=c("4900440","4900470"), Characteristic=c("Total dissolved solids","Phosphate-phosphorus"), start_date="01/01/2016", end_date="12/31/2018")
#' 
#' # Read Utah DWQ projects
#' projects=readAWQMS(type="projects",OrganizationIdentifiersCsv="UTAHDWQ_WQX")


#' @export
readAWQMS<-function(type="results", unnest_results=TRUE, ...){
args=list(...)
#argument testing
#type="sites"
#start_date="01/01/2015"
#end_date="12/31/2018"
#MonitoringLocationIdentifiersCsv=c("4900440","4900470")
#MonitoringLocationType=c('Lake')
#Characteristic=c("Total dissolved solids", "Phosphate-phosphorus", "Temperature, water", "Dissolved oxygen (DO)")
#args=list(MonitoringLocationIdentifiersCsv=MonitoringLocationIdentifiersCsv, Characteristic=Characteristic, start_date=start_date, end_date=end_date)
#args=list(MonitoringLocationIdentifiersCsv=MonitoringLocationIdentifiersCsv)

base_url='http://awqms.goldsystems.com/api/'
if(type=='projects'){type_url='ProjectsVer1'}
if(type=='sites'){type_url='MonitoringLocationsVer1'}
if(type=='metrics'){type_url='MetricsVer1'}
if(type=='results'){type_url='ResultsVer1'}
if(type=='continuous_results'){type_url='ContinuousResultsVer1'}
if(type=='indexes'){type_url='IndexesVer1'}
if(type=='beach_actions'){type_url='BeachActionsVer1'}

base_url=paste0(base_url,type_url,"?")

if(any(names(args)=="start_date")){
	args$MinDate=format(as.Date(args$start_date, format='%m/%d/%Y'), format="%m-%d-%Y")
	args=args[names(args)!="start_date"]
}

if(any(names(args)=="end_date")){
	args$MaxDate=format(as.Date(args$end_date, format='%m/%d/%Y'), format="%m-%d-%Y")
	args=args[names(args)!="end_date"]
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrRight(names(args),3)=="Csv"

csv_args=args[substrRight(names(args),3)=="Csv"]
for(n in 1:length(csv_args)){
	csv_args[n]=paste0(noquote(csv_args[[n]]),collapse=',')
}

args=args[substrRight(names(args),3)!="Csv"]
pastecollapse=function(x){paste0(names(x), "=", x, collapse="&")}

args=append(args, csv_args)


if(any(names(args)=="Characteristic")){
	char=args$Characteristic
	char=gsub(",", "%2C", char)
	args$Characteristic=char
}

if(length(args)>1){
	for(n in 1:(length(args)-1)){
		if(n==1){args_mrg=merge(args[n],args[(n+1)])
		}else{args_mrg=merge(args_mrg,args[(n+1)])}
	}
	arg_paths=apply(args_mrg,1,'pastecollapse')

}else{arg_paths=pastecollapse(args)}

arg_paths=gsub("/", "%2F", arg_paths)
arg_paths=gsub(" ", "%20", arg_paths)
paths_all=paste0(base_url,arg_paths)

print(paths_all)

result=plyr::ldply(paths_all, .fun=jsonlite::fromJSON, .progress="text")
if(type=='results' & unnest_results){
	result=tidyr::unnest(result, Results)
}

if(type=='sites'){
	result$Latitude=as.numeric(result$Latitude)
	result$Longitude=as.numeric(result$Longitude)
	names(result)[names(result)=="Latitude"]="LatitudeMeasure"
	names(result)[names(result)=="Longitude"]="LongitudeMeasure"
	names(result)[names(result)=="MonitoringLocationType"]="MonitoringLocationTypeName"
}

return(result)

}

