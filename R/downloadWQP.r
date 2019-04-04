#' Download water quality, station, and other data from USEPA Water Quality Portal
#'
#' Download data from EPA's Water Quality Portal (WQP) as .csv files to outfile_path folder.
#' @param outfile_path Path for file outputs. Defaults to current working directory.
#' @param StartDate Query start date. "mm/dd/yyyy" format.
#' @param EndDate Query end date. "mm/dd/yyyy" format.
#' @param retrieve Vector of data type names to retrieve from WQP. One or more of: "result","narrowresult","activity","activitymetric","sites","detquantlim". Defaults to query all.
#' @return Exports .csv files for all selected data types during selected date period in specified output path.
#' @examples
#' # Read 2018 download narrow result & sites
#' downloadWQP(outfile_path='C:\\Your\\Folder\\Path', start_date="01/01/2018", end_date="12/31/2018", retrieve=c("narrowresult","sites"))

#' @export
downloadWQP<-function(outfile_path=getwd(),start_date,end_date,retrieve=c("narrowresult","activity","sites","detquantlim"),
	siteType=c('Aggregate surface-water-use','Lake, Reservoir, Impoundment','Spring','Stream'),
	statecode="US:49", ...){

#outfile_path='P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\draft_code'
#start_date="01/01/2018"
#end_date="12/31/2018"
#retrieve=c("narrowresult","sites")
#statecode="US:49"
##retrieve=c("sites")
#siteType=c('Aggregate surface-water-use','Lake, Reservoir, Impoundment','Spring','Stream')


# Warning that all WQP data used in irTools should come from the same download.
complete <- c("narrowresult","activity","sites","detquantlim")
if(any(!(complete%in%retrieve))){
  print("WARNING: Data files with records from the same date range but downloaded at different times may differ in ResultIdentifier or other database-generated ID's. All WQP .csvs needed in irTools should come from one download event (e.g. single run of downloadWQP for sites, activity, narrowresult, and detquantlim files).")
}
rm(complete)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

if(substrRight(outfile_path, 1) != '/' | substrRight(outfile_path, 2) != '\\'){
	outfile_path=paste0(outfile_path,'/')
}

retvd_objects=list()

for(n in 1:length(retrieve)){
	file_name_n=paste0(outfile_path,retrieve[n],'-',Sys.Date(),'.csv')
	url_n=readWQP(type=retrieve[n], start_date=start_date, end_date=end_date, statecode=statecode, siteType=siteType, url_only=T, ...)
	if(file.exists(file_name_n)){warning(paste('WARNING:', file_name_n, 'already exists and will not be downloaded and overwritten. Move, rename, or delete this file from outfile_path to re-download this file.'))}
	i=1
	while(!file.exists(file_name_n) & i <=10){
		try({download.file(url_n, file_name_n)})
		i=i+1
	}
}

}
