#' Download water quality, station, and other data from USEPA Water Quality Portal
#'
#' Download data from EPA's Water Quality Portal (WQP) as .csv files to outfile_path folder.
#' @param outfile_path Path for file outputs. Defaults to current working directory.
#' @param start_date Query start date. "mm/dd/yyyy" format.
#' @param end_date Query end date. "mm/dd/yyyy" format.
#' @param retrieve Vector of data type names to retrieve from WQP. One or more of: "result","narrowresult","activity","sites","detquantlim". Defaults to query narrowresult, activity, sites, and detquantlim.
#' @param zip Logical. If FALSE (default) files are downloaded straight as csv. If TRUE, files are downloaded as zipped folders (helps prevent server time-outs, recommend for large downloads).
#' @param zip Logical. If FALSE (default) files downloaded as zip folders are not automatically unzipped. If TRUE, the function will unzip all downloaded zip files. Only used if zip==TRUE.
#' @param ... Other arguments to be passed to readWQP when generating a query URL.
#' @return Exports .csv files for all selected data types during selected date period in specified output path.
#' @examples
#' # Read 2018 download narrow result & sites
#' downloadWQP(outfile_path='C:\\Your\\Folder\\Path', start_date="01/01/2018", end_date="12/31/2018", retrieve=c("narrowresult","sites"))

#' @export
downloadWQP<-function(outfile_path=getwd(),retrieve=c("narrowresult","activity","sites","detquantlim"),
	siteType=c('Aggregate surface-water-use','Lake, Reservoir, Impoundment','Spring','Stream'),
	statecode="US:49", zip=FALSE, unzip=FALSE, ...){

#outfile_path='C:\\Users\\jvander\\Documents\\R\\irTools-test-16\\01-raw-data'
#start_date="01/01/2014"
#end_date="12/31/2018"
#retrieve=c("narrowresult","sites", 'result', 'activity','detquantlim')
#statecode="US:49"
##retrieve=c("sites")
#siteType=c('Aggregate surface-water-use','Lake, Reservoir, Impoundment','Spring','Stream')
#zip=T
#unzip=T

# Warning that all WQP data used in irTools should come from the same download.
complete <- c("narrowresult","activity","sites","detquantlim")
if(any(!(complete%in%retrieve))){
  print("WARNING: Data files with records from the same date range but downloaded at different times may differ in ResultIdentifier or other database-generated ID's. All WQP .csvs needed in irTools should come from one download event (e.g. single run of downloadWQP for sites, activity, narrowresult, and detquantlim files).")
}
rm(complete)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

zip_file_names=vector(length=length(retrieve))
for(n in 1:length(retrieve)){
	file_name_n=paste0(outfile_path,"/",retrieve[n],'-',Sys.Date(),'.csv')
	url_n=readWQP(type=retrieve[n], statecode=statecode, siteType=siteType, url_only=T, ...)
	if(zip){
		url_n=gsub('zip=no', 'zip=yes', url_n)
		file_name_n=gsub('.csv', '.zip', file_name_n)
		zip_file_names[n]=file_name_n
	}
	if(file.exists(file_name_n)){stop(paste('ERROR:', file_name_n, 'already exists and will not be downloaded and overwritten. Move, rename, or delete this file from outfile_path to re-download this file.'))}
	i=1
	while(!file.exists(file_name_n) & i <=10){
		try({download.file(url_n, file_name_n, mode='wb')})
		i=i+1
	}
}


if(zip & unzip){
	unzipped_file_names=gsub(paste0("-",Sys.Date(),".zip"), '.csv', zip_file_names)
	unzipped_file_names=gsub('sites', 'station', unzipped_file_names)
	unzipped_file_names=gsub('detquantlim', 'resdetectqntlmt', unzipped_file_names)
	unzipped_file_names=gsub('activity', 'activityall', unzipped_file_names)
	
	csv_file_names=gsub('zip', 'csv', zip_file_names)
	for(n in 1:length(zip_file_names)){
		unzip(zip_file_names[n], exdir=outfile_path)
		file.rename(unzipped_file_names[n], csv_file_names[n])
		file.remove(zip_file_names[n])
	}
}

}












