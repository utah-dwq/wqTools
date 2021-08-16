#' Read facility information from EPA ECHO webservices
#'
#' This function extracts facility information from EPA ECHO based on argument inputs.
#' @param ... Additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/facility-search-water#!/Facility_Information/get_cwa_rest_services_get_facility_info for optional arguments for facilities. Note that arguments for output are ignored.
#' @return A data frame of EPA ECHO facility information
#' @param df Logical. If TRUE, extract geometry coordinates from list format and return as columns in data frame.
#' @importFrom jsonlite fromJSON
#' @examples
#' # Read facility locations in Utah
#' ut_fac=readECHO_fac(p_st="ut", p_act="y")
#' head(ut_fac)
#' # Read facility locations for two permit IDs
#' two_fac=readECHO_fac(p_pid=c("UT0021717","UT0025241"))
#' two_fac

#' @export
readECHO_fac<-function(..., df=FALSE){
args=list(...)

#args=list(p_st="ut", p_pid=c("UT0021717","UT0020834","UT0020109","UT0020427"))
pastecollapse=function(x){paste0(x, collapse="%2C%20")}
args=plyr::llply(args,pastecollapse)

path="https://echodata.epa.gov/echo/cwa_rest_services.get_facility_info?"
args$output="JSON"

arg_path=paste0(names(args),"=",args,collapse="&")
path=paste0(path,arg_path)
print(path)

#print("Querying facility information...")
fac_query=jsonlite::fromJSON(path)
geoJSON_path=paste0("https://echodata.epa.gov/echo/cwa_rest_services.get_geojson?output=GEOJSON&qid=",fac_query$Results$QueryID)
print("Querying facility geometries...")
fac_geoJSON=jsonlite::fromJSON(geoJSON_path)
result=fac_geoJSON$features
print("Read facilities complete...")

if(df){ # Convert from list dataframe to standard dataframe
	fac_coords=do.call(rbind.data.frame,result$geometry$coordinates)
	names(fac_coords)=c("dec_long","dec_lat") # Rename columns
	fac_coords
	fac_df=data.frame(result$properties, fac_coords) #Don't really need the type object, it's always all "Feature"
	result=fac_df
}

return(result)

}



