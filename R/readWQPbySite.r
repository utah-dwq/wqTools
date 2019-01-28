#' Read WQP data by selecting sites in an interactive map
#' 
#' This function allows the user to read WQP data by selecting desired sites in an interactive map and specifiying desired types of data and output.
#' 
#' @param sites Optional. A sites object containing lat/long columns named "LatitudeMeasure" & "LongitudeMeasure". If no sites object is provided, a Utah state-wide query of WQP will be generated for you.
#' @param map Optional. A map object to use as a background for site selection. If no map object is provided, a basic map will be generated for you.
#' @param mode Mode for map selection. One of "click" or "draw". Click allows site selection by clicking on individual sites. Draw allows site selection by polygons.
#' @param types Vector of data types to read from WQP for selected sites. See ?wqTools::readWQP for options.
#' @param merge Logical. If TRUE (default), merge all selected data types to single data frame. Merges are performed as left joins in order they are provided. If FALSE, return list of individual data objects.
#' @param ... Other arguments to be passed to readWQP (e.g. start_date, end_date, characteristicName, etc.). See ?wqTools::readWQP for more info.
#' @importFrom sf st_as_sf
#' @importFrom mapedit selectFeatures
#' @examples
#' wqp_data=readWQPbySite()
#' @export
readWQPbySite=function(sites, map, mode = "click", types=c('narrowresult', 'activity', 'sites'), merge=T, ...){
	
	if(missing(sites)){
		print("Querying WQP site locations.")
		sites=readWQP(type="sites", statecode="US:49", print=F, ...)
		sites=sites[sites$MonitoringLocationTypeName %in% c("Facility","Lake, Reservoir, Impoundment","Spring","Stream","Wetland"),]
	}
	if(missing(map) & !missing(sites)){map=buildMap(sites=sites)}

	print("Generating site selection map.")
		
	sites_sf=sf::st_as_sf(sites, coords=c("LongitudeMeasure", "LatitudeMeasure"))
	selection=mapedit::selectFeatures(sites_sf, map=map, index=T, mode=mode)
	siteids_sel=sites$MonitoringLocationIdentifier[selection]
	
	print("Sites selected. Reading selected data types from WQP.")
	
	result=list()
	for(n in 1:length(types)){
		res_n=readWQP(type=paste(types[n]),siteid=as.vector(siteids_sel), print=F, ...)
		result$res_n=res_n
		names(result)[n]=types[n]
	}
	
	if(merge){
		merged_result = Reduce(function(...) merge(..., all.x=T), result)
		return(merged_result)
	}else{return(result)}
		
}



