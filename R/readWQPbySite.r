#' Read WQP data by selecting sites in an interactive map
#' 
#' This function allows the user to read WQP data by selecting desired sites in an interactive map and specifiying desired types of data and output.
#' When map is launched, either click (mode="click") or draw polygons (mode="draw") around desired sites, then click "Done" button at bottom right of map. If edit==TRUE, an edit dialog will open 
#' in R console. Update "keep" column to anything other than "Y" to reject selected sites, then close dialog to proceed.
#' 
#' @param sites Optional. A sites object containing lat/long columns named "LatitudeMeasure" & "LongitudeMeasure". If no sites object is provided, a Utah state-wide query of WQP will be generated for you.
#' @param map Optional. A map object to use as a background for site selection. If no map object is provided, a basic map will be generated for you.
#' @param mode Mode for map selection. One of "click" or "draw". Click allows site selection by clicking on individual sites. Draw allows site selection by polygons.
#' @param types Vector of data types to read from WQP for selected sites. See ?wqTools::readWQP for options.
#' @param merge Logical. If TRUE (default), merge all selected data types to single data frame. Merges are performed as left joins in order they are provided. If FALSE, return list of individual data objects.
#' @param sitetypes Vector of site types to be included in query (only used if sites argument is not provided).
#' @param edit Logical. If TRUE (default) open editor in R console to edit selected sites. To drop sites from the query, update the "keep" column to anything other than "Y", then close edit dialog.
#' @param ... Other arguments to be passed to readWQP (e.g. start_date, end_date, characteristicName, etc.). See ?wqTools::readWQP for more info. This is passed to both the sites query (if sites are not provided) and the results queries.
#' @importFrom sf st_as_sf
#' @importFrom mapedit selectFeatures
#' @examples
#' wqp_data=readWQPbySite(start_date="01/01/2016", end_date="12/31/2018")
#' @export
readWQPbySite=function(sites, map, mode = "click", types=c('sites','narrowresult', 'activity'), merge=T, edit=T,
						sitetypes=c("Canal Drainage","Canal Irrigation","Canal Transport","Lake","Lake, Reservoir, Impoundment","Reservoir","River/Stream","River/Stream Intermittent",
									"River/Stream Perennial","Seep","Spring","Stream","Stream: Canal","Stream: Ditch","Wetland","Wetland Undifferentiated"),
						...){
	
	if(missing(sites)){
		print("Querying WQP site locations.")
		sites=readWQP(type="sites", statecode="US:49", ...)
		sites=sites[sites$MonitoringLocationTypeName %in% sitetypes,]
	}
	if(missing(map) & !missing(sites)){map=buildMap()}

	print("Generating site selection map.")
		
	#sites_sf=sf::st_as_sf(sites, coords=c("LongitudeMeasure", "LatitudeMeasure"))
	
	selection=mapedit::selectFeatures(sf::st_as_sf(sites, coords=c("LongitudeMeasure", "LatitudeMeasure")), map=map, index=T, mode=mode)
	siteids_sel=sites$MonitoringLocationIdentifier[selection]
	
	selected_sites=sites[sites$MonitoringLocationIdentifier %in% siteids_sel, ]
	selected_sites$keep="Y"
	selected_sites=selected_sites[,c("keep","OrganizationFormalName","MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName","ProviderName")]
	if(edit){
		selected_sites=edit(selected_sites, title="Update keep column. Only 'Y's will be queried")
		selected_sites=selected_sites[selected_sites$keep=="Y",]
	}
	siteids_sel=selected_sites$MonitoringLocationIdentifier
	
	print("Sites selected. Reading selected data types from WQP.")
	
	result=list()
	for(n in 1:length(types)){
		print(paste("Reading",types[n]))
		res_n=readWQP(type=paste(types[n]),siteid=as.vector(siteids_sel), ...)
		result$res_n=res_n
		names(result)[n]=types[n]
	}
	
	if(merge){
		merged_result = Reduce(function(...) merge(..., all.x=T), result)
		return(merged_result)
	}else{return(result)}
		
}



