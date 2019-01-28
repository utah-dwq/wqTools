#' Read WQP data by selecting sites in an interactive map
#' 
#' 
#' 
#' 
#' @export
readWQPbySite=function(sites, map=buildMap(sites=sites), types=c('narrowresult', 'activity', 'sites'), ...)
	if(missing(sites)){
		sites=readWQP(type="sites", statecode="US:49")
		sites=sites[sites$MonitoringLocationTypeName %in% c("Aggregate surface-water-use","Facility","Lake, Reservoir, Impoundment","Spring","Stream","Wetland"),]
	}
	
	sites_sf=sf::st_as_sf(sites, coords=c("LongitudeMeasure","LatitudeMeasure"))
	selection=mapedit::selectFeatures(sites_sf, map=map, index=T)
	siteids_sel=sites$MonitoringLocationIdentifier[selection]
	
	
	
	
	
	
}






