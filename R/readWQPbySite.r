#' Read WQP data by selecting sites in an interactive map
#' 
#' 
#' 
#' 
#' @export
readWQPbySite=function(sites, map=buildMap(sites=sites), mode = "click", types=c('narrowresult', 'activity', 'sites'), merge=T, ...)
	if(missing(sites)){
		sites=readWQP(type="sites", statecode="US:49", ...)
		sites=sites[sites$MonitoringLocationTypeName %in% c("Aggregate surface-water-use","Facility","Lake, Reservoir, Impoundment","Spring","Stream","Wetland"),]
	}
	
	sites_sf=sf::st_as_sf(sites, coords=c("LongitudeMeasure","LatitudeMeasure"))
	selection=mapedit::selectFeatures(sites_sf, map=map, index=T, mode=mode)
	siteids_sel=sites$MonitoringLocationIdentifier[selection]
	
	result=list()
	for(n in 1:length(types)){
		res_n=readWQP(type=paste(types[n]),siteid=as.vector(siteids_sel), ...)
		result=append(result, res_n)
	}
	
	
	
	
	
	
}






