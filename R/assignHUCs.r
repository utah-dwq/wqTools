#' Assign HUC 8 & 12 values to sites
#'
#' This function assigns HUC 8 & 12 values to water quality portal type site objects (or data with site information attached).
#' @param data Input dataset. Must include latitude & longitude columns.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_intersection
#' @examples 
#' # Read a couple of sites from Mantua Reservoir
#' sites=readWQP(type="sites", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"))
#' sites_HUCs=assignHUCs(sites)
#' @return Returns the input data frame with HUC 8 & 12 information appended.
#' @export
assignHUCs=function(data, lat="LatitudeMeasure", long="LongitudeMeasure"){
	poly=wqTools::huc8_12_poly
	x=data
	x=sf::st_as_sf(x, coords=c(long,lat), crs=4326, remove=F)
	x=sf::st_set_crs(x, sf::st_crs(poly))	
	isect=suppressMessages({suppressWarnings({sf::st_join(x, poly, left=TRUE)})})
	sf::st_geometry(isect)=NULL
	return(isect)
}
