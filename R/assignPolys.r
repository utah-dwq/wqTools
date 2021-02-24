#' Assign spatial polygons to sites
#'
#' This function assigns spatial polygon data to water quality portal type site objects (or data with site information attached).
#' @param data Input dataset. Must include latitude & longitude columns.
#' @param polygon Polygon to assign information to sites. Must be an sf type polygon shapefile, WGS84 projection recommended.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @param columns Vector of columns to retain from polygon object. If columns not specified, function will retain all columns. 
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_join
#' @examples 
#' # Read a couple of sites (Mantua Reservoir and a tributary)
#' sites=readWQP(type="sites", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"))
#' sites_AUs=assignPolys(sites, wqTools::au_poly)
#' sites_BUs=assignPolys(sites, wqTools::bu_poly)
#' @return Returns the input data frame with assessment unit information appended.
#' @export
assignPolys=function(data, polygon, lat="LatitudeMeasure", long="LongitudeMeasure", columns){
	if(exists("columns")){polygon=polygon[,columns]} # Subset columns
	x=sf::st_as_sf(data, coords=c(long,lat), crs=4326, remove=F) # Convert data to sf object, WGS84 projection
	isect=suppressMessages({suppressWarnings({sf::st_join(x, polygon, left=TRUE)})}) # Run spatial join
	sf::st_geometry(isect)=NULL # Drop geometry column (this could be optional)
	return(isect)
}


