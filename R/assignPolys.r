#' Assign spatial polygons to sites
#'
#' This function assigns spatial polygon data to water quality portal type site objects (or data with site information attached).
#' @param data Input dataset. Must include latitude & longitude columns.
#' @param polygon Polygon to assign information to sites. Must be an sf type polygon shapefile.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @param columns Vector of columns to retain from polygon object. If columns not specified, function will retain all columns. 
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_join

#' @return Returns the input data frame with assessment unit information appended.
#' @export
assignPolys=function(data, polygon, lat="LatitudeMeasure", long="LongitudeMeasure", columns){
	if(exists("columns")){polygon=polygon[,columns]}
	x=data
	x=sf::st_as_sf(x, coords=c(long,lat), crs=4326, remove=F)
	x=sf::st_set_crs(x, sf::st_crs(polygon))	
	isect=suppressMessages({suppressWarnings({sf::st_join(x, polygon, left=TRUE)})})
	sf::st_geometry(isect)=NULL
	return(isect)
}


