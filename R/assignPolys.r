#' Assign spatial polygons to sites
#'
#' This function assigns spatial polygon data to water quality portal type site objects (or data with site information attached).
#' @param polygon Input polygon used to assign information to sites. Must be an sf type polygon shapefile. User may choose from au_poly, bu_poly, huc8_12_poly, huc8_poly, huc12_poly,
#' ss_poly, ut_poly, or wmu_poly
#' @param columns Vector of columns user wishes to retain from polygon object. If columns object is not specified, function will retain all polygon columns. 
#' @param data Input dataset. Must include latitude & longitude columns.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_intersection

#' @return Returns the input data frame with assessment unit information appended.
#' @export
assignPolys=function(polygon, columns, data, lat="LatitudeMeasure", long="LongitudeMeasure"){
	poly = eval(parse(text = paste0("wqTools::",polygon)))
	poly1=sf::st_as_sf(poly)
	if(exists("columns")){poly1=poly1[,columns]}
	x=data
	x=sf::st_as_sf(x, coords=c(long,lat), crs=4326, remove=F)
	x=sf::st_set_crs(x, sf::st_crs(poly1))	
	isect=suppressMessages({suppressWarnings({sf::st_intersection(x, poly1)})})
	sf::st_geometry(isect)=NULL
	result=merge(data, isect, all.x=T)
	return(result)
}


