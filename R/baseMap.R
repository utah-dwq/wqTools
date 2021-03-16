#' Water quality base map
#'
#' This function generates a base map for use in water quality analyses. It includes useful base layers, Utah assessment unit polygons, and Utah beneficial use designations.
#' @import leaflet
#' @param layers_control Logical. If TRUE (default), a layers control widget is added to the map. Set to FALSE to generate a custom layers control widget.
#' @return A leaflet map object
#' @export
baseMap=function(layers_control=TRUE){
	map=leaflet()%>%
	addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer", group = "USGS topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
	addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", group = "Hydrography", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
	addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
	addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
	addMapPane("underlay_polygons", zIndex = 410) %>%
	addMapPane("au_poly", zIndex = 415)  %>%
	addMapPane("markers", zIndex = 420)  %>%
	addPolygons(data=wqTools::bu_poly,group="Beneficial uses",fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
		popup=paste0(
			"Description: ", wqTools::bu_poly$R317Descrp,
			"<br> Uses: ", wqTools::bu_poly$bu_class)
	) %>% 
	addPolygons(data=wqTools::au_poly,group="Assessment units",fillOpacity = 0.1,weight=3,color="orange", options = pathOptions(pane = "au_poly"),
		popup=paste0(
			"AU name: ", wqTools::au_poly$AU_NAME,
			"<br> AU ID: ", wqTools::au_poly$ASSESS_ID,
			"<br> AU type: ", wqTools::au_poly$AU_Type)
	) %>% 
	addPolygons(data=wqTools::ss_poly,group="Site-specific standards",fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
		popup=paste0("SS std: ", wqTools::ss_poly$SiteSpecif)
	) %>%
	addPolygons(data=wqTools::wmu_poly,group="Watershed management units",fillOpacity = 0.1,weight=3,color="red", options = pathOptions(pane = "underlay_polygons"),
		popup=wqTools::wmu_poly$Mgmt_Unit
	) %>%
	addPolygons(data=wqTools::hnnc_poly,group="Headwater NNC",fillOpacity = 0.4,weight=3,color="pink", options = pathOptions(pane = "underlay_polygons")) %>%
	addPolygons(data=wqTools::ut_poly,group="UT boundary",fillOpacity = 0.1,weight=3,color="purple", options = pathOptions(pane = "underlay_polygons")) %>%
	hideGroup("Assessment units") %>%
	hideGroup("Site-specific standards") %>%
	hideGroup("Beneficial uses") %>%
	hideGroup("Headwater NNC") %>%
	hideGroup("UT boundary") %>%
	hideGroup("Watershed management units")
	
	if(layers_control){
		map=leaflet::addLayersControl(map,
			position ="topleft",
			baseGroups = c("World topo","USGS topo", "Hydrography", "Satellite"),overlayGroups = c("Assessment units","Beneficial uses", "Site-specific standards", "Headwater NNC", "Watershed management units", "UT boundary"),
			options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
	}	
	return(map)
}
