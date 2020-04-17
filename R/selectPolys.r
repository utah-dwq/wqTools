#' Interactively select spatial polygons to query or filter data
#' 
#' A Shiny gadget designed for interactive selection of spatial polygons. This is intended to help users find AU and HUC IDs for querying or filtering data.
#' @param type One of 'au', 'huc8', or 'huc12' to select assessment unit, HUC 8, or HUC 12 polygons
#' @import shiny
#' @import miniUI
#' @import leaflet

#' @return A vector of selected polygon identifiers (AU IDs or HUC numbers).

#' @examples
#' sites=readWQP(type='sites', auid=selectPolys('au'))

#' @export
selectPolys=function(type='au'){
	
	if(type=='au'){
		poly=wqTools::au_poly
		poly=within(poly, {
			label=lapply(paste0(
						'<p>',
						"AU name: ", poly$AU_NAME,
						'<br />', "AU ID: ", poly$ASSESS_ID,
						'<br />', "AU type: ", poly$AU_Type
					), HTML)
			id=ASSESS_ID		
		})
	}
	
	if(type=='huc12'){
		poly=wqTools::huc12_poly
		poly=within(poly, {
			polyID=seq(1, dim(poly)[1], 1)
			label=HUC12
			id=HUC12
		})
	}

	if(type=='huc8'){
		poly=wqTools::huc8_poly
		poly=within(poly, {
			polyID=seq(1, dim(poly)[1], 1)
			label=HUC8
			id=HUC8
		})
	}
	
	ui=miniPage(
		gadgetTitleBar("Select polygons"),
		miniContentPanel(
			shinycssloaders::withSpinner(leaflet::leafletOutput("map", height='600px', width="100%"),size=2, color="#0080b7")
		)
	)
	
	server=function(input, output, session){

		reactive_objects=reactiveValues()		
		reactive_objects$sel_polys=NULL
		
		output$map=renderLeaflet({
			wqTools::buildMap(plot_polys=F) %>% 
				addPolygons(data=poly, group="Polygons",smoothFactor=2,fillOpacity = 0.1, layerId=~polyID,weight=3,color="orange", options = pathOptions(pane = "au_poly"),
					label=~label
				) %>% 
				addMapPane("highlight", zIndex = 414) %>%
				leaflet::addLayersControl(
							position ="topleft",
							baseGroups = c("Topo","Satellite"),overlayGroups = c("Polygons"),
							options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
		})
		
		observeEvent(input$map_shape_click,{
			click = input$map_shape_click$id
			if(!is.null(click)){
				sel_id=as.character(unique(poly$id[poly$polyID==click]))
				if(sel_id %in% reactive_objects$sel_polys){
					reactive_objects$sel_polys=reactive_objects$sel_polys[!reactive_objects$sel_polys %in% sel_id]
				}else{
					reactive_objects$sel_polys=append(reactive_objects$sel_polys, sel_id)
				}
			}
		})
		
		# Update map highlight
		observeEvent(reactive_objects$sel_polys, ignoreNULL = F, ignoreInit=T, {
			leafletProxy("map") %>%
				clearGroup(group='highlight') %>%
				addPolygons(data=poly[poly$id %in% reactive_objects$sel_polys,],
					group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
		})
		
		observeEvent(input$done, {
			stopApp(reactive_objects$sel_polys)
		})
	}

	runGadget(ui, server)

}

