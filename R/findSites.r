#' Interactively find and select water quality sites
#' 
#' A Shiny gadget designed for interactive selection of water quality sites including permit locations, ambient monitoring locations, and streamflow gauges. This is intended to help users find sites for querying data from ECHO, WQP, or NWIS.
#' @import shiny
#' @import miniUI
#' @import leaflet
#' @import shinyBS
#' @import shinybusy

#' @return A dataframe of site information

#' @export
findSites=function(){

	ui=miniPage(
		gadgetTitleBar("Find data", right=NULL),
		miniContentPanel(
			column(5,
				bsCollapse(open=c(1,2), multiple=T,
				bsCollapsePanel(list(icon('plus-circle'),"Query sites"), value=1, 
					fluidRow(
						column(4, actionButton("zoom", "Zoom to coordinates", style='color: #fff; background-color: #337ab7; margin-top: 24px; border-color: #2e6da4%', icon=icon('search-plus'))),
						column(4, numericInput("lat", "Latitude", 40.8)),
						column(4, numericInput("lon", "Longitude", -111.8))
					),
					shinyWidgets::checkboxGroupButtons("data_types", label="Search for:", choices=c("Permits", "Monitoring locations", "USGS gauges"), 
						selected=c("Permits", "Monitoring locations", "USGS gauges"), status="primary", checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
					actionButton('query_sites', 'Query sites in view area', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
				),
				bsCollapsePanel(list(icon('plus-circle'),"WQP site options"), value=2, 
					fluidRow(
						column(6, uiOutput('orgs')),
						column(6, uiOutput('site_types'))
					),
					fluidRow(
						column(6, uiOutput('visit_count_slider'))
					)
				),
				bsCollapsePanel(list(icon('plus-circle'),"Select and export"), value=3, 
					uiOutput('sel_sites_multiInput'),
					fluidRow(
						column(2, uiOutput('sel_all')),
						column(2, uiOutput('clear_all'))
					),
					fluidRow(column(3, uiOutput('export')))
				))
			),
			column(7,
					shinycssloaders::withSpinner(leaflet::leafletOutput("map", height='600px', width="100%"),size=2, color="#0080b7")
			)
		)
	)
	
	server=function(input, output, session){
	
		reactive_objects=reactiveValues()		
	
		output$map=renderLeaflet({			
				wqTools::baseMap() %>% wqTools::addMapResetButton() %>% addMapPane("highlight", zIndex = 418) %>% 
					leaflet::addLegend("topright", colors=c("purple","orange","blue"), labels=c("Monitoring location", "Permit", "USGS gauge"))
					#leaflet.extras::addDrawToolbar(
					#	polylineOptions = F,
					#	polygonOptions = F,
					#	circleOptions = F,
					#	rectangleOptions = drawRectangleOptions(),
					#	markerOptions = F,
					#	circleMarkerOptions = F,
					#	singleFeature = FALSE
					#)
		})
		
		## Search for sites
		observeEvent(input$query_sites, {
			req(input$map_bounds)
			shinybusy::show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Querying...", session = shiny::getDefaultReactiveDomain())	
			map_box=input$map_bounds
			
			## Add feature - jitter coords if duplicated ##
			
			if("Monitoring locations" %in%  input$data_types){
				bbox=paste(map_box[4], map_box[3], map_box[2], map_box[1], sep='%2C')
				suppressMessages({
					sites=wqTools::readWQP(type='sites', bBox=bbox, siteType=c("Lake, Reservoir, Impoundment","Stream","Spring","Facility"))
					act=wqTools::readWQP(type='activity', bBox=bbox, siteType=c("Lake, Reservoir, Impoundment","Stream","Spring","Facility"))
				})
				act=subset(act, MonitoringLocationIdentifier %in% sites$MonitoringLocationIdentifier)
				#if(dim(sites)[1]>0){
					visits=unique(act[,c("MonitoringLocationIdentifier","ActivityStartDate")])
					visit_counts=aggregate(ActivityStartDate~MonitoringLocationIdentifier, visits, FUN='length')
					names(visit_counts)[names(visit_counts)=='ActivityStartDate']="count"
					sites_counts=merge(sites, visit_counts)
					reactive_objects$wqp_sites=sites_counts
				#}else{
				#	reactive_objects$wqp_sites=sites
				#}
			}
			if("USGS gauges" %in%  input$data_types){
				reactive_objects$gauges=NULL
				try(reactive_objects$gauges <- 
				
				
				dataRetrieval::readNWISdata(bBox=c(round(map_box[[4]],7), round(map_box[[3]],7), round(map_box[[2]],7), round(map_box[[1]],7)), service="site", siteTypeCd="ST", hasDataTypeCd="iv"))
				
				
				
				
				
			}
			if("Permits" %in% input$data_types){
				permits_coords=wasteloadR::permits_coords
				reactive_objects$permits=subset(permits_coords, LongitudeMeasure>=map_box[[4]] & LatitudeMeasure>=map_box[[3]] & LongitudeMeasure<=map_box[[2]] & LatitudeMeasure<=map_box[[1]])				
			}
			shinybusy::remove_modal_spinner()
		})
		
		
		## Collect queried sites
		observe({
			#req(reactive_objects$wqp_sites, reactive_objects$permits, reactive_objects$gauges)
			
			map_sites=data.frame(matrix(nrow=0, ncol=8))
			names(map_sites)=c("site_id", "site_name", "org", "lat", "long","type", "visit_count", "color")
			
			if(!is.null(reactive_objects$wqp_sites)){
				req(input$visit_count_slider, input$orgs, input$site_types)
				wqp_sites=subset(reactive_objects$wqp_sites, count>=input$visit_count_slider & OrganizationIdentifier %in% input$orgs & MonitoringLocationTypeName %in% input$site_types)
				wqp_sites=dplyr::rename(wqp_sites, site_id=MonitoringLocationIdentifier, site_name=MonitoringLocationName, lat=LatitudeMeasure, long=LongitudeMeasure, org=OrganizationIdentifier, visit_count=count)
				#if(any(names(map_sites)=="count")){map_sites=dplyr::rename(map_sites, visit_count=count)}else{map_sites$count=NA}
				wqp_sites$type="Monitoring location"
				wqp_sites$color="purple"
				wqp_sites=wqp_sites[,c("site_id", "site_name", "org", "lat", "long","type", "visit_count", "color")]
				map_sites=plyr::rbind.fill(map_sites, wqp_sites)
			}

			if(!is.null(reactive_objects$gauges)){
				gauges=reactive_objects$gauges
				gauges=dplyr::rename(gauges, site_id=site_no, site_name=station_nm, lat=dec_lat_va, long=dec_long_va, org=agency_cd)
				gauges$type="Gauge"
				gauges$color="blue"
				gauges=gauges[,c("site_id", "site_name", "org", "lat", "long","type", "color")]
				map_sites=plyr::rbind.fill(map_sites, gauges)
			}
			
			if(!is.null(reactive_objects$permits)){
				if(dim(reactive_objects$permits)[1]>0){
					permits=reactive_objects$permits
					permits=dplyr::rename(permits, site_id=permit_id, site_name=permit_name, lat=LatitudeMeasure, long=LongitudeMeasure)
					permits$type="Permit"
					permits$color="orange"
					permits=permits[,c("site_id", "site_name", "lat", "long","type", "color")]
					map_sites=plyr::rbind.fill(map_sites, permits)
				}
			}
			
			if(dim(map_sites)[1]>0){
				map_sites=within(map_sites, {
					lab=paste0(
						'<p>',
						'<br />', "Site ID: ", site_id,
						'<br />', "Site name: ", site_name,
						'<br />', "Site type: ", type,
						'<br />', "Visit count: ", visit_count)
				})
				reactive_objects$map_sites=map_sites
			}
		})
				
		## WQP site options
		### Site types
		output$site_types=renderUI({
			req(reactive_objects$wqp_sites)
			types=unique(subset(reactive_objects$wqp_sites, OrganizationIdentifier %in% input$orgs)$MonitoringLocationTypeName)
			types=types[order(types)]
			shinyWidgets::multiInput('site_types', 'Site types:', choices=types, selected=types, width="100%")	
		})
			
		### Organizations
		output$orgs=renderUI({
			req(reactive_objects$wqp_sites)
			orgs=unique(reactive_objects$wqp_sites$OrganizationIdentifier)
			orgs=orgs[order(orgs)]
			shinyWidgets::multiInput('orgs', "Organizations:", choices=orgs,
				selected=c("UTAHDWQ_WQX"), width="100%")	
		})
	
		### Visit count slider
		output$visit_count_slider=renderUI({
			req(reactive_objects$wqp_sites)
			suppressWarnings({
				min_visit=min(subset(reactive_objects$wqp_sites, MonitoringLocationTypeName %in% input$site_types & OrganizationIdentifier %in% input$orgs)$count)
				max_visit=max(subset(reactive_objects$wqp_sites, MonitoringLocationTypeName %in% input$site_types & OrganizationIdentifier %in% input$orgs)$count)
				sliderInput("visit_count_slider", "Min visit count:", min=min_visit, max=max_visit, value=min_visit)	
			})
		})


		## Zoom to coordinates
		observeEvent(input$zoom, {
			leaflet::leafletProxy("map") %>% leaflet::flyTo(input$lon, input$lat, zoom=12)
		})
		
		## Plot queried sites on map
		observe({
			req(reactive_objects$map_sites)
			map_sites=reactive_objects$map_sites
			
			# Subset for WQP visit counts
			
			leafletProxy("map") %>% clearGroup("sites") %>%
				leaflet::addCircleMarkers(data=map_sites, lat=~lat, lng=~long, options = pathOptions(pane = "markers"), group="sites",
					color = ~color, opacity=0.8, layerId=~site_id,
					label = lapply(map_sites$lab, HTML) # crashes if only one site is selected for lapply
				)
		})

		## Query WQ data UI
		output$sel_all=renderUI({
			req(reactive_objects$map_sites)
			actionButton('sel_all', 'Select all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('plus'))
		})

		output$clear_all=renderUI({
			req(reactive_objects$map_sites)
			actionButton('clear_all', 'Clear all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('trash-alt'))	
		})

		observeEvent(input$sel_all, ignoreInit=TRUE, {
			reactive_objects$sel_sites=reactive_objects$map_sites$site_id		
		})
		observeEvent(input$clear_all, ignoreInit=TRUE, {
			reactive_objects$sel_sites=NA
		})
	
		
		### Select sites for WQ data query
		output$sel_sites_multiInput=renderUI({
			req(reactive_objects$map_sites)
			shinyWidgets::multiInput("sel_sites_input", "Select sites (or map click):", choices=reactive_objects$map_sites$site_id, selected=reactive_objects$sel_sites, width="100%")	
		})
	
	
		### Select sites on map click
		observeEvent(input$map_marker_click, {
			mlid = input$map_marker_click$id
			if(!is.null(mlid)){
				if(mlid %in% reactive_objects$sel_sites){
					reactive_objects$sel_sites=reactive_objects$sel_sites[!reactive_objects$sel_sites %in% mlid]
				}else{
					reactive_objects$sel_sites=append(reactive_objects$sel_sites, mlid)
				}
			}
		})
		
	
		### Update sel_sites w/ multiInput
		observeEvent(input$sel_sites_input, ignoreNULL=F, {
			reactive_objects$sel_sites=input$sel_sites_input
		})

		### Update map site highlight
		observeEvent(reactive_objects$sel_sites, ignoreNULL = F, ignoreInit=T, {
			leafletProxy("map") %>%
				clearGroup(group='highlight') %>%
				addCircleMarkers(data=subset(reactive_objects$map_sites, site_id %in% reactive_objects$sel_sites), lat=~lat, lng=~long,
					group='highlight', options = pathOptions(pane = "highlight"), radius = 20, color='chartreuse', opacity = 0.75, fillOpacity = 0.4)
		})		
		
		## Export selected sites
		output$export=renderUI({
			req(reactive_objects$sel_sites)
			actionButton('export', 'Export sites', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('file-export'))
		})
	
		observeEvent(input$export, {
			sites=subset(reactive_objects$map_sites, site_id %in% reactive_objects$sel_sites)
			sites=sites[,!names(sites) %in% c("color","lab")]
			stopApp(sites)
		})
	}

	runGadget(ui, server, viewer = browserViewer())

}

#findSites()









		## Drawing toolbar observers
		#observeEvent(input$map_marker_click, {
		#	print(input$map_marker_click)
		#})  
		#observeEvent(input$map_draw_new_feature, {
		#	print(input$map_draw_new_feature)
		#})












		# Observe map1 drawing features
		#observeEvent(input$map1_draw_new_feature,{
		#	draw=input$map1_draw_new_feature
		#	if(draw$geometry$type=='Point'){
		#		reactive_objects$pin=c(draw$geometry$coordinates[[2]],draw$geometry$coordinates[[1]])
		#	}else{
		#		#print(draw$geometry$coordinates)
		#		reactive_objects$bbox=c(
		#			draw$geometry$coordinates[[1]][[1]][[1]], #W
		#			draw$geometry$coordinates[[1]][[1]][[2]], #S
		#			draw$geometry$coordinates[[1]][[3]][[1]], #E
		#			draw$geometry$coordinates[[1]][[3]][[2]]  #N
		#		)
		#		#print(bbox())
		#	}		
		#	#within=10&lat=-111.6670281&long=40.0089953
		#})
	
		
		## Map1 click
		#observeEvent(input$map1_shape_click,{
		#	click = input$map1_shape_click$id
		#	if(!is.null(click)){
		#		sel_id=as.character(unique(reactive_objects$poly$id