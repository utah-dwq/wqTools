# Figures module

figuresModUI <- function(id){
	ns <- NS(id)
	tagList(	
	
		tags$style(type = "text/css", "html, body {width:100%;height:100%}",
			".leaflet .legend i{
			border-radius: 50%;
			width: 10px;
			height: 10px;
			margin-top: 4px;
			}"
		),
		
		fluidRow(
			column(2,uiOutput(ns('sel_param1')), uiOutput(ns('sel_units1')), uiOutput(ns('sel_frac1'))),
			conditionalPanel(paste0("input['", ns("tabs"),"'] == 'Multiple parameters'"),
				column(2,
					uiOutput(ns('sel_param2')),
					uiOutput(ns('sel_units2')),
					uiOutput(ns('sel_frac2'))
				),
				column(3,
					uiOutput(ns('sel_site'))
				)
			)
		),
		tabsetPanel(id=ns('tabs'),
			tabPanel('Multiple sites',
				fluidRow(column(4,radioButtons(ns("compare_plottype"), "Plot type", choices = c("Time series","Boxplot", "Concentration map"), selected = "Time series", inline = TRUE))),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Time series'"), plotlyOutput(ns('multi_site_ts'), height='600px')),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Boxplot'"), plotlyOutput(ns('multi_site_bp'), height='600px')),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Concentration map'"), shinycssloaders::withSpinner(leafletOutput(ns('conc_map'), height='600px', width="100%"),size=2, color="#0080b7"))
			),
			tabPanel("Multiple parameters", 
				plotlyOutput(ns("multi_param_ts"))
			)
		)
	)
}


figuresMod <- function(input, output, session, sel_data){
	
	
	# Empty reactive objects
	reactive_objects=reactiveValues()

	# Get data & format
	observe({
		req(sel_data())
		sel_data=sel_data()
		sel_data$ActivityStartDate=as.Date(sel_data$ActivityStartDate)
		sel_data$ResultMeasure.MeasureUnitCode=toupper(sel_data$ResultMeasure.MeasureUnitCode)
		reactive_objects$sel_data=sel_data
	})
	
	# Select param 1
	output$sel_param1 <- renderUI({
		ns <- session$ns
		req(reactive_objects$sel_data$CharacteristicName)
		selectInput(ns("sel_param1"),"Select parameter 1", choices = unique(reactive_objects$sel_data$CharacteristicName[order(reactive_objects$sel_data$CharacteristicName)]))
	})
	
	observe({
		req(input$sel_param1)
		reactive_objects$param1_sub=reactive_objects$sel_data[reactive_objects$sel_data$CharacteristicName == input$sel_param1,]
	})
	
	observe({
		req(reactive_objects$param1_sub)
		reactive_objects$units1=unique(reactive_objects$param1_sub$ResultMeasure.MeasureUnitCode)
		reactive_objects$fractions1=unique(reactive_objects$param1_sub$ResultSampleFractionText)
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		req(reactive_objects$units1)
		ns <- session$ns
		units=reactive_objects$units1
		selectInput(ns("sel_units1"),"Select units 1", choices = units, selected="")
	})
	
    
	observe({
		req(reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_units1'), selected="")
	})
    
	# Select fraction 1
	output$sel_frac1 <- renderUI({
		req(reactive_objects$fractions1)
		ns <- session$ns
		selectInput(ns("sel_frac1"),"Select fraction 1", choices = reactive_objects$fractions1, selected="")
	})
    
	observe({
		req(reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_frac1'), selected="")
	})
    
	
	# Select param 2
	output$sel_param2 <- renderUI({
		ns <- session$ns
		param_choices=reactive_objects$sel_data$CharacteristicName[! reactive_objects$sel_data$CharacteristicName %in% input$sel_param1]
		selectInput(ns("sel_param2"),"Select parameter 2", choices = param_choices[order(param_choices)])
	})
    
	# Select units 2
	output$sel_units2 <- renderUI({
		ns <- session$ns
		units=unique(reactive_objects$sel_data[reactive_objects$sel_data$CharacteristicName == input$sel_param2, 'ResultMeasure.MeasureUnitCode'])
		selectInput(ns("sel_units2"),"Select units 2", choices = units)
	})

	# Select fraction 2
	output$sel_frac2 <- renderUI({
		ns <- session$ns
		fraction=unique(reactive_objects$sel_data[reactive_objects$sel_data$CharacteristicName == input$sel_param2, 'ResultSampleFractionText'])
		selectInput(ns("sel_frac2"),"Select fraction 2", choices = fraction)
	})

	observe({
		req(reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_frac2'), selected="")
	})
	
	# Generate parameter 1 data
	observe({
		req(input$sel_param1, input$sel_units1, input$sel_frac1)
			## Data
			param1=subset(reactive_objects$sel_data, CharacteristicName == input$sel_param1 & ResultSampleFractionText==input$sel_frac1)
			if(dim(param1)[1]>0){
				param1$target_unit=input$sel_units1
				param1=wqTools::convertUnits(param1, input_units='ResultMeasure.MeasureUnitCode', target_units = "target_unit", value_var='ResultMeasureValue', conv_val_col='plot_value')
				param1=param1[order(param1$ActivityStartDate),]
				reactive_objects$param1=droplevels(unique(param1[,c('MonitoringLocationIdentifier','ActivityStartDate','LatitudeMeasure','LongitudeMeasure','CharacteristicName','plot_value','target_unit','MonitoringLocationName','ResultSampleFractionText','ASSESS_ID','AU_NAME','AU_Type')]))
			}
	})
    
	# Generate parameter 2 data
	observe({if(input$tabs=='Multiple parameters'){
		req(input$sel_param2, input$sel_units2, input$sel_frac2)
		param2=subset(reactive_objects$sel_data, CharacteristicName == input$sel_param2 & ResultSampleFractionText==input$sel_frac2)
		if(dim(param2)[1]>0){
			param2$target_unit=input$sel_units2
			param2=wqTools::convertUnits(param2, input_units='ResultMeasure.MeasureUnitCode', target_units = "target_unit", value_var='ResultMeasureValue', conv_val_col='plot_value')
			param2=param2[order(param2$ActivityStartDate),]
			reactive_objects$param2=unique(param2[,c('MonitoringLocationIdentifier','ActivityStartDate','LatitudeMeasure','LongitudeMeasure','CharacteristicName','plot_value','target_unit','MonitoringLocationName','ResultSampleFractionText','ASSESS_ID','AU_NAME','AU_Type')])
		}
	}})
	
	# Multi-site figure labels & visibilities
	observe({
		req(reactive_objects$param1)
		reactive_objects$title = input$sel_param1
		reactive_objects$ylab = paste0(input$sel_param1,' (', input$sel_units1,')')
		reactive_objects$ylab2 = paste0(input$sel_param2,' (', input$sel_units2,')')
		mlid_len=length(unique(reactive_objects$param1$MonitoringLocationIdentifier))
		au_len=length(unique(reactive_objects$param1$ASSESS_ID))
		reactive_objects$mlid_vis=as.list(append(rep(T, mlid_len), rep(F, au_len)))
		reactive_objects$au_vis=as.list(append(rep(F,mlid_len), rep(T, au_len)))
	})
    
	# Multi-site time series
	multi_site_ts=reactive({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$au_vis)				
		#if(all(!is.na(reactive_objects$param1$plot_value))){
			multi_site_ts=plot_ly(source="a") %>%
				add_trace(data=reactive_objects$param1, type = 'scatter', mode = 'lines+markers', x=~as.Date(ActivityStartDate), y=~plot_value, color = ~droplevels(MonitoringLocationIdentifier), marker = list(size=10), visible=T, text=~MonitoringLocationIdentifier) %>%
				add_trace(data=reactive_objects$param1, type = 'scatter', mode = 'markers',x = ~as.Date(ActivityStartDate), y=~plot_value, color = ~droplevels(ASSESS_ID), marker = list(size=10), visible=F, text=~ASSESS_ID)
			multi_site_ts=layout(multi_site_ts,
							title = reactive_objects$title,
							xaxis = list(title = "Date"),
							yaxis = list(title = reactive_objects$ylab),
						updatemenus = list(
							list(
								buttons = list(
									list(method = "update", label='Group by site', 
										args = list(list(visible = reactive_objects$mlid_vis))
									),
									list(method = "update", label='Group by AU', 
										args = list(list(visible = reactive_objects$au_vis))
									)
								)
							)
						)
					) %>% 
				config(displaylogo = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'lasso2d'
					)
				)
	})

	output$multi_site_ts=renderPlotly({
		multi_site_ts()
	})
    	
	# Multi site boxplot
	multi_site_bp=reactive({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$au_vis, reactive_objects$title, reactive_objects$ylab, reactive_objects$mlid_vis, reactive_objects$au_vis)
		plot_ly(data=reactive_objects$param1, type = 'box', y = ~plot_value, color = ~droplevels(MonitoringLocationIdentifier), visible=T) %>%
			add_trace(type = 'box', y = ~plot_value, color = ~droplevels(ASSESS_ID), visible=F) %>%
			layout(
				title = reactive_objects$title,
				xaxis = list(title = "Monitoring location ID"),
				xaxis2 = list(overlaying = "x", zeroline=F, showticklabels = FALSE, showgrid = FALSE),
				yaxis = list(title = reactive_objects$ylab),
				updatemenus = list(
					list(
						buttons = list(
							list(method = "update", label='Group by site', 
								args = list(list(visible = reactive_objects$mlid_vis), list(xaxis = list(title = 'Monitoring location ID')))
							),
							list(method = "update", label='Group by AU', 
								args = list(list(visible = reactive_objects$au_vis), list(xaxis = list(title = 'Assessment unit ID')))
							)
						)
					)
				)
			) %>%
			config(displaylogo = FALSE,
				modeBarButtonsToRemove = c(
					'sendDataToCloud',
					'select2d',
					'lasso2d'
				)
			)
	})

	output$multi_site_bp=renderPlotly({
		multi_site_bp()
	})
    
	
	# Concentration map		
	conc_map = wqTools::buildMap(plot_polys=T, search="")
	conc_map=conc_map%>%clearGroup('Sites') %>% clearControls()
	conc_map = leaflet::addLayersControl(conc_map,
		position ="topleft",
		baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites"),
		options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
	conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
	conc_map=hideGroup(conc_map, "Assessment units")
	conc_map=hideGroup(conc_map, "Site-specific standards")
	conc_map=hideGroup(conc_map, "Beneficial uses")
	conc_map=removeMeasure(conc_map)
    
	output$conc_map <- leaflet::renderLeaflet({
		isolate({
			if(!is.null(reactive_objects$param1) & dim(reactive_objects$param1)[1]>0){
				sites=reactive_objects$param1
				sites=sites[!is.na(sites$plot_value),]
				count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
				names(count)[names(count)=='plot_value'] = 'count'
				sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean', na.rm=TRUE)
				sites=merge(sites,count,all.x=T)
				sites$radius=scales::rescale(sites$plot_value, c(5,35))
				min_lat=min(sites$LatitudeMeasure)*0.999
				min_lng=min(sites$LongitudeMeasure)*0.999
				max_lat=max(sites$LatitudeMeasure)*1.001
				max_lng=max(sites$LongitudeMeasure)*1.001
				leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
				leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
				conc_map = conc_map %>% fitBounds(min_lng,min_lat,max_lng,max_lat) %>%	
					addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
						radius = ~radius, options = pathOptions(pane = "site_markers"),
						popup = paste0(
							"MLID: ", sites$MonitoringLocationIdentifier,
							"<br> ML name: ", sites$MonitoringLocationName,
							"<br> Average Parameter Value: ", sites$plot_value,
							"<br> Sample Count: ", sites$count)
					) %>%
				addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
			}
		})

		conc_map
	})
		
    conc_map_output=reactive({
		conc_map_output = wqTools::buildMap(plot_polys=FALSE, search="")
		conc_map_output=conc_map_output%>%clearGroup('Sites') %>% clearControls()
		conc_map_output = leaflet::addLayersControl(conc_map_output,
			position ="topleft",
			baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites"),
			options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
		conc_map_output=addMapPane(conc_map_output,"site_markers", zIndex = 450)
		conc_map_output=hideGroup(conc_map_output, "Assessment units")
		conc_map_output=hideGroup(conc_map_output, "Site-specific standards")
		conc_map_output=hideGroup(conc_map_output, "Beneficial uses")
		conc_map_output=removeMeasure(conc_map_output)
			if(!is.null(reactive_objects$param1) & dim(reactive_objects$param1)[1]>0){
				sites=reactive_objects$param1
				sites=sites[!is.na(sites$plot_value),]
				count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
				names(count)[names(count)=='plot_value'] = 'count'
				sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean', na.rm=TRUE)
				sites=merge(sites,count,all.x=T)
				sites$radius=scales::rescale(sites$plot_value, c(5,35))
				min_lat=min(sites$LatitudeMeasure)*0.999
				min_lng=min(sites$LongitudeMeasure)*0.999
				max_lat=max(sites$LatitudeMeasure)*1.001
				max_lng=max(sites$LongitudeMeasure)*1.001
				leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
				leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
				conc_map_output = conc_map_output %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
					addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
						radius = ~radius, options = pathOptions(pane = "site_markers"),
						popup = paste0(
							"MLID: ", sites$MonitoringLocationIdentifier,
							"<br> ML name: ", sites$MonitoringLocationName,
							"<br> Average Parameter Value: ", sites$plot_value,
							"<br> Sample Count: ", sites$count)
					) %>%
				addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
			}
		conc_map_output
	})
	
	# Map proxy
	conc_proxy = leaflet::leafletProxy("conc_map")
   
	# Custom leaflet legend (re-size circles)
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
		colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
		labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
		return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
	}
	
    
	# Update concentration map via proxy on param1 change
	observeEvent(reactive_objects$param1, {
		#req(reactive_objects$param1, reactive_objects$ylab)
		conc_proxy%>%clearGroup('Sites') %>% clearControls()
		if(exists('sites')){rm(sites)}
		if(any(!is.na(reactive_objects$param1$plot_value))){
			sites=reactive_objects$param1
			sites=sites[!is.na(sites$plot_value),]
			count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
			names(count)[names(count)=='plot_value'] = 'count'
			sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean')
			sites=merge(sites,count,all.x=T)
			sites$radius=scales::rescale(sites$plot_value, c(5,35))
			min_lat=min(sites$LatitudeMeasure)*0.999
			min_lng=min(sites$LongitudeMeasure)*0.999
			max_lat=max(sites$LatitudeMeasure)*1.001
			max_lng=max(sites$LongitudeMeasure)*1.001
			leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
			leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
			conc_proxy %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
				addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
					radius = ~radius, options = pathOptions(pane = "site_markers"),
					popup = paste0(
						"MLID: ", sites$MonitoringLocationIdentifier,
						"<br> ML name: ", sites$MonitoringLocationName,
						"<br> Average Parameter Value: ", sites$plot_value,
						"<br> Sample Count: ", sites$count)
				) %>%
			addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
		}
	})
	
	
	# Multi-parameter time series
	## Site selection
	output$sel_site <- renderUI({
		req(reactive_objects$param1,reactive_objects$param2)
		param_site = as.character(unique(reactive_objects$param1$MonitoringLocationIdentifier[reactive_objects$param1$MonitoringLocationIdentifier %in% reactive_objects$param2$MonitoringLocationIdentifier]))
		ns <- session$ns
		selectInput(ns("sel_site"),"Select Site", choices = param_site)
	})
    
	output$multi_param_ts=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, input$sel_frac1, reactive_objects$param2, input$sel_units2, input$sel_frac2, input$sel_site)
		param1=reactive_objects$param1[reactive_objects$param1$MonitoringLocationIdentifier %in% input$sel_site,]
		param2=reactive_objects$param2[reactive_objects$param2$MonitoringLocationIdentifier %in% input$sel_site,]
		#if(all(!is.na(reactive_objects$param1$plot_value)) & all(!is.na(reactive_objects$param2$plot_value))){
			plot_ly(type = 'scatter', mode = 'lines+markers')%>%
				layout(title = input$sel_site,
					font = list(
					family = "Arial, sans-serif"),
					xaxis = list(title = "Date"),
					yaxis = list(title = reactive_objects$ylab),
					yaxis2 = list(side="right", overlaying = "y",title = reactive_objects$ylab2)
				) %>% 
				add_trace(data=param1, x = ~ActivityStartDate, y = ~plot_value, name = reactive_objects$ylab, marker = list(size = 10)) %>%
				add_trace(data=param2, x = ~ActivityStartDate, y = ~plot_value, name = reactive_objects$ylab2, marker = list(size = 10), yaxis = "y2") %>%
				config(displaylogo = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'lasso2d',
						'select2d'
					)
				)
		#}
	})

	return(list(
		select_data=reactive({event_data("plotly_selected", source="a")}),
		param1=reactive({input$sel_param1}),
		param_choices=reactive({
				req(reactive_objects$sel_data)
				unique(reactive_objects$sel_data$CharacteristicName[order(reactive_objects$sel_data$CharacteristicName)])
			}),
		multi_site_ts=multi_site_ts,
		multi_site_bp=multi_site_bp,
		conc_map=conc_map_output
	))
	
}
