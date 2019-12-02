#' Run the Water Quality Data Dashboard

#' Runs the Water Quality Data Dashboard app embedded in UT WQ tools package.

#' @import shiny
#' @import shinyBS
#' @import leaflet
#' @importFrom jsonlite fromJSON
#' @import plotly


#' @export
wqDD=function(){
	shiny::runApp(system.file('wqDD', package='wqTools'))
}
