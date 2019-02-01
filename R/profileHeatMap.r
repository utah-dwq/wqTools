#' Profile heat map plot
#'
#' Plots a lake profile heatmap for a single site and parameter.
#' @param data Lake profile data (wide format)
#' @param date Date column name. Must be in 'YYYY-mm-dd' format.
#' @param parameter Column name for parameter to be used as z-values.
#' @param param_units Character. Parameter units. Used to plot build label.
#' @param param_lab Character. Label to be used for parameter name. Used to build plot label.
#' @param depth Column name for depth column.
#' @param depth_units Character. Depth units. Used to plot build label.
#' @param min_date Minimum plot date. 'YYYY-mm-dd' format.
#' @param max_date Maximum plot date. 'YYYY-mm-dd' format.
#' @param show_dates Logical. If TRUE (default), show individual profile dates on plot x-axis.
#' @param criteria Vector of criteria values to be used as contours on heatmap plot. If not specified, contours from 0-30 at increments of 5 are drawn.
#' @importFrom akima interp
#' @importFrom lubridate origin

#' @export
profileHeatMap=function(data, parameter, depth="Depth_m", param_units, depth_units="m", date="ActivityStartDate", show_dates=TRUE, min_date=min(data[,date], na.rm=T), max_date=max(data[,date], na.rm=T), param_lab="pH", criteria){

	##############
	#load("assessed_profs.rdata")
	#data=assessed_profs$profiles_wide[assessed_profs$profiles_wide$IR_MLID=="UTAHDWQ_WQX-5914010",]
	#data$ActivityStartDate=as.Date(data$ActivityStartDate)
	#date="ActivityStartDate"
	#parameter="DO_mgL"
	#param_units="deg C"
	#param_lab="Temperature"
	#depth="Depth_m"
	#depth_units="m"
	#show_dates=TRUE
	#min_date=min(data[,date], na.rm=T)
	#max_date=max(data[,date], na.rm=T)
	#criteria=c(20,27)
	#profileHeatMap(assessed_profs$profiles_wide[assessed_profs$profiles_wide$IR_MLID=="UTAHDWQ_WQX-5914010",],parameter="Temp_degC",param_units="deg C",param_lab="Temperature",depth="Depth_m",depth_units="m")
	##########
	
	data[,date]=as.Date(data[,date])
	if(missing(criteria)){criteria=c(0,10,15,20,25,30)}
	data=data[!is.na(data[,parameter]),]
	if(any(is.na(data[,parameter]))){
		stop("Missing values in profile for selected parameter")
	}
	
	int=akima::interp(data[,date],data[,depth],data[,parameter],duplicate="strip",linear=TRUE)
	
	if(show_dates==TRUE){
		par(mar=c(8.1,4.1,4.1,2.1))
		filled.contour(int,color.palette=topo.colors,xlim=c(min_date-1,max_date+1),ylim=rev(range(data[,depth])),
			ylab=paste0("Depth (",depth_units,")"), main=paste0(param_lab," (",param_units,")"),
			plot.axes = { contour(int, levels = criteria, drawlabels = TRUE, axes = FALSE, frame.plot = FALSE, add = TRUE);
									axis(1,at=unique(data[,date]),labels=unique(data[,date]),par(las=2)); axis(2) } )
			
	}else{
		filled.contour(int,color.palette=topo.colors,xlim=c(min_date-1,max_date+1),ylim=rev(range(data[,depth])),
			ylab=paste0("Depth (",depth_units,")"), main=paste0(param_lab," (",param_units,")"),
			plot.axes = { contour(int, levels = criteria, 
				drawlabels = TRUE, axes = FALSE, 
                frame.plot = FALSE, add = TRUE);
				axis.Date(side=1,x=as.Date(int$x,origin=lubridate::origin)); axis(2) } )}
}



