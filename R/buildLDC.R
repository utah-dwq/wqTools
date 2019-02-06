#' Calculate parameter loading capacity
#' 
#' Uses flow, parameter concentration, and standard values to determine observed loading and loading capacity
#' 
#' @param data A data frame containing columns indicating flow and parameter concentration (required for plotting: location and parameter name).
#' @param flow String indicating name of column containing flow data
#' @param value String indicating name of column containing parameter concentration data
#' @param location String indicating column name containing location name(s)
#' @param parameter String indicating column name containing parameter name
#' @param crit A numeric value representing the standard criterion (in same concentration units as parameter) against which observed loadings are compared
#' @param loading_units A character string indicating the loading units (amount/time) to be plotted on the y-axis of the LDC (if plot_it = TRUE).
#' @param mos A decimal representing the percent margin of safety to apply to the loading capacity for management decisions.
#' @param cf A numeric value representing the correction factor linking flow and parameter concentration to desired unit (load per time).
#' @param plot_it Logical. If TRUE, plots observed capacity and loading capacity in one figure in a load duration curve framework.

#### TESTING ######
# wb.dat <- loadWorkbook(wb_path_new)
# loading.dat <- readWorkbook(wb.dat, sheet="LDC_Data")
# new.dat <- loading.dat[,!names(loading.dat)%in%c("loading.capacity","loading.capacity.mos","observed.loading","exceedance","flow.percentile")]
# new.dat$Date <- as.Date(new.dat$Date, origin="1899-12-30")
# new.dat$Parameter = "E.coli"
# 
# test <- ldc_func(x=new.dat,flow="Flow.cfs",value="E.coli",location="MLID",parameter = "Parameter",crit=126,loading_units = "MPN/day", cf=1000/100*28.3168*3600*24, plot_it = TRUE)

ldc_func <- function(x, flow, value, location, parameter, crit, loading_units, mos=.1,cf, plot_it = TRUE){
  
  flow_perc <- function(x){flow.percentile = (1-percent_rank(x))*100}
  # Calculate loadings
  data = x
  data$loading.capacity <- data[,flow]*crit*cf
  data$loading.capacity.mos <- data$loading.capacity*(1-mos)
  data$observed.loading <- data[,flow]*data[,value]*cf
  data$exceedance <- ifelse(data$observed.loading>data$loading.capacity.mos,"yes","no")
  data$flow.percentile = flow_perc(data[,flow])
  if(plot_it){
    flow.plot <- data[order(data$flow.percentile),]
    #windows()
    quartz()
    plot(1, type="n", xlab="Flow Exceedance Percentile", ylab=paste0(flow.plot[,parameter][1], " Loading ",loading_units), xlim=c(0, 100), ylim=c(min(flow.plot$observed.loading),max(flow.plot$observed.loading)), main=paste("Load Duration Curve:",flow.plot[,location][1]))
    abline(v=10, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=90, lty=2)
    text(5, max(flow.plot$observed.loading)-.2*max(flow.plot$observed.loading),"High \n Flows")
    text(25, max(flow.plot$observed.loading)-.2*max(flow.plot$observed.loading),"Moist \n Conditions")
    text(50, max(flow.plot$observed.loading)-.2*max(flow.plot$observed.loading),"Mid-Range \n Flows")
    text(75, max(flow.plot$observed.loading)-.2*max(flow.plot$observed.loading),"Dry \n Conditions")
    text(95, max(flow.plot$observed.loading)-.2*max(flow.plot$observed.loading),"Low \n Flows")
    lines(flow.plot$loading.capacity~flow.plot$flow.percentile, type="l", col="blue", lwd=2)
    lines(flow.plot$loading.capacity.mos~flow.plot$flow.percentile, col="green", lwd=2)
    points(flow.plot$observed.loading~flow.plot$flow.percentile, pch=21, col="black", bg="purple", cex=1.5)
    legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", paste("Observed",flow.plot[,parameter][1],"Loading")), bty="n", col=c("blue","green","black"), lty=c(1,1,NA),lwd=c(2,2,NA),pch=c(NA,NA,21), pt.bg=c(NA,NA,"purple"), cex=1)
  }
  return(data)
}