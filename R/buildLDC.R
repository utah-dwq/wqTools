#' Calculate parameter loading capacity
#' 
#' Uses flow, parameter concentration, and standard values to determine observed loading and loading capacity
#' 
#' @param x A data frame containing columns indicating flow and parameter concentration for given dates (required for plotting: location and parameter name). Note that flow data for which no parameter values exist are still used to construct LDC plot.
#' @param flow String. Column name containing flow data.
#' @param date String. Column name containing date data.
#' @param value String. Column name containing parameter concentration data.
#' @param location String Column name containing location name(s).
#' @param parameter String. Column name containing parameter name.
#' @param crit Numeric.  Represents the standard criterion (in same concentration units as parameter) against which observed loadings are compared.
#' @param loading_units String. Indicates the loading units (amount/time) to be plotted on the y-axis of the LDC (if plot_it = TRUE).
#' @param mos Numeric. A decimal representing the percent margin of safety to apply to the loading capacity for management decisions.
#' @param cf Numeric. A value representing the correction factor linking flow and parameter concentration to desired unit (load per time).
#' @return A data frame containing original columns supplied to function plus observed loading, loading capacity, loading capacity plus margin of safety, season, and flow percentile.
#' @param plot_it Logical. If TRUE, plots observed capacity and loading capacity in one figure in a load duration curve framework.
#' @import colorspace
#' @export buildLDC
#' 
#### TESTING ######
# wb.dat <- read.csv("C:\\Users\\ehinman\\Desktop\\Spring_Creek_ex.csv")
# wb.dat$Date <- as.Date(wb.dat$ActivityStartDate, "%m/%d/%Y")
# wb.dat$Parameter = "E.coli"
# 
buildLDC <- function(x, flow, date, value, location, parameter, crit, loading_units, mos=.1,cf, plot_it = TRUE){
  
  flow_perc <- function(x){(1-percent_rank(x))*100}
  
  # Determine season for LDC - taken from https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))}
  
  # Calculate loadings
  data = x
  data$Date <- data[,date]
  data$Season <- getSeason(data$Date)
  data$Loading.Capacity <- data[,flow]*crit*cf
  data$Loading.Capacity.mos <- data$Loading.Capacity*(1-mos)
  data$Observed.Loading <- data[,flow]*data[,value]*cf
  data$Exceedance <- ifelse(data$Observed.Loading>data$Loading.Capacity.mos,"yes","no")
  data$Flow.Percentile = flow_perc(data[,flow])
  if(plot_it){
    # Order flow data
    flow.plot <- data[order(data$Flow.Percentile),]
    # Define seasonal color scheme using colorspace
    colpal <- sequential_hcl(4)
    # Pull out observed loadings (E.coli data)
    loads <- data[!is.na(data[,value]),]
    spre <- loads[loads$Season=="Spring",]
    sume <- loads[loads$Season=="Summer",]
    fale <- loads[loads$Season=="Fall",]
    wine <- loads[loads$Season=="Winter",]
    windows()
    plot(1, type="n", xlab="Flow Exceedance Percentile", ylab=paste(data[,parameter][1], "Load", loading_units), xlim=c(0, 100), ylim=c(0,max(loads$Observed.Loading)), main=paste("Load Duration Curve:",data[,location][1]))
    abline(v=10, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=90, lty=2)
    text(5, max(loads$Observed.Loading)-.3*max(loads$Observed.Loading),"High \n Flows")
    text(25, max(loads$Observed.Loading)-.3*max(loads$Observed.Loading),"Moist \n Conditions")
    text(50, max(loads$Observed.Loading)-.3*max(loads$Observed.Loading),"Mid-Range \n Flows")
    text(75, max(loads$Observed.Loading)-.3*max(loads$Observed.Loading),"Dry \n Conditions")
    text(95, max(loads$Observed.Loading)-.3*max(loads$Observed.Loading),"Low \n Flows")
    lines(flow.plot$Loading.Capacity~flow.plot$Flow.Percentile, type="l", col="firebrick3", lwd=2)
    lines(flow.plot$Loading.Capacity.mos~flow.plot$Flow.Percentile, col="red", lwd=2)
    points(spre$Observed.Loading~spre$Flow.Percentile, pch=21, col="black", bg=colpal[1], cex=1.5)
    points(sume$Observed.Loading~sume$Flow.Percentile, pch=21, col="black", bg=colpal[2], cex=1.5)
    points(fale$Observed.Loading~fale$Flow.Percentile, pch=21, col="black", bg=colpal[3], cex=1.5)
    points(wine$Observed.Loading~wine$Flow.Percentile, pch=21, col="black", bg=colpal[4], cex=1.5)
    legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", paste(data[,parameter][1],"- Spring"), paste(data[,parameter][1],"- Summer"),paste(data[,parameter][1],"- Fall"), paste(data[,parameter][1],"- Winter")), bty="n", col=c("firebrick3","red","black","black","black","black"), lty=c(1,1,NA,NA,NA,NA),lwd=c(2,2,NA,NA,NA,NA),pch=c(NA,NA,21,21,21,21), pt.bg=c(NA,NA,colpal), cex=1)
  }
  return(data)
}