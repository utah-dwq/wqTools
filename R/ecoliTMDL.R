#' E. coli data visualization tool for TMDL development
#'
#' Calculates daily, monthly, rec season, and load duration curve data and produces exploratory figures for TMDL development. 
#'
#' @param wb_path Path to an excel spreadsheet with tabs for E.coli data and flow data. Both E.coli and flow data records must contain fields for: MLID, ML_Name, and Date
#' @param geom_crit 30-day geometric mean max criterion in MPN/100 mL.
#' @param max_crit Not to exceed maximum criterion in MPN/100 mL.
#' @param cf Correction factor for conversion between MPN/100 mL & cfs to MPN/day.

#' @return An expanded workbook with tabs containing monthly, rec season, and LDC geometric means.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData

#' @export

library(openxlsx)
library(lubridate)

# Testing
geom_crit <- 206
max_crit <- 668
cf <- 1000/100*28.3168*3600*24
wb_path <- "C:\\Users\\ehinman\\Desktop\\Spring_Creek.xlsx"
wb_path <- "Spring_Creek.xlsx"

ecoliTMDL <- function(wb_path, geom_crit,max_crit,cf=1000/100*28.3168*3600*24){
  
  ## Calculations needed for plotting and assessment ## 
  gmean=function(x){exp(mean(log(x)))} # geometric mean
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  
  ### Load the dataset from the workbook and convert to dates ###
  wb.dat <- loadWorkbook(wb_path)
  ecoli.dat <- readWorkbook(wb.dat,sheet="Ecoli_data",startRow=1)
  ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
  flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
  flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
  
  ###### TIME SERIES -- ENTIRE DATASET BY SITE ######
  # Get dates for axis
  max.dat <- max(ecoli.dat$Date)
  min.dat <- min(ecoli.dat$Date)
  
  # Take geometric mean over same site/day samples
  ecoli.day.gmean <- aggregate(E.coli~Date+ML_Name+MLID, data=ecoli.dat, FUN='gmean')
  
  # Write daily geometric mean data to new datasheet 
  if(!any(wb.dat$sheet_names=="Daily_Geomean_Data")){
    addWorksheet(wb.dat, "Daily_Geomean_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Daily_Geomean_Data", ecoli.day.gmean, rowNames = FALSE)}
  
  # Define number of sites shown in window
  mlid_num <- length(unique(ecoli.day.gmean$MLID))
  quartz()
  par(mfrow=c(mlid_num, 1))
  
  # Plotting function
  # Function to plot MPN over time by site -- add in time period option.
  dat_sum <- function(x){
    max.e <- max(x$E.coli)
    min.e <- min(x$E.coli)
    perc.exc = round(length(x$E.coli[x$E.coli>max_crit])/length(x$E.coli)*100, digits=0)
    plot(x$E.coli~x$Date,xlab="",ylab="MPN/100 mL", xaxt="n", pch=19, main=paste("E.coli","in",x$ML_Name[1],":",perc.exc,"% exceed Max Crit Std."))
    lines(x$E.coli~x$Date,xlab="Date",ylab="MPN/100 mL", lwd=1.5, lty=2)
    axis.Date(1, at=seq(min.dat, max.dat, by="6 months"), format="%m-%Y", las=2, cex=0.8)
    abline(h=geom_crit,col="orange", lwd=2)
    abline(h=max_crit, col="red", lwd=2)
    l=legend("topleft",c("Max","Geomean"),col=c("red","orange"), lty=1, lwd=2, bty="n", cex=0.6) 
    text(x=l$text$x[1], y=l$text$y[1]-l$rect$h[1]/2,paste("Max E.coli:",max.e,"MPN/100 mL"), adj=c(0,1), cex=0.8)
    
  }
  
  # Create multi-panel figure where each panel shows e coli over time for each MLID
  by(ecoli.day.gmean,ecoli.day.gmean[,"MLID"],dat_sum)
  
  ###### LOAD DURATION CALCULATIONS, MONTHLY LOADS/REC SEASON AND PERC REDUCTION #######
  
  flow_perc <- function(x){(1-percent_rank(x))*100}
  
  ## Create loading dataset
  flow.dat.2 <- flow.dat[,!names(flow.dat)%in%c("ML_NAME")]
  ecoli.flow.dat <- merge(ecoli.day.gmean,flow.dat.2, all.x=TRUE)
  ecoli.flow.dat <- ecoli.flow.dat[!is.na(ecoli.flow.dat$Flow.cfs),]
  ecoli.flow.dat$loading.capacity <- ecoli.flow.dat$Flow.cfs*geom_crit*cf
  ecoli.flow.dat$loading.capacity.mos <- ecoli.flow.dat$loading.capacity*0.9
  ecoli.flow.dat$observed.loading <- ecoli.flow.dat$Flow.cfs*ecoli.flow.dat$E.coli*cf
  ecoli.flow.dat$exceedance <- ifelse(ecoli.flow.dat$observed.loading>ecoli.flow.dat$loading.capacity.mos,"yes","no")
  
  flow.percentile <- unlist(tapply(ecoli.flow.dat$Flow.cfs,ecoli.flow.dat$MLID, flow_perc))
  
  ecoli.flow.dat$flow.percentile <- flow.percentile
  
  # Write load duration curve data to new datasheet 
  if(!any(wb.dat$sheet_names=="LDC_Data")){
    addWorksheet(wb.dat, "LDC_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "LDC_Data", ecoli.flow.dat, rowNames = FALSE)}
  
  # Plotting function
  ldc_func <- function(x){
    # Load duration curves
    flow.plot <- x[order(x$flow.percentile),]
    quartz()
    plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (MPN/day)", xlim=c(0, 100), ylim=c(min(flow.plot$observed.loading),max(flow.plot$observed.loading)), main=paste("Load Duration Curve:",x$ML_Name[1]))
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
    legend("topright",legend=c("Loading Capacity","Loading Capacity + 10% MOS", "Observed E.coli Loading"), bty="n", col=c("blue","green","black"), lty=c(1,1,NA),lwd=c(2,2,NA),pch=c(NA,NA,21), pt.bg=c(NA,NA,"purple"), cex=1)
  }
  
  by(ecoli.flow.dat,ecoli.flow.dat[,"MLID"],ldc_func)
  
  ## Loading by month ##
  ecoli.flow.dat$month <- month(ecoli.flow.dat$Date, label=TRUE)
  ol_mo <- aggregate(observed.loading~month+MLID+ML_Name, dat=ecoli.flow.dat, FUN=gmean)
  lc_mo <- aggregate(loading.capacity~month+MLID+ML_Name, dat=ecoli.flow.dat, FUN=gmean)
  mo_load <- merge(ol_mo,lc_mo, all=TRUE)
  mo_load <- mo_load[order(mo_load$month),]
  mo_load$perc.red <- ifelse(mo_load$observed.loading>mo_load$loading.capacity,round(perc.red(mo_load$loading.capacity,mo_load$observed.loading), digits=0),0)
  rownames(mo_load)<- mo_load$month
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Monthly_Data")){
    addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Monthly_Data", mo_load, rowNames = FALSE)}
 
  # Plotting function
  month_loading <- function(x){
    mo_load.p <- x[,!names(x)%in%c("month","perc.red","MLID","ML_Name")]
    quartz()
    barp <- barplot(t(mo_load.p), beside=T, main = paste("Monthly E.coli Loading Geomean:",x$ML_Name[1]), ylim=c(0, max(mo_load.p$observed.loading)+0.1*max(mo_load.p$observed.loading)), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
    legend("topleft",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
    box(bty="l")
    barps <- barp[1,]
    barperc <- data.frame(cbind(barps,x$observed.loading, x$perc.red))
    barperc <- barperc[barperc$V3>0,]
    barperc$V3 <- paste(barperc$V3,"%",sep="")
    text(barperc$barps,barperc$V2+0.1*barperc$V2[1],labels=barperc$V3,cex=0.8)
  }
  
  by(mo_load,mo_load[,"MLID"],month_loading)
  
  ## Filter to Rec Season ##
  ecoli.flow.rec <- ecoli.flow.dat[month(ecoli.flow.dat$Date)>4&month(ecoli.flow.dat$Date)<10,]
  ecoli.flow.rec$year <- year(ecoli.flow.rec$Date)
  
  # Aggregate by geometric mean
  ol_rec <- aggregate(observed.loading~year+MLID+ML_Name, dat=ecoli.flow.rec, FUN=gmean)
  lcmos_rec <- aggregate(loading.capacity.mos~year+MLID+ML_Name, dat=ecoli.flow.rec, FUN=gmean)
  rec <- merge(ol_rec,lcmos_rec, all=TRUE)
  rownames(rec) <- rec$year
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
    addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Rec_Season_Data", rec, rowNames = FALSE)}
  
  rec_season <- function(x){
    quartz()
    rec.p <- x[,!names(x)%in%c("year","MLID","ML_Name")]
    barplot(t(rec.p), beside=T, main = paste("Rec Season E.coli Loading Geomean by Year:",x$ML_Name[1]), ylim=c(0, max(rec.p$observed.loading)), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
    legend("topleft",legend=c("Observed Loading","Loading Capacity"), bty="n", fill=c("firebrick3","dodgerblue3"), cex=0.8)
    box(bty="l")
    
  }
  
  by(rec,rec[,"MLID"],rec_season)
  
  saveWorkbook(wb.dat, "Spring_Creek.xlsx", overwrite = TRUE)
  
}

