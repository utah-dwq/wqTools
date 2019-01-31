#' Plot an individual lake profile
#' 
#' Plots an individual lake profile provided in long data format. If provided, dashed lines representing water quality criteria are also plotted.
#' Default arguments are set to take data from the water quality portal, however, they may be updated as needed to reflect different data sources.
#'
#' @param data Lake profile data (long format)
#' @param parameter Column name containing parameter names.
#' @param units Column name containing data units.
#' @param depth Name of depth variable in input data.
#' @param do Name of dissolved oxygen variable in input data.
#' @param temp Name of temperature variable in input data.
#' @param pH Name of pH variable in input data.
#' @param value_var Column name of value variable.
#' @param line_no Column name containing line number.
#' @param do_crit Optional. Dissolved oxygen criterion to display on plot.
#' @param temp_crit Optional. Temperature criterion to display on plot.
#' @param pH_crit Optional. Vector of two pH criteria to display on plot.
#' @importFrom reshape2 dcast
#' @importFrom rLakeAnalyzer thermo.depth
#' @examples
#' # Read in some profile data
#' nr=readWQP(type="narrowresult", siteid="UTAHDWQ_WQX-4938550", print=F)
#' act=readWQP(type="activity", siteid="UTAHDWQ_WQX-4938550", print=F)
#' nr_act=merge(nr, act, all.x=T)
#' profiles=nr_act[!is.na(nr_act$DataLoggerLine),] #Subset to profile data
#' table(droplevels(profiles$ActivityIdentifier)) #Find activity IDs associated w/ profiles
#' profilePlot(subset(profiles,ActivityIdentifier=="UTAHDWQ_WQX-BORFG051909-4938550-0519-Pr-F"))
#' profilePlot(subset(profiles,ActivityIdentifier=="UTAHDWQ_WQX-LC082807-210255-PR3855083007"), do_crit=4, temp_crit=20, pH_crit=c(6.5,9))
#' @export
profilePlot=function(data, parameter="CharacteristicName", units="ResultMeasure.MeasureUnitCode", depth="Depth, data-logger (ported)", do="Dissolved oxygen (DO)",
	temp="Temperature, water", pH="pH", value_var="ResultMeasureValue", line_no="DataLoggerLine", do_crit, temp_crit, pH_crit){

#####
##testing
#library(wqTools)
#
#site_list=c("UTAHDWQ_WQX-4938550","UTAHDWQ_WQX-4938570","UTAHDWQ_WQX-5913220","UTAHDWQ_WQX-4917370","UTAHDWQ_WQX-4917310")
#nr=readWQP(type="narrowresult", siteid=site_list, print=F)
#activity=readWQP(type="activity", siteid=site_list)
#sites=readWQP(type="sites", siteid=site_list)
##sites=assignUses(sites, flatten=T)
##sites=assignAUs(sites)
#data_merged=merge(nr, activity, all.x=T)
#data_merged=merge(data_merged, sites, all.x=T)
#profiles=data_merged[!is.na(data_merged$DataLoggerLine),]
#head(table(droplevels(profiles$ActivityIdentifier)))
#
#data=profiles[profiles$ActivityIdentifier=="UTAHDWQ_WQX-BORFG080410-4938550-0804-Pr-F",]
#data_bk=data
#
#parameter="CharacteristicName"
#depth="Depth, data-logger (ported)"
#do="Dissolved oxygen (DO)"
#temp="Temperature, water"
#pH="pH"
#units="ResultMeasure.MeasureUnitCode"
#do_crit=5
#temp_crit=20
#pH_crit=c(6.5,9)
#value_var="ResultMeasureValue"
#line_no="DataLoggerLine"
#
#
#
#data=test
#parameter = "R3172ParameterName"
#units = "IR_Unit"
#depth = "Profile depth"
#do = "Minimum Dissolved Oxygen"
#temp = "Temperature, water"
#pH = "pH"
#value_var = "IR_Value"
#line_no = "DataLoggerLine"
#
######



# Subset to parameter inputs (in case something else sneaks in w/ !is.na(DataLoggerLine)
data=data[!is.na(data[,line_no]),]
data=droplevels(data[data[,parameter] %in% c(depth,do,temp, pH),])

if(all(data[,parameter] != depth)){stop("No depth values associated with this profile")}

# Simplify parameter names via function argument specifications
param_names=data.frame(rbind(depth,do,temp,pH))
colnames(param_names)[1]=paste(parameter)
param_names$param_name=row.names(param_names)

data=merge(data,param_names, all.x=T)

# Check units for each parameter
if(any(
	length(unique(data[,units][data[,parameter]==do]))>1,
	length(unique(data[,units][data[,parameter]==depth]))>1,
	length(unique(data[,units][data[,parameter]==temp]))>1,
	length(unique(data[,units][data[,parameter]==pH]))>1
	)){stop("Mixed units for one or more parameter present. Convert units as appropriate.")
		print(table(data[,parameter],data[,units]))
		}

unit_table=unique(data[,c("param_name",units)])

# Cast to matrix
prof_matrix=reshape2::dcast(MonitoringLocationIdentifier+ActivityIdentifier+ActivityStartDate+get(line_no)~param_name, value.var=paste(value_var), fun.aggregate=mean, data=data)

# Order matrix by depth
prof_matrix=prof_matrix[order(prof_matrix$depth),]

# Calc thermocline depth
tc_data=aggregate(temp~depth,data=prof_matrix,FUN='mean')
tc_depth=rLakeAnalyzer::thermo.depth(tc_data$temp, tc_data$depth)

# Plotting
par(mar=c(7.1,5.1,3.1,2.1))

plot(depth~temp, prof_matrix, ylim=rev(range(depth)), xlim=c(min(data[data$param_name!="depth",value_var], na.rm=T),max(data[data$param_name!="depth",value_var], na.rm=T)), pch=NA, xlab="",
	ylab=paste0("Depth (",unit_table[unit_table$param_name=="depth",2],")"), xaxt='n', cex.axis=1.25, cex.lab=1.5)
axis(3, cex.axis=1.25)
abline(h=tc_depth, col="purple", lwd=2, lty=2)
abline(h=0, lwd=1, lty=3)

if(!missing(do_crit)){abline(v=do_crit, col="deepskyblue3", lty=3, lwd=3)}
if(!missing(temp_crit)){abline(v=temp_crit, col="orange", lty=3, lwd=3)}
if(!missing(pH_crit)){abline(v=pH_crit[1], col="green", lty=3, lwd=3)}
if(!missing(pH_crit)){abline(v=pH_crit[2],col="green", lty=3, lwd=3)}

if(any(!is.na(prof_matrix$do))){
	points(depth~do, prof_matrix, type='b', bg="deepskyblue3", pch=24, cex=1.5)
}
if(any(!is.na(prof_matrix$temp))){
	points(depth~temp, prof_matrix, type='b', bg="orange", pch=21, cex=1.5)
}
if(any(!is.na(prof_matrix$pH))){
	points(depth~pH, prof_matrix, type='b', bg="green", pch=22, cex=1.5)
}

leg_x=min(data[data$param_name!="depth",value_var])
leg_y=max(prof_matrix$depth)*1.05

par(xpd=TRUE)
legend(x=leg_x, y=leg_y, bty='n',lty=c(NA,NA,NA,3),lwd=c(NA,NA,NA,3), col=c("black","black","black","purple"),
	legend=c(
		paste0("Dissolved oxygen (",unit_table[unit_table$param_name=="do",2],")"),
		paste0("Temperature (",unit_table[unit_table$param_name=="temp",2],")"),
		"pH", "Thermocline"),
	pch=c(24,21,22,NA), pt.bg=c("deepskyblue3","orange","green",NA), cex=1.5)
par(xpd=FALSE)

}
