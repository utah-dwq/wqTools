#' Plot an individual lake profile
#' 
#' Plots an individual lake profile provided in long data format
#'
#' @param data Lake profile data (long format)

#' @export
profilePlot=function(data, parameter="CharacteristicName", units="ResultMeasure.MeasureUnitCode", depth="Depth, data-logger (ported)", do="Dissolved oxygen (DO)", temp="Temperature, water", pH="pH", value_var="ResultMeasureValue", line_no="DataLoggerLine",
	do_crit=5, temp_crit=20, pH_crit=c(6.5,9)){

####
#testing
library(wqTools)

site_list=c("UTAHDWQ_WQX-4938550","UTAHDWQ_WQX-4938570","UTAHDWQ_WQX-5913220","UTAHDWQ_WQX-4917370","UTAHDWQ_WQX-4917310")
nr=readWQP(type="narrowresult", siteid=site_list, print=F)
activity=readWQP(type="activity", siteid=site_list)
sites=readWQP(type="sites", siteid=site_list)
#sites=assignUses(sites, flatten=T)
#sites=assignAUs(sites)
data_merged=merge(nr, activity, all.x=T)
data_merged=merge(data_merged, sites, all.x=T)
profiles=data_merged[!is.na(data_merged$DataLoggerLine),]
table(droplevels(profiles$ActivityIdentifier))

data=profiles[profiles$ActivityIdentifier=="UTAHDWQ_WQX-BORFG051909-4938550-0519-Pr-F",]

parameter="CharacteristicName"
depth="Depth, data-logger (ported)"
do="Dissolved oxygen (DO)"
temp="Temperature, water"
pH="pH"
units="ResultMeasure.MeasureUnitCode"
do_crit=5
temp_crit=20
pH_crit=c(6.5,9)

####

# Subset to parameter inputs (in case something else sneaks in w/ !is.na(DataLoggerLine)
data=droplevels(data[data[,parameter] %in% c(depth,do,temp),])

# Check units for each parameter
if(any(
	length(unique(data[,units][data[,parameter]==do]))>1,
	length(unique(data[,units][data[,parameter]==depth]))>1,
	length(unique(data[,units][data[,parameter]==temp]))>1,
	length(unique(data[,units][data[,parameter]==pH]))>1
	)){warning("Mixed units for one or more parameter present. Check if these are OK to mix or convert units as appropriate.")
		print(table(data[,parameter],data[,units]))
		}


# Assign criteria (if not already assigned)
do_crit=data.frame(unlist(do_crit))
do_crit$BeneficialUse=row.names(do_crit)
do_crit[,paste(parameter)]=do
names(do_crit)[names(do_crit)=="unlist.do_crit."]="input_crit"

temp_crit=data.frame(unlist(temp_crit))
temp_crit$BeneficialUse=row.names(temp_crit)
temp_crit[,paste(parameter)]=temp
names(temp_crit)[names(temp_crit)=="unlist.temp_crit."]="input_crit"

pH_crit=data.frame(unlist(pH_crit))
pH_crit$BeneficialUse=NA
pH_crit[,paste(parameter)]=pH
names(pH_crit)[names(pH_crit)=="unlist.pH_crit."]="input_crit"

input_crit=rbind(do_crit, temp_crit, pH_crit)
data_sub=data
data_sub=merge(data_sub,input_crit, all.x=T)

# Make numeric criterion numeric
if(class(data$NumericCriterion)=="character"){data$NumericCriterion=as.numeric(data$NumericCriterion)}
if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=facToNum(data$NumericCriterion)}

# Determine exceedances
data_exc=data_sub
data_exc$exc=0

data_exc=within(data_exc, {
	exc[CriterionType=="max" & IR_Value > NumericCriterion]=1
	exc[CriterionType=="min" & IR_Value < NumericCriterion]=1
	})





# Cast to matrix




plot(-1*paste(depth)~paste(temp), prof_matrix)






}




