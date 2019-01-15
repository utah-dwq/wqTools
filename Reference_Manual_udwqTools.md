

<!-- toc -->

January 14, 2019

# DESCRIPTION

```
Package: udwqTools
Title: What the Package Does (one line, title case)
Version: 0.0.0.9000
Authors@R: person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))
Description: What the package does (one paragraph).
Depends: R (>= 3.5.1)
License: What license is it under?
Encoding: UTF-8
LazyData: true
RoxygenNote: 6.1.1```


# `calcTSI`: Calculate TSI values from input data

## Description


 This function calculates TSI values according to Utah's IR methods from input data containing values for of chlorophyll, total phosphorus, and secchi disk depth.
 Note that inputs for these parameters must be specified in units of ug/L, mg/L, and meters, respectively.


## Usage

```r
calcTSI(x, in_format = "wide", chl_ugL = "chla", TP_mgL = "TP",
  SD_m = "SD")
```


## Arguments

Argument      |Description
------------- |----------------
```x```     |     Input dataset
```in_format```     |     One of "wide" or "flat" to specify data input format. Note that only wide format inputs are currently supported.
```chl_ugL```     |     Name of chlorophyll-a variable in ug/L
```TP_mgL```     |     Name of total phosphorus variable in mg/L
```SD_m```     |     Name of secchi disk depth variable in m

## Examples

```r 
 data(ul_trophic)
 head(ul_trophic)
 tsi=calcTSI(ul_trophic,chl_ugL="ChlA",TP_mgL="Phosphate.phosphorus.Total",SD_m="Depth.Secchi.disk.depth")
 head(tsi)
 plot(TSIchl~ChlA,tsi)
 ``` 

# `facToNum`: Convert factors to numeric equivalents
 Converts input object to number if class=="factor". If class !="factor", input object is returned un-altered.

## Description


 Convert factors to numeric equivalents
 Converts input object to number if class=="factor". If class !="factor", input object is returned un-altered.


## Usage

```r
facToNum(x)
```


## Arguments

Argument      |Description
------------- |----------------
```x```     |     Input vector object

# `readECHO_ec`: Read effluent chart data from EPA ECHO webservices

## Description


 This function extracts effluent chart data from EPA ECHO for multiple stations & combinations of parameters. All arguments are optional except p_id. At least one p_id must be specified.


## Usage

```r
readECHO_ec(...)
```


## Arguments

Argument      |Description
------------- |----------------
```...```     |     additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/effluent-charts#!/Effluent_Charts/get_eff_rest_services_download_effluent_chart optional arguments for effluent chart data reads. Note that arguments for output are ignored.
```p_id```     |     Permitted facility ID. Either a single text value (in quotes) or a vector of text values.
```parameter_code```     |     Parameter code. Either a single text value (in quotes) or a vector of text values.
```start_date```     |     Query start date in "mm/dd/yyyy" format.
```end_date```     |     Query end date in "mm/dd/yyyy" format.

## Value


 A flat data frame of EPA ECHO effluent chart data


## Examples

```r 
 #Extract effluent chart data for facility UT0025241, all outfalls
 UT0025241_ec=readECHO_ec(type="ec",p_id="UT0025241")
 head(UT0025241_ec)
 
 #Extract effluent total phosphorus data from outfall 001 for facility UT0025241 (note that monitoring_location_desc is not an available argument for download_effluent_chart ECHO web services)
 UT0025241_tp_001=readECHO_ec(p_id="UT0025241", parameter_code="00665", outfall="001")
 UT0025241_tp_001_effluent=UT0025241_tp_001[UT0025241_tp_001$monitoring_location_desc=="Effluent Gross",]
 head(UT0025241_tp_001_effluent)
 
 #Extract flow through facility from UT0021717
 UT0021717_flow=readECHO_ec(p_id="UT0021717", parameter_code="50050")
 ggplot2::qplot(as.Date(monitoring_period_end_date, '%m/%d/%Y'),dmr_value_standard_units,data=UT0021717_flow[UT0021717_flow$monitoring_location_desc=="Effluent Gross",],color=statistical_base_short_desc, ylab="Flow (mgd)", xlab="Date") +ggplot2::theme_bw()
 
 #Extract flow & TP from UT0025241 & UT0021717
 tp_flow=readECHO_ec(p_id=c("UT0025241","UT0021717"), parameter_code=c("50050","00665"))
 ``` 

# `readECHO_fac`: Read facility information from EPA ECHO webservices

## Description


 This function extracts facility information from EPA ECHO based on argument inputs.


## Usage

```r
readECHO_fac(type = "", ...)
```


## Arguments

Argument      |Description
------------- |----------------
```...```     |     additional arguments to be passed to ECHO query path. See https://echo.epa.gov/tools/web-services/facility-search-water#!/Facility_Information/get_cwa_rest_services_get_facility_info for optional arguments for facilities. Note that arguments for output are ignored.

## Value


 A data frame of EPA ECHO facility information


## Examples

```r 
 #Extract facility locations in Utah
 ut_fac=readECHO_fac(p_st="ut", p_act="y")
 head(ut_fac)
 ``` 

# `readWQP`: Read EPA Water Quality Portal Data

## Description


 This function extracts water quality data from EPA's Water Quality Portal based on user arguemnt inputs. Note that connections to the WQP occassionally time out during download. This function tries to download requested files up to 10 times before exiting.
 All arguments except type are optional, but at least one should be provided to limit download size and prevent errors connecting to WQP.
 Note that some, but not all, special characters in characteristic names have been accounted if. If in doubt, use the WQP web interface to determine the appropriate sytax for odd characteristic names.


## Usage

```r
readWQP(type = "result", ..., print = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
```type```     |     Data type to read. One of "result", "narrowresult", "sites", "activity", or "detquantlim".
```...```     |     additional arguments to be passed to WQP query path. See https://www.waterqualitydata.us/portal/ for optional arguments.
```print```     |     Logical. Print summary table of sites & characteristics (only for result or narrowresult types).
```startDateHi```     |     Query start date in 'mm-dd-yyyy' format.
```startDateLo```     |     Query end date in 'mm-dd-yyyy' format.

## Value


 A data frame of WQP data


## Examples

```r 
 # Read some data from Mantua Reservoir (2016-2018)
 nr=readWQP(type="narrowresult", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"), startDateLo="01-01-2016", startDateHi="12-31-2018")
 
 # Read just Arsenic, Cadmium, and DO, all dates
 nr=readWQP(type="narrowresult", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"), characteristicName=c("Arsenic","Cadmium","Dissolved oxygen (DO)"))
 
 # Read all Total dissolved solids statewide (2016-2018) (& note statecode for Utah)
 tds_sw=readWQP(type="result", statecode="US:49", characteristicName="Total dissolved solids", startDateLo="01-01-2016", startDateHi="12-31-2018", print=F)
 
 # Read sites in Utah
 sites=readWQP(type="sites", statecode="US:49")
 plot(LatitudeMeasure~LongitudeMeasure, sites[sites$LatitudeMeasure>0 & sites$LongitudeMeasure<0,])
 ``` 

