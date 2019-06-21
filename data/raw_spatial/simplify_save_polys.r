
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(sf)
library(rmapshaper)
library(wqTools)
library(leaflet)

setwd("C:\\Users\\jvander\\Documents\\R\\wqTools\\data\\raw_spatial")

# Read in shapefiles
au_poly=sf::st_read(getwd(),"AU_poly_wgs84")
au_poly$polyID=seq(1:dim(au_poly)[1])
bu_poly=sf::st_read(getwd(),"Beneficial_Uses_All_2020IR_wgs84")
gsl_poly=sf::st_read(getwd(),"GSL_poly_wgs84")
ss_poly=sf::st_read(getwd(),"SiteSpecific_wgs84")
wmu_poly=sf::st_read(getwd(),"wmu_poly_wgs84")

# Calculate mileages & acreages by AU




# Simplify AU & BU polygons

au_poly_simp=ms_simplify(au_poly, keep=0.5)
plot(au_poly_simp[1])
all(au_poly$ASSESS_ID %in% au_poly_simp$ASSESS_ID)
all(au_poly$polyID %in% au_poly_simp$polyID)


bu_poly_simp=ms_simplify(bu_poly, keep=0.5)
plot(bu_poly_simp[1])

buildMap() %>% clearGroup(group='Assessment units') %>% clearGroup(group='Beneficial uses') %>% 
	addPolygons(data=au_poly_simp,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, color='orange', options = pathOptions(pane = "au_poly")) %>%
	addPolygons(data=bu_poly_simp,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1, color='green', options = pathOptions(pane = "underlay_polygons"))
	
# Save polygon .rdata files

setwd("C:\\Users\\jvander\\Documents\\R\\wqTools\\data")

save(file="wqTools\\data\\au_poly.rdata",au_poly_simp)
save(file="wqTools\\data\\bu_poly.rdata",bu_poly_simp)


au_poly=sf::st_read(getwd(),"AU_poly_wgs84")
save(file="C:\\Users\\jvander\\Documents\\R\\wqTools\\data\\au_poly.rdata", au_poly)

bu_poly=sf::st_read(getwd(),"Beneficial_Uses_All_2020IR_wgs84")
save(file="F:\\R\\udwqTools\\data\\bu_poly.rdata", bu_poly)

ss_poly=sf::st_read(getwd(),"SiteSpecific_wgs84")
save(file="C:\\Users\\jvander\\Documents\\R\\wqTools\\data\\ss_poly.rdata", ss_poly)

ut_poly=sf::st_read(getwd(),"UT_state_bnd_noTribal_wgs84")
save(file="C:\\Users\\jvander\\Documents\\R\\wqTools\\data\\ut_poly.rdata", ut_poly)



#load("ut_poly_orig.rdata")
#ut_poly_simp=ms_simplify(ut_poly, keep=0.2)
#plot(ut_poly_simp[1])
#ut_poly=ut_poly_simp


#devtools::install_local("C:\\Users\\jvander\\Documents\\R\\wqTools")

# Save .rdata files to package

save(file="wqTools\\data\\ut_poly.rdata",ut_poly)




