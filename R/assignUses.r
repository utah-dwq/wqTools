#' Assign Utah beneficial use classes to sites
#'
#' This function assigns beneficial use classes to water quality portal type site objects (or data with site information attached).
#' @param data Input dataset. Must include latitude & longitude columns.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @param flatten Logical. If FALSE (default), maintain use categorys as single comma separated column. If TRUE, use column and data are flattened by expanded use column.
#' @importFrom sf st_as_sf
#' @importFrom sf st_set_crs
#' @importFrom sf st_join
#' @importFrom sf st_geometry
#' @importFrom reshape2 colsplit
#' @importFrom reshape2 melt
#' @examples 
#' # Read a couple of sites (Mantua Reservoir and a tributary)
#' sites=readWQP(type="sites", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"))
#' sites_uses=assignUses(sites)
#' sites_uses_flat=assignUses(sites, flatten=TRUE)

#' @export
assignUses=function(data, lat="LatitudeMeasure", long="LongitudeMeasure", flatten=FALSE){	
	bu_poly=wqTools::bu_poly
	x=data
	x=sf::st_as_sf(x, coords=c(long,lat), crs=4326, remove=F)
	x=sf::st_set_crs(x, sf::st_crs(bu_poly))	
	
	isect=suppressMessages({suppressWarnings({sf::st_join(x, bu_poly, left=TRUE)})})
	sf::st_geometry(isect)=NULL

	if(flatten){
		#Expand comma separated uses (bu_class)
		max_use_count=max(sapply(strsplit(as.character(isect$bu_class),","),FUN="length"))
		use_colnames=paste0(rep("use",max_use_count),seq(1:max_use_count))
		uses_mat=unique(data.frame(isect$bu_class,reshape2::colsplit(isect$bu_class,",",use_colnames)))
		names(uses_mat)[names(uses_mat)=="isect.bu_class"]="bu_class"
		
		#Flatten uses
		uses_flat=reshape2::melt(uses_mat, id.vars="bu_class", value.name = "BeneficialUse")
		uses_flat=uses_flat[,!names(uses_flat)=="variable"]
		uses_flat=uses_flat[uses_flat$BeneficialUse!="" & !is.na(uses_flat$BeneficialUse),]
		
		#Merge flat uses back to data by bu_class
		result=merge(isect,uses_flat,all=T)
	}else{
		result=isect
		names(result)[names(result)=="bu_class"]="BeneficialUse"
	}
			
return(result)

}
