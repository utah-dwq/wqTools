#' Assign water years to dates
#'
#' This function assigns water years to standard dates.
#' @param dates Input date vector. Must of class "Date".
#' @param start_month Month to start water year. Default is 10.
#' @examples 
#' dates=as.Date(c("2022-09-29","2022-09-30","2022-10-01","2022-10-02","2022-10-03"))
#' waterYear(dates)
#' @return Returns vector of water years.
#' @export
waterYear <- function(dates, start_month = 10) {
	if(class(dates) != "Date"){
		stop("Input must be date class.")
	}
	offset = ifelse(as.integer(format(dates, "%m")) < start_month, 0, 1)
	water_year = as.integer(format(dates, "%Y")) + offset
	return(water_year)
}


