#' Convert data to target units
#'
#' This converts flat data to target units as defined in the dataset. It creates three new columns in your dataset: 1. The converted unit value, 3. the conversion factor used, & 4. a recommended acceptance/rejection based on pairs of input & target units.
#' All original columns are retained.
#' @param x Input dataset.
#' @param input_unit Column name of x with units to convert from (in quotes).
#' @param target_unit Column name of x with units to convert to (in quotes).
#' @param value_var Column name of x containing result values (in quotes).
#' @param conv_val_col Name of new values column for converted values (in quotes).

#' @examples 
#' # Make example data
#' value=c(abs(rnorm(5,1, 0.1)), rnorm(5, 1000, 100), rnorm(5, 20, 1))
#' raw_units=c(rep('mg/l',5), rep('ug/L', 5), rep('deg C',5))
#' preferred_units=c(rep('mg/L', 10), rep('deg C',5))
#' data=data.frame(value,raw_units,preferred_units)
#' data
#' 
#' # Unit conversion
#' conv_data=convertUnits(data, input_units='raw_units', target_units='preferred_units', value_var='value', conv_val_col='converted_value')
#' conv_data

#' @export
convertUnits=function(x, input_units, target_units='target_unit', value_var, conv_val_col='converted_value'){
	
	if(! input_units %in% names(x)){stop('input_units name not in x')}
	if(! target_units %in% names(x)){stop('target_units name not in x')}
	
	unit_conv_table=wqTools::unit_conv_table

	names(unit_conv_table)[names(unit_conv_table)=='target_unit'] = paste(target_units)
	names(unit_conv_table)[names(unit_conv_table)=='input_unit'] = paste(input_units)
	x=merge(x, unit_conv_table, all.x=T)
	x[,input_units]=as.character(x[,input_units])
	x[,target_units]=as.character(x[,target_units])
	
	conv_x=x
	conv_x$conversion_flag[is.na(conv_x$conversion_factor) & conv_x[,input_units]==conv_x[,target_units]] = "ACCEPT"
	conv_x$conversion_factor[is.na(conv_x$conversion_factor) & conv_x[,input_units]==conv_x[,target_units]] = 1
	conv_x$conv_val_col = conv_x[,value_var] * conv_x[,'conversion_factor']
	
	
	
	names(conv_x)[names(conv_x)=='conv_val_col'] = conv_val_col
	
	return(conv_x)
}

