#' Convert data to target units
#'
#' This converts flat data to target units as defined in the dataset. It creates three new columns in your dataset: 1. The converted unit value, 3. the conversion factor used, & 4. a recommended acceptance/rejection based on pairs of input & target units.
#' All original columns are retained.
#' @param x Input dataset.
#' @param input_unit Column name of x with units to convert from.
#' @param target_unit Column name of x with units to convert to.
#' @param value_var Column name of x containing result values.
#' @param conv_val_col Name of new values column for converted values.

#' @examples 
#' # Make example data
#' value=c(abs(rnorm(5,1, 0.1)), rnorm(5, 1000, 100))
#' units=c(rep('mg/l',5), rep('ug/L', 5))
#' target_unit=rep('mg/L', 10)
#' data=data.frame(value,units,target_unit)
#' data
#' 
#' # Unit conversion
#' conv_data=convertUnits(data, input_unit='units', target_unit='target_unit', value_var='value', conv_val_col='converted_value')
#' conv_data

#' @export
convertUnits=function(x, input_unit, target_unit='target_unit', value_var, conv_val_col='converted_value'){
	#x=data
	#input_unit='units'
	#target_unit='target_unit'
	#conv_val_col='converted_value'
	#value_var='value'
	
	data(unit_conv_table, package='wqTools')
	names(unit_conv_table)[names(unit_conv_table)=='target_unit'] = paste(target_unit)
	names(unit_conv_table)[names(unit_conv_table)=='input_unit'] = paste(input_unit)
	x=merge(x, unit_conv_table, all.x=T)
	
	conv_x=within(x, {
		units=as.character(units)
		target_unit=as.character(target_unit)
		conversion_flag[is.na(conversion_factor) & units==target_unit] = "ACCEPT"
		conversion_factor[is.na(conversion_factor) & units==target_unit] = 1
		conv_val_col = get(value_var) * conversion_factor
	})

	names(conv_x)[names(conv_x)=='conv_val_col'] = conv_val_col
	
	return(conv_x)
}

