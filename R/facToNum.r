#' Convert factors to numeric equivalents
#' 
#' Converts input object to number if class=="factor". If class !="factor", input object is returned un-altered.
#' @param x Input vector object

#' @export
facToNum=function(x){
	if(class(x)=="factor"){result=as.numeric(levels(x))[x]
	}else{
		if(class(x)=='character'){result=as.numeric(x)
		}else{result=x}
	}
	return(result)
	}

	