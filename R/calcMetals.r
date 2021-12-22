#' Calculate hardness adjusted metals criteria for aquatic life uses & conversion factors
#'
#' Calculates metals criteria and conversion factors for aquatic life uses given a hardness value. Hardness must be supplied in mg/L.
#' 
#' @param hardness Numeric hardness value in mg/L
#' @param cap400 Logical. If TRUE (default), assume hardness of 400 mg/L for waters with observed hardnes >= 400 mg/L per UAC R3172-
#' @return
#' @examples 

#' @export
calcMetals=function(x, cap400=T){

hardness=c(100)
cap400=T

if(cap400){hardness[hardness>400]=400}

formulas=read.csv(file="C:\\Users\\jvander\\Documents\\R\\wqTools\\data\\metals_formulas.csv")

hardness_formulas=merge(hardness, formulas)
names(hardness_formulas)[names(hardness_formulas)=="x"]="hardness"

calcs_flat=within(hardness_formulas, {
	formula=gsub("ln", "log", formula, fixed=TRUE)
	formula=gsub(") (", ")*(", formula, fixed=TRUE)
	formula=gsub(")(",  ")*(", formula, fixed=TRUE)
	formula=gsub("(e(",  "(exp(", formula, fixed=TRUE)
	formula=gsub("e(",  "exp(", formula, fixed=TRUE)
	formula=stringr::str_replace_all(formula, "hardness", as.character(hardness))
	calculated=sapply(formula, function(x) eval(parse(text=x)))
	suppressWarnings({
		calculated=wqTools::facToNum(calculated)
	})
})


#calcs_wide=

return(list(calcs_flat=calcs_flat,calcs_wide=calcs_wide)

}

