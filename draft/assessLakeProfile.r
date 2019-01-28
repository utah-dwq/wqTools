#' Assess an individual lake profile
#' 
#' Assesses an individual lake profile with uses assigned provided in long data format
#'
#' @param data Lake profile data (long format)

#' @export



library(wqTools)

sites=c("UTAHDWQ_WQX-4938550","UTAHDWQ_WQX-4938570","UTAHDWQ_WQX-5913220","UTAHDWQ_WQX-4917370","UTAHDWQ_WQX-4917310")
nr=readWQP(type="narrowresult", siteid=sites)
activity=readWQP(type="activity", siteid=sites)
data=merge(nr, activity, all.x=T)
profiles=data[!is.na(data$DataLoggerLine),]


assessLakeProfile=function(x, depth="Depth, data-logger (ported)", do="", pH="", temp="", bu="BeneficialUse"){
	x=x[order(x$Profile.depth),]
	
	samp_count=dim(x)[1]
	pct10=ceiling(dim(x)[1] *0.1)
	do_exc_cnt=sum(x$do_exc)
	temp_exc_cnt=sum(x$temp_exc)
	pH_exc_cnt=sum(x$pH_exc)
	pH_asmnt=ifelse(pH_exc_cnt<=pct10,"FS","NS")

	rles=rle(x$do_temp_exc)
	strat=data.frame(rles$lengths, rles$values)
	strat=within(strat,{
		bottom_index=cumsum(rles$lengths)
		bottom_depth=x$Profile.depth[bottom_index]
		top_index=bottom_index-rles.lengths+1
		top_depth=x$Profile.depth[top_index]
		layer_width=bottom_depth-top_depth
	})
	
	strat		
	max_hab_width=max(strat$layer_width[strat$rles.values==0])

	if(x$stratified[1]==1 & max(x$Profile.depth)>3){ #stratified
		do_temp_asmnt=ifelse(max_hab_width>=3, "FS", "NS")
		do_asmnt=as.factor(NA)
		temp_asmnt=as.factor(NA)
	
	}else{ #non-stratified
		do_temp_asmnt=as.factor(NA)
		temp_asmnt=ifelse(temp_exc_cnt>pct10 & temp_exc_cnt>=2,"NS","FS")
		do_asmnt=ifelse(do_exc_cnt>pct10 & do_exc_cnt>=2,"NS","FS")
	}

	asmnt=data.frame(max_hab_width,do_temp_asmnt,do_exc_cnt,do_asmnt,temp_exc_cnt,temp_asmnt,pH_exc_cnt,pH_asmnt,samp_count,pct10)
	
	return(asmnt)
}

