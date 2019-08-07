function(varlist,fixNaDay = TRUE,fixNaMonth = TRUE){

	if(!all(c('year','month','day') %in% names(varlist))){
		stop('usage: varsToDates(list(year=year,month=month,day=day))')
		}


	fixNa <- function(x,replacement){
		if(is.na(x)) warning('Replacing ',names(x))
		x <- ifelse(is.na(x),replacement,x) 
		}

	vtd <- function(varlist){

		vars <- lapply(varlist,as.numeric)%>%
		as.data.frame(stringsAsFactors = FALSE)

		apply(vars,1,function(r){

		r <- lapply(r,function(v){ifelse(v > 0,v,NA)})

		if(fixNaDay){
			r['day'] <- fixNa(r['day'],15)
		}
		if(fixNaMonth){
			r['month'] <- fixNa(r['month'],6)
		}

		if(all(!is.na(r))){
		paste(r['year'],r['month'],r['day'],sep = '-')
		}

		else {
		NA
		}

		})
	}
	as.Date(vtd(varlist))
}
