
function(cfs, codebook){
   lookup <- function(dictionary, type, value){
      dictionary[[type]][[value]]
   }
   varstodates <- dget('functions/varsToDates.R')
   if(nrow(cfs) > 0){

      cfs$start_date <- suppressWarnings(varstodates(list(year = cfs$cf_dec_yr,
                                         month = cfs$cf_dec_month,
                                         day = cfs$cf_dec_day), fixNaMonth = TRUE)) 
      cfs$end_date <- suppressWarnings(varstodates(list(year = cfs$end_yr,
                                         month = cfs$end_month,
                                         day = cfs$end_day), fixNaMonth = TRUE))

      cfs$year <- year(cfs$start_date)

      cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s',' ')
      cfs$actor_name <- str_replace_all(cfs$actor_name,' +',' ')

      cfs$type <- sapply(as.character(cfs$ceasefire_type), function(t){
         lookup(codebook,"type",t)
         })
      cfs$purpose <- sapply(as.character(cfs$purpose_1), function(p){
         lookup(codebook,"purpose",p)
         }) 

      cfs %>%
         select(
            location,
            start = start_date,
            id = cf_id,
            conflict_id = ucdp_acd_id,
            actor_name,
            ucdp_dyad,
            year,
            type,
            purpose,
            name = actor_name)
   } else {
      tibble(location = character(),
             start = integer(),
             id = integer(),
             actor_name = character(),
             ucdp_dyad = character(),
             year = integer(),
             type = character(),
             purpose = character(),
             name = character())

   }
   
}


