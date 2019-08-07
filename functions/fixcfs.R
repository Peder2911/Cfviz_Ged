
function(cfs){
   varstodates <- dget('functions/varsToDates.R')

   cfs$start_date <- suppressWarnings(varstodates(list(year = cfs$cf_dec_yr,
                                      month = cfs$cf_dec_month,
                                      day = cfs$cf_dec_day), fixNaMonth = TRUE)) 
   cfs$end_date <- suppressWarnings(varstodates(list(year = cfs$end_yr,
                                      month = cfs$end_month,
                                      day = cfs$end_day), fixNaMonth = TRUE))

   cfs$year <- year(cfs$start_date)

   cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s',' ')
   cfs$actor_name <- str_replace_all(cfs$actor_name,' +',' ')

   cfs %>%
      select(
         location,
         start = start_date,
         id = cf_id,
         actor_name,
         ucdp_dyad,
         year,
         name = actor_name)
}


