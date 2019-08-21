
function(cfs, codebook){
   lookup <- function(x,dict){
      sapply(x, function(value) {dict[[value]]}, USE.NAMES = FALSE)
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

      catvars <- c('ceasefire_type','purpose_1','ddr','nsa_frac','geography',
                   'timing','mediator_nego','implement','enforcement')
      for(v in catvars){
         cfs[[v]] <- lookup(as.character(cfs[[v]]), codebook[[v]])
      }

      cfs %>%
         select(
            location,
            start = start_date,
            id = cf_id,
            conflict_id = ucdp_acd_id,
            actor_name,
            ucdp_dyad,
            year,
            cat_type = ceasefire_type,
            cat_purpose = purpose_1,
            cat_ddr = ddr,
            cat_fractionalization = nsa_frac,
            cat_geography = geography,
            cat_timing = timing,
            cat_mediated = mediator_nego,
            cat_mechanism = implement,
            cat_enforcement = enforcement, 
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


