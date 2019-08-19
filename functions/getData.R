#' getData
#'
#' Retrieves variables from {table} @ {con} according to two predicates: time
#' and country. 
#'

function(con, table, schema, country, startyear, endyear, variables = '*'){
   schema_dictionary <- list('ged' = list(country = 'country',
                                          startyear = 'year'),
                             'cfs' = list(country = 'location',
                                          startyear = 'cf_dec_yr'))

   query <- glue('SELECT {variables} FROM {table}')

   country_predicate <- ' WHERE {cntryvar} =\'{country}\''
   query <- paste0(query,country_predicate)

   if(startyear > 1989 | endyear < 2019){
      year_predicate <- ' AND ({yrvar} >= {startyear} AND {yrvar} <= {endyear})'
      query <- paste0(query,year_predicate)
   }

   cntryvar <- schema_dictionary[[schema]]$country
   yrvar <- schema_dictionary[[schema]]$startyear

   dbGetQuery(con,glue(query))
}
