shh <- suppressPackageStartupMessages
shh(library(RPostgreSQL))
shh(library(glue))
shh(library(ggplot2))
shh(library(magrittr))
shh(library(dplyr))
shh(library(stringr))
shh(library(lubridate))

options(warn = -1)

# Local functions ================
gedtimeline <- dget('functions/gedtimeline.R')
fixcfs <- dget('functions/fixcfs.R')
timelineplot <- dget('functions/timelineplot.R')
latestversion <- dget('functions/latestversion.R')

server <- function(input, output, session){
   # SQL setup ======================
   dr <- dbDriver('PostgreSQL')
   con <- dbConnect(dr, db = Sys.getenv('GED_DB'), 
                    user = Sys.getenv('GED_USER'), 
                    password = Sys.getenv('GED_PASS'),
                    host = Sys.getenv('GED_HOST'), 
                    port = Sys.getenv('GED_PORT'))

   GEDTABLE <- Sys.getenv('GED_TABLE')
   CFTABLE <- {
      alltables <- dbListTables(con)
      latestversion(alltables,'cf')  
   }

   # Choices setup ==================
   gedcountries <- dbGetQuery(con,glue('SELECT country FROM {GEDTABLE}')) %>%
      unique()    
   cfcountries <- dbGetQuery(con,glue('SELECT Location FROM {CFTABLE}')) %>%
      unique()
   allcountries <- intersect(gedcountries$country,cfcountries$location) %>%
      sort()

   updateSelectInput(session,'country',choices = allcountries)

   # Do plot when cntry changes =====
   observeEvent({input$country
                 input$enterpress
                 },{
      cntry <- input$country 
      gedquery <- glue('SELECT * FROM {GEDTABLE} WHERE country=\'{cntry}\'')
      cfquery <- glue('SELECT * FROM {CFTABLE} WHERE location=\'{cntry}\'')

      if(input$startyear > 1989 | input$endyear < 2019){
         template <- 'AND ({yrvar} >= {input$startyear} AND {yrvar} <= {input$endyear})'
         yrvar <- 'year'
         gedcond <- glue(template)
         yrvar <- 'cf_dec_yr'
         cfcond <- glue(template)

         gedquery <- glue('{gedquery} {gedcond}')
         cfquery <- glue('{cfquery} {cfcond}')
      }

      ged <- dbGetQuery(con,gedquery)
      cfs <- dbGetQuery(con,cfquery)

      if(nrow(ged) > 0 & nrow(cfs) > 0){
         ged_tl <- gedtimeline(ged)
         cfs_fixed <- fixcfs(cfs)

         timeline <- timelineplot(ged_tl,cfs_fixed,
                                  range = c(input$startyear,input$endyear))

         output$graph <- renderPlot(timeline)
      } else {
         output$graph <- NULL
      }
   })
}
