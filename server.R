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
fixacd <- dget('functions/fixacd.R')
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

   alltables <- dbListTables(con)
   ACDTABLE <- latestversion(alltables,'acd')  
   CFTABLE <- latestversion(alltables,'cf') 

   # Choices setup ==================
   gedcountries <- dbGetQuery(con,glue('SELECT country FROM {GEDTABLE}')) %>%
      unique()    
   cfcountries <- dbGetQuery(con,glue('SELECT Location FROM {CFTABLE}')) %>%
      unique()
   allcountries <- intersect(gedcountries$country,cfcountries$location) %>%
      sort()

   acdvars <- c('conflict_id','side_a','side_b')
   acdquery <- glue('SELECT {glue_collapse(acdvars,sep = ",")} FROM {ACDTABLE}')
   acd <- dbGetQuery(con,acdquery)
   acd <- acd %>% fixacd()

   updateSelectInput(session,'country',choices = allcountries)

   observeEvent({input$country},{
      # Get all unique actors

      actorquery <- 'SELECT {glue_collapse(actorvars,sep = \',\')} FROM {table} WHERE {locvar}=\'{input$country}\''

      table <- GEDTABLE 
      locvar <- 'country'
      actorvars <- c('side_a','side_b')
      gedactors <- dbGetQuery(con,glue(actorquery))

      table <- CFTABLE 
      locvar <- 'location'
      actorvars <- 'actor_name'
      ceasefireactors <- dbGetQuery(con,glue(actorquery))


      actors <- lapply(list(gedactors,ceasefireactors),function(data){
         do.call(c,data)
         }) 
      actors <- intersect(actors[[1]],actors[[2]])

      updateCheckboxGroupInput(session,'actors',choices = actors, selected = FALSE)

      locvar <- 'location'
      })

   # Do plot when cntry changes =====
   observeEvent({input$enterpress},{
      withProgress({
         cntry <- input$country 
         gedquery <- glue('SELECT * FROM {GEDTABLE} WHERE country=\'{cntry}\'')
         cfquery <- glue('SELECT * FROM {CFTABLE} WHERE location=\'{cntry}\'')

         if(input$startyear > 1989 | input$endyear < 2019){
            yrtemplate <- 'AND ({yrvar} >= {input$startyear} AND {yrvar} <= {input$endyear})'
            yrvar <- 'year'
            gedcond <- glue(yrtemplate)
            yrvar <- 'cf_dec_yr'
            cfcond <- glue(yrtemplate)

            gedquery <- glue('{gedquery} {gedcond}')
            cfquery <- glue('{cfquery} {cfcond}')
         }


         ged <- dbGetQuery(con,gedquery)
         cfs <- dbGetQuery(con,cfquery)
         incProgress(0.25)

         if(length(input$actors) > 0){
            DIST <- 0.4
            ged <- ged %>% filter(side_a %in% input$actors|
                                  side_b %in% input$actors)

            cfs <- cfs %>% filter(actor_name %in% input$actors)
         }

         incProgress(0.25)

         if(nrow(ged) > 0 | nrow(cfs) > 0){
            ged_tl <- gedtimeline(ged)
            incProgress(0.125)
            cfs_fixed <- fixcfs(cfs) #TODO add acd naming
            incProgress(0.125)

            timeline <- timelineplot(ged_tl,cfs_fixed,acd,
                                     usenames = FALSE,
                                     range = c(input$startyear,input$endyear))

            output$graph <- renderPlot(timeline)
         } else {
            output$graph <- renderPlot(ggplot(tibble()) + geom_text(aes(x = 1,y = 1, label = 'No data in date-range' ), size = 25) + theme_void())
         }
         incProgress(0.25)
      })
   })
}
