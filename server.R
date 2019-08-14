shh <- suppressPackageStartupMessages
shh(library(RPostgreSQL))
shh(library(glue))
shh(library(ggplot2))
shh(library(magrittr))
shh(library(dplyr))
shh(library(stringr))
shh(library(lubridate))
shh(library(yaml))

options(warn = -1)

# Local functions ================
gedtimeline <- dget('functions/gedtimeline.R')
fixcfs <- dget('functions/fixcfs.R')
fixacd <- dget('functions/fixacd.R')
timelineplot <- dget('functions/timelineplot.R')
latestversion <- dget('functions/latestversion.R')

# ================================

if(Sys.getenv('GED_CONFIG') == ""){
   fpath <- 'config.Robj'
} else {
   fpath <- Sys.getenv('GED_CONFIG')
}

config <- dget(fpath)

con_config <- config$con
dr <- dbDriver('PostgreSQL')
con_config$dr <- dr

server <- function(input, output, session){

   # SQL setup ======================

   con <- do.call(dbConnect,con_config)

   alltables <- dbListTables(con)
   ACDTABLE <- latestversion(alltables,'acd')  
   CFTABLE <- latestversion(alltables,'cf') 
   GEDTABLE <- latestversion(alltables,'ged') 

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
   dbDisconnect(con)

   updateSelectInput(session,'country',choices = allcountries)

   observeEvent({input$country},{
       con <- do.call(dbConnect,con_config)
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
      dbDisconnect(con)
      })

   # Do plot when cntry changes =====
   observeEvent({input$enterpress},{
      con <- do.call(dbConnect,con_config)
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
   dbDisconnect(con)
   })
}
