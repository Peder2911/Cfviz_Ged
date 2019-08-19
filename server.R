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
getData <- dget('functions/getData.R')

# ================================

colors <- readRDS('data/colors.rds')
cfcodebook <- dget('data/cfcodebook.R')

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
   CFTABLE <- latestversion(alltables,'cf') 
   GEDTABLE <- latestversion(alltables,'ged') 

   # Choices setup ==================
   gedcountries <- dbGetQuery(con,glue('SELECT country FROM {GEDTABLE}')) %>%
      unique()    
   cfcountries <- dbGetQuery(con,glue('SELECT Location FROM {CFTABLE}')) %>%
      unique()
   allcountries <- intersect(gedcountries$country,cfcountries$location) %>%
      sort()

   dbDisconnect(con)

   updateSelectInput(session,'country',choices = allcountries)

   # ================================================
   # Update choices @ country change ================
   # ================================================
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

   # ================================================
   # Generate plot @ enterpress =====================
   # ================================================
   observeEvent({input$enterpress},{
      withProgress({
         con <- do.call(dbConnect,con_config)

         cntry <- input$country 

         ged <- getData(con, GEDTABLE, 'ged',cntry, input$startyear, input$endyear)
         cfs <- getData(con, CFTABLE,'cfs',cntry, input$startyear, input$endyear)

         incProgress(0.25)

         if(length(input$actors) > 0){
            ged <- ged %>% filter(side_a %in% input$actors|
                                  side_b %in% input$actors)

            cfs <- cfs %>% filter(actor_name %in% input$actors)
         }
         incProgress(0.25)

         if(nrow(ged) > 0 | nrow(cfs) > 0){

            ged_tl <<- gedtimeline(ged)
            incProgress(0.125)
            cfs_fixed <<- fixcfs(cfs, cfcodebook)
            incProgress(0.125)

            if(input$coloring == 'None' | is.null(input$coloring)){
               coloring <- NULL
            } else {
               coloring <- tolower(input$coloring)
            }

            timeline <- timelineplot(ged_tl, cfs_fixed,
                                     range = c(input$startyear,input$endyear),
                                     colors = colors, coloring = coloring)

            output$graph <- renderPlot(timeline)
         } else {
            # Placeholder 4 no data
            output$graph <- renderPlot(ggplot(tibble()) + 
               geom_text(aes(x = 1,y = 1, label = 'No data in date-range' ), size = 25) + 
               theme_void())
         }

         dbDisconnect(con)
         incProgress(0.25)
      })
   })

   output$download_data <- downloadHandler(filename = 'data.zip',
      content = function(file){
         dir <- tempdir()
         paths <- list(cfs = glue('{dir}/ceasefires.csv'),
                       ged = glue('{dir}/ged.csv'))
         write.csv(ged_tl, paths$ged, row.names = FALSE)
         write.csv(cfs_fixed, paths$cfs, row.names = FALSE)

         zip(file, unlist(paths), flags = '-r9Xj')
      }
   )
}
