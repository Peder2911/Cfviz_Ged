shh <- suppressPackageStartupMessages
shh(library(RPostgreSQL))
shh(library(glue))
shh(library(ggplot2))
shh(library(magrittr))
shh(library(dplyr))
shh(library(stringr))
shh(library(lubridate))
shh(library(yaml))
shh(library(tools))

options(warn = -1)

# Local functions ================

timelineplot <- dget('functions/timelineplot.R')
latestversion <- dget('functions/latestversion.R')

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
   allcountries <- lapply(list(GEDTABLE,CFTABLE), function(TABLE){
      dbGetQuery(con, glue('SELECT location FROM {TABLE}')) %>%
         unlist() %>%
         unique()
      }) %>%
      do.call(intersect, .)

   # Cat variables setup ============================
   infoquery <- dbSendQuery(con,glue('SELECT * FROM {CFTABLE}'))
   cfnames <- dbColumnInfo(infoquery)[['name']]
   dbClearResult(infoquery)

   catvars <- cfnames[str_detect(cfnames,'^cat_')]
   catvars <- sapply(catvars, function(vname){
      str_extract(vname, '(?<=cat_)[a-zA-Z]+') %>%
         toTitleCase()
   })

   dbDisconnect(con)

   updateSelectInput(session,'country',choices = allcountries)
   updateSelectInput(session,'coloring',choices = catvars)

   # ================================================
   # Update choices @ country change ================
   # ================================================
   observeEvent({input$country},{
       con <- do.call(dbConnect,con_config)
       # Get all unique actors

      actorquery <- 'SELECT {glue_collapse(actorvars,sep = \',\')} FROM {table} WHERE {locvar}=\'{input$country}\''

      gedquery <- 'SELECT side_a,side_b FROM {GEDTABLE}' %>%
         paste0(' WHERE location=\'{input$country}\'' )
      gedactors <- unique(unlist(dbGetQuery(con,glue(gedquery))))

      cfquery <- 'SELECT name FROM {CFTABLE}' %>%
         paste0(' WHERE location=\'{input$country}\'')
      ceasefireactors <- unlist(dbGetQuery(con,glue(cfquery)))
      ceasefireactors <- ceasefireactors %>%
         str_split(' *- *') %>%
         do.call(c, .) %>%
         unique()

      actors <- intersect(gedactors,ceasefireactors)

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

         
         gedquery <- "SELECT * FROM {GEDTABLE} WHERE location = '{cntry}'"
         cfquery <- "SELECT * FROM {CFTABLE} WHERE location = '{cntry}'"

         startyear <- input$startyear
         endyear <- input$endyear

         if(startyear > 1989 | endyear < 2019){
            gedquery <- gedquery %>%
               paste0(' AND (year >= {startyear} AND year <= {endyear})')
            cfquery <- cfquery %>%
               paste0(' AND (date_part(\'year\',start) >= {startyear}') %>%
               paste0(' AND date_part(\'year\',start) <= {endyear})')
         }
         ged <<- dbGetQuery(con, glue(gedquery))
         cfs <<- dbGetQuery(con, glue(cfquery))

         incProgress(0.25)

         if(length(input$actors) > 0){
            ged <- ged %>% filter(side_a %in% input$actors|
                                  side_b %in% input$actors)

            cfs <- cfs %>% filter(str_detect(name,input$actors))
         }
         incProgress(0.25)

         if(nrow(ged) > 0 | nrow(cfs) > 0){
            if(input$coloring == 'None' | is.null(input$coloring)){
               coloring <- NULL
            } else {
               coloring <- paste0('cat_',tolower(input$coloring))
            }

            timeline <- timelineplot(ged, cfs,
                                     range = c(input$startyear,input$endyear),
                                     gedtype = input$gedtype,
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
         write.csv(ged, paths$ged, row.names = FALSE)
         write.csv(cfs, paths$cfs, row.names = FALSE)

         zip(file, unlist(paths), flags = '-r9Xj')
      }
   )
}
