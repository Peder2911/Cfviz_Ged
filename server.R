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
lookup <- dget('functions/lookup.R')

# ================================

COLORS <- readRDS('data/colors.rds')
CODEBOOK <- dget('data/cfcodebook.R')

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

   GEDTABLE <<- 'ged191'
   HEADTABLE <<- 'head'
   LOCATIONSTABLE <<- 'locations'
   CEASEFIRESTABLE <<- 'ceasefires'
   ACTORSTABLE <<- 'actors'

   #TODO Update the names of these for UI!
   # Currently placeholder to see variable names.
   CAT_VARIABLES <- c(Written='written',Purpose_1='purpose_1',Purpose_2='purpose_2',
                      Mediator_nego='mediator_nego', Implement='implement',
                      Enforcement='enforcement')

   # Choices setup ==================
   allcountries <- lapply(list(GEDTABLE,LOCATIONSTABLE), function(TABLE){
      dbGetQuery(con, glue('SELECT location FROM {TABLE}')) %>%
         unlist() %>%
         unique()})          

   countries <-  do.call(intersect, allcountries) %>%
         sort()

   updateSelectInput(session,'country',choices = countries)

   updateSelectInput(session,'coloring',choices = names(CAT_VARIABLES)) 

   dbDisconnect(con)


   # ================================================
   # Update choices @ country change ================
   # ================================================
   observeEvent({input$country},{
       con <- do.call(dbConnect,con_config)

      cfactors <- dbGetQuery(con,glue("SELECT actor_name FROM {LOCATIONSTABLE} 
                                       JOIN head ON head.cc=locations.cc
                                       JOIN actors ON head.acid=actors.acid 
                                       WHERE locations.location = '{input$country}'"))
      gedactors <- dbGetQuery(con,glue("SELECT side_a,side_b FROM {GEDTABLE}
                                        WHERE location = '{input$country}'"))

      actors <- sapply(list(cfactors,gedactors), unlist) %>%
        do.call(intersect, .)
      # REPLACES ^ =====================================

      updateCheckboxGroupInput(session,'actors',choices = actors, selected = FALSE)
      dbDisconnect(con)
      })

   # ================================================
   # Generate plot @ enterpress =====================
   # ================================================
   observeEvent({input$enterpress},{
      con <- do.call(dbConnect,con_config)

      # ================================================
      # Sort out queries ===============================

      gedquery <- "SELECT * FROM {GEDTABLE} WHERE location='{input$country}'"
      variables <- c('{CEASEFIRESTABLE}.cf_effect_yr as year',
                     '{CEASEFIRESTABLE}.cf_effect_month as month',
                     '{CEASEFIRESTABLE}.cf_effect_day as day',
                     '{ACTORSTABLE}.actor_name actor') 

      if(!input$coloring == 'None' |! is.null(input$coloring)){
         coloring <<- CAT_VARIABLES[input$coloring]
         catvar <- '{CEASEFIRESTABLE}.{coloring} as category'
      } else {
         catvar <- 'null as category'
      }

      variables <- sapply(c(variables,catvar),glue)
      variables <- glue_collapse(variables, sep = ', ')

      cfquery <- "SELECT {variables} FROM {LOCATIONSTABLE} 
                  JOIN {HEADTABLE} ON {HEADTABLE}.cc={LOCATIONSTABLE}.cc
                  JOIN {CEASEFIRESTABLE} ON {HEADTABLE}.locid={CEASEFIRESTABLE}.locid
                  JOIN {ACTORSTABLE} ON {HEADTABLE}.acid={ACTORSTABLE}.acid
                  WHERE {LOCATIONSTABLE}.location='{input$country}'"

      startyear <- input$startyear
      endyear <- input$endyear

      if(startyear > 1989 | endyear < 2019){
         gedquery <- gedquery %>%
            paste0(' AND (year >= {startyear} AND year <= {endyear})')
         cfquery <- cfquery %>%
            paste0(' AND ({CEASEFIRESTABLE}.cf_effect_yr >= {startyear}
                     AND {CEASEFIRESTABLE}.cf_effect_yr <= {endyear})')
      }

      ged <<- dbGetQuery(con, glue(gedquery))
      cfs <<- dbGetQuery(con, glue(cfquery)) %>%
         mutate(start = ymd(glue('{year}-{month}-{day}')), 
                category = factor(lookup(category,CODEBOOK[[coloring]])))

      # Might change this?
      # Perhaps move to query - stage for Ceasefire data...
      if(length(input$actors) > 0){
         ged <- ged %>% filter(side_a %in% input$actors|
                               side_b %in% input$actors)

         cfs <- cfs %>% filter(str_detect(actor,input$actors))
      }

      # Plot or no plot
      if(nrow(ged) > 0 & nrow(cfs) > 0){

         # Collapse names
         cfs <- cfs %>% group_by_at(names(cfs)[names(cfs) != 'actor']) %>%
            summarize(actor = glue_collapse(actor, sep = ' - ')) %>%
            ungroup()

         timeline <- timelineplot(ged, cfs,
                                  range = c(input$startyear,input$endyear),
                                  gedtype = input$gedtype,
                                  categoryName = input$coloring,
                                  colors = COLORS)

         currentplot <<- timeline
         output$graph <- renderPlot(timeline)
      } else {
         # Placeholder 4 no data
         output$graph <- renderPlot(ggplot(tibble()) + 
            geom_text(aes(x = 1,y = 1, label = 'No data in date-range' ), size = 25) + 
            theme_void())
      }
      dbDisconnect(con)
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
   output$download_plot <- downloadHandler(filename = 'plot.rds',
      content = function(file){
         saveRDS(currentplot,version = 2,file)
      }
   )
}
