shh <- suppressPackageStartupMessages
shh(library(RPostgreSQL))
shh(library(RSQLite))
shh(library(glue))
shh(library(ggplot2))
shh(library(magrittr))
shh(library(dplyr))
shh(library(stringr))
shh(library(lubridate))
shh(library(yaml))
shh(library(tools))
shh(library(jsonlite))

# ================================================
# Local functions ================================

timelineplot <- dget('functions/timelineplot.R')
cakeplot <- dget('functions/cake.R')

zerodays <- dget('functions/zerodays.R')
lookup <- dget('functions/lookup.R')

# ================================================

COLORS <- readRDS('data/colors.rds')
CODEBOOK <- dget('data/cfcodebook.R')

# ================================================
# Config, check if testing =======================
# ================================================

if(Sys.getenv('GED_CONFIG') == ""){
   fpath <- 'config.Robj'
} else {
   fpath <- Sys.getenv('GED_CONFIG')
}

testing <- interactive() | 
   getOption("shiny.testmode",FALSE) |
   !Sys.getenv("TESTING") == ''

if(testing){
   if(interactive()) writeLines("\x1b[33;33;33mInteractive\x1b[0m")
   if(getOption("shiny.testmode",FALSE)) writeLines("\x1b[33;33;33mTesting mode\x1b[0m")
   if(!Sys.getenv("TESTING") == '') writeLines("\x1b[33;33;33mENV variable\x1b[0m")
}

if(!testing){
   # Assume it's in a docker environment, and can reach a database
   # @ database:5432
   message("*** NORMAL MODE ***")
   config <- dget(fpath)
   con_config <- config$con
   dr <- dbDriver('PostgreSQL')
   con_config$dr <- dr
   #dburl <- glue("http://{con_config$host}:{con_config$port}")
   #e <- tryCatch(httr::GET(dburl), error = function(e) e)
   #if(any(class(e) == "error")) stop(glue("Could not reach database @ {dburl}!"))
} else {
   # Assume its being tested, and can find data in an SQLite file 
   # @ the CWD
   message("*** TESTING MODE ***")
   con_config <- list(SQLite(), dbname = "test.sqlite")
}

# ================================================
# Server function ================================
# ================================================

server <- function(input, output, session){

   # SQL setup ======================

   con <- do.call(dbConnect,con_config)

   GEDTABLE <<- 'ged191'
   HEADTABLE <<- 'head'
   LOCATIONSTABLE <<- 'locations'
   CEASEFIRESTABLE <<- 'ceasefires'
   ACTORSTABLE <<- 'actors'

   CAT_VARIABLES <- read_json("data/variables.json") %>%
      bind_rows()

   # Choices setup ==================
   allcountries <- lapply(list(GEDTABLE,LOCATIONSTABLE), function(TABLE){
      dbGetQuery(con, glue('SELECT location FROM {TABLE}')) %>%
         unlist() %>%
         unique()})          

   countries <- do.call(intersect, allcountries) %>%
         sort()

   updateSelectInput(session,'country',choices = countries)

   updateSelectInput(session,'coloring',choices = CAT_VARIABLES$alias) 

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

      actors <- lapply(list(cfactors,gedactors), unlist) %>%
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
                     '{CEASEFIRESTABLE}.locid as id',
                     '{ACTORSTABLE}.actor_name actor') 

      if(!input$coloring == 'None' |! is.null(input$coloring)){

         coloring_variable <<- CAT_VARIABLES[CAT_VARIABLES$alias == input$coloring,]
         catvar <- '{CEASEFIRESTABLE}.{coloring_variable$name} as category'
      } else {
         coloring_variable <<- NULL
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

      if(startyear > 1989 | endyear < 2018){
         gedquery <- gedquery %>%
            paste0(' AND (year >= {startyear} AND year <= {endyear})')
         cfquery <- cfquery %>%
            paste0(' AND ({CEASEFIRESTABLE}.cf_effect_yr >= {startyear}
                     AND {CEASEFIRESTABLE}.cf_effect_yr <= {endyear})')
      }

      # no intergovernmental
      gedquery  <- gedquery %>%
         paste0(" AND NOT (side_a LIKE 'Government%' AND side_b LIKE 'Government%')")

      ged <- dbGetQuery(con, glue(gedquery))

      cfs <- dbGetQuery(con, glue(cfquery)) %>%
         mutate(start = ymd(glue('{year}-{month}-{day}')), 
                category = factor(lookup(category,CODEBOOK[[coloring_variable$name]])))

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
            summarize(actor = glue_collapse(sort(unique(actor)), sep = ' - ')) %>%
            ungroup()

         # Bin GED dates
         grouping <- local({
            yrdiff <- input$endyear - input$startyear
            if(yrdiff >= 25){
               "3 months"
            } else if(yrdiff > 6){
               "months"
            } else {
               "weeks"
            }
         })

         class(ged$date) <- "Date"
         ged <- ged %>% 
            group_by(date = floor_date(date,grouping)) %>%
            summarize(cnt = sum(cnt))
         startdate <- ymd(glue("{startyear}-01-01"))
         enddate <- ymd(glue("{endyear}-01-01"))
         ged <- zerodays(ged,date,cnt,grouping)
                         

         ged <<- ged
         cfs <<- cfs
         timeline <- timelineplot(ged, cfs,
                                  range = c(input$startyear,input$endyear),
                                  gedtype = input$gedtype,
                                  categoryName = input$coloring,
                                  colors = COLORS,
                                  dategrouping = grouping)
         cake <- cakeplot(cfs, colors = COLORS)

         currentplot <<- timeline

         output$timeline <- renderPlot(timeline)
         output$cake <- renderPlot(cake)
         output$description <- renderText(coloring_variable$description)
      } else {
         # Placeholder 4 no data
         output$timeline <- renderPlot(ggplot(tibble()) + 
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
   output$download_plot <- downloadHandler(filename = 'ged_cfs.eps',
      content = function(file){
         ggsave(file,currentplot,device = 'eps',height = 6, width = 14)
      }
   )
}
