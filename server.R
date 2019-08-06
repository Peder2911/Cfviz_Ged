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
timelinePlot <- dget('functions/timelineplot.R')

server <- function(input, output, session){
   # SQL setup ======================
   dr <- dbDriver('PostgreSQL')
   con <- dbConnect(dr, db = Sys.getenv('GED_DB'), 
                    user = Sys.getenv('GED_USER'), 
                    password = Sys.getenv('GED_PASS'),
                    host = Sys.getenv('GED_HOST'), 
                    port = Sys.getenv('GED_PORT'))

   TABLE <- Sys.getenv('GED_TABLE')

   # Choices setup ==================
   allcountries <- dbGetQuery(con,glue('SELECT country FROM {TABLE}')) %>%
      unique() %>%
      arrange(country)


   updateSelectInput(session,'country',choices = allcountries)

   # Do plot when cntry changes =====
   observeEvent(input$country,{
      cntry <- input$country 
      ged <- dbGetQuery(con,glue('SELECT * FROM {TABLE} WHERE country=\'{cntry}\''))
      if(nrow(ged) > 0){
         ged_tl <- gedtimeline(ged)
         timeline <- timelinePlot(ged_tl)

         output$graph <- renderPlot(timeline)
      } else {
         output$graph <- NULL
      }
   })

   #output$debug <- renderText(glue('{length(allcountries)} countries considered'))
}
