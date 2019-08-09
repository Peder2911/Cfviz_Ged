fluidPage(
   includeScript('script.js'),
   sidebarLayout(
      sidebarPanel(
         fluidRow(
            selectInput('country','Country:',choices = NULL),
            actionButton('refresh','Refresh',icon = icon('redo')) 
         ),
         fluidRow(
            numericInput('startyear','Start year',value = 1989,min = 1989,max = 2019),
            numericInput('endyear','End year',value = 2019,min = 1989,max = 2019),
            #checkboxInput('usenames','Use names',FALSE),
            checkboxGroupInput('actors','Actors',NULL,FALSE)
         )
      ),
      mainPanel(
         plotOutput('graph')
      )
   )
)
