fluidPage(
   sidebarLayout(
      sidebarPanel(
         selectInput('country','Country:',choices = NULL)
      ),
      mainPanel(
         plotOutput('graph')
      )
   )
)
