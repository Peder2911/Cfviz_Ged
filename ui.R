fluidPage(
   includeScript('script.js'),
   includeCSS('customization.css'),
   sidebarLayout(
      sidebarPanel(id = "controlpanel", width = 3,
            fluidRow(
               tags$div(class = "gfg",
                  column(6,
                     tags$img(src = "priologo.png")
                  ),
                  column(6,
                     tags$img(src = "ethsmall.png")
                  ),
                  tags$br()
               )
            ),
            tags$hr(),
            fluidRow(
               selectInput('country','Country:',choices = NULL)
            ),
            fluidRow(
               actionButton('refresh','Refresh',icon = icon('redo')) 
            ),
            tags$hr(),
            fluidRow(
               column(6,
                  numericInput('startyear','Start year',
                                value = 1989,min = 1989,max = 2019)
               ),
               column(6,
                  numericInput('endyear','End year',value = 2019,min = 1989,max = 2019)
               )
            ),
            tags$hr(),
            fluidRow(
               selectInput('coloring','Coloring:', choices = NULL),
               checkboxGroupInput('actors','Actors',NULL,FALSE)
            ),
            fluidRow(
               downloadButton('download_data','Download Data'),
               downloadButton('download_plot','Download EPS')
            ),
            tags$hr(),
            fluidRow(
               column(6, id = "sources",
                  HTML("<p>Casualty data from the 
                        <a href=\"https://ucdp.uu.se\">UCDP GED</a>
                        dataset.</p>")
               ),
               column(6, id = "attrib",
                  HTML("<p>Peder G. Landsverk 2019 (<a href=\"http://github.com/peder2911/Cfviz_Ged\">source code</a>)</p>")
               )
           )
      ),
      mainPanel(id = "window", width = 9,
         fluidRow(id = "headline",
            tags$div(class = "container",
               tags$h1("Ceasefires and casualites")
            )
         ),
         fluidRow(
            column(12,
               plotOutput('timeline')
            )
         ),
         fluidRow(id = "lower",
            column(6,
               tags$div(class="card",
                  tags$div(class="container",
                     textOutput("description")
                  )
               )
            ),
            column(6,
               plotOutput('cake')
            )
         )
      )
   )
)
