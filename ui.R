
popover <- function(content){
   tags$span(class = "glyphicon glyphicon-question-sign infofloat",
      `data-toggle`="popover",
      `data-trigger`="hover",
      `data-content`= content,
      style = "position:element(#attrib);")
}

fluidPage(
   includeScript('script.js'),
   includeScript("js/debounce.js"),
   includeScript("js/popper.min.js"),
   includeCSS('customization.css'),
   #tags$div(id= "spinner", class = "spinner-border text-primary loader", role = "status",
   #   tags$span(class = "sr-only", "Loading...")),
   tags$img(id = "ready", src = "sunglasses.jpg", class = "loader", style = "display:none"),
   tags$div(id = "ui", 
      sidebarLayout(
         sidebarPanel(id = "controlpanel", width = 3,
               tags$h1(id= "datatitle","ETH-PRIO Civil war Ceasefires Dataset"),
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
                  column(12,
                     popover("Only Countries included in both GED and the PRIO / ETH Ceasefire Dataset are included"),
                     selectInput('country','Country:',choices = NULL)
                  )
               ),
               fluidRow(
                  column(6,
                     numericInput('startyear','Start year',
                                   value = 1989,min = 1989,max = 2018)
                  ),
                  column(6,
                     numericInput('endyear','End year',value = 2018,min = 1989,max = 2018)
                  ),
	          downloadButton("download_plot","Download eps"),
		  popover("Download both plots in EPS format for publishing")
               ),
               tags$hr(),
               column(12,
                  fluidRow(
                     selectInput('coloring','Category variable:', choices = NULL)
                  ),
                  fluidRow(
                     popover("If one or more actors is selected, the plots only show the ceasefires / combat deaths related to the selected parties. Only actors present in both the GED and PRIO/ETH Ceasefires Dataset are shown."),
                     checkboxGroupInput('actors','Actors',NULL,FALSE)
                  ) 
               ),
               tags$hr(),
               fluidRow(
                  column(6, id = "sources",
                     HTML("<p>Casualty data from the 
                           <a href=\"https://ucdp.uu.se\">UCDP GED</a>
                           dataset.</p>")
                  ),
                  column(6, id = "attrib",
                     HTML("<p>Peder Landsverk 2019 (<a href=\"http://github.com/peder2911/Cfviz_Ged\">source code</a>)</p>")
                  )
              )
         ),
         mainPanel(id = "window", width = 9,
            fluidRow(
               column(12,
                  tags$div(class = "card",
                     plotOutput('timeline')
                  )
               )
            ),
            fluidRow(id = "lower",
               column(6,
                  tags$div(class="card",
                     tags$h3("Variable description:"),
                     textOutput("description")
                  )
               ),
               column(6,
                  tags$div(class = "card",
                     plotOutput('cake')
                  )
               )
            )
         )
      )
   )
)
