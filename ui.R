#########
# PCCAT #
#########



library(shiny)
library(scatterD3)


shinyUI(fluidPage(
  titlePanel('PCCAT: Principle Component and Clustering Analysis Tool'),
  
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      # img(src="logo.jpg",height = 80, width = '100%', align='center'),
      #includeMarkdown("help.Rmd"),
      includeHTML("help.html"),
      br(),
      fileInput("file", label = ("Data import (.csv)")),
      #p(strong('Note:'), 'Only comma delimited (.csv) files will be accepted.'),
      tags$head(
        tags$style(HTML('#goButton{background-color:rgba(0,255,0,.3)}'))
      ),
      fluidRow(
        # h5(strong("Select variables:")), #tags$hr(), 
        column(3, numericInput( "start", label = h5("Feature start col"), value=2, min=1)),
        column(3, selectInput( "color", label = h5("Color"), "")),
        column(3, selectInput( "size", label = h5("Size"), "")),
        column(3, numericInput( "k", label = h5("#clusters"), value=4, min=1))
        # column(3, selectInput( "symbol", label = h5("Symbol"), ""))#,
        # column(3, selectInput( "opacity", label = h5("Opacity"), "")),
      ),
      fluidRow(
        column(3, checkboxInput("log",label = "Log-transform?", FALSE)),
        column(3, checkboxInput("std",label = "Standardize?", TRUE)), 
        column(3, checkboxInput("ellipses", "Confidence ellipses?", FALSE)) 
      ),
      fluidRow(
        column(4, sliderInput("scatterD3_labsize", h5("Labels size"), min = 5, max = 25, value = 11)),
        column(4, sliderInput("scatterD3_opacity", h5("Points opacity"), min = 0, max = 1, value = 1, step = 0.05)),
        column(3, actionButton("goButton", "Run Analysis"))
      ),
      tags$p(
        actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
        actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
        tags$a(id = "scatterD3-svg-export", href = "#",
               class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG"))
      )
      # tags$hr(), 
      # hr(),
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(
          "PCA",
          plotOutput("plot1", width = "850px", height = "400px"),
          # hr(),
          scatterD3Output("plot2", width = "850px", height = "450px"), 
          hr()
        ), 
        
        tabPanel(
          "Clustering",
          plotOutput("plot3", width = "100%"),
          hr()
        )#,
        
        # tabPanel(
        #   "Document", 
        #   tags$iframe(style="height:660px; width:70%; scrolling=yes", 
        #               src="report.pdf#page=1&zoom=75")
        # )
        
      )             
    )
    
  )
  
))

