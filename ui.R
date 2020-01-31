library(shiny)
library(shinydashboard)

dashboardPage(skin="green",
  dashboardHeader(title="PCCAT"),
  dashboardSidebar(
    
      sidebarMenu(
        menuItem("Introduction", tabName = "intro",icon= icon("info")),
        menuItem("Data Input", tabName = "input", icon = icon("file-upload")),
        menuItem("Data Analysis", icon = icon("bar-chart-o"),
          menuSubItem("Principal Component Analysis", tabName = "pca"),
          menuSubItem("Clustering Analysis", tabName = "clustering")),
        menuItem("Help", tabName = "help",icon = icon("question"))
      )
    
   ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              img(src="MI_EGLE.png",height = '10%', width = '7.5%', align='left'),
              includeHTML("help.html")),
      
      # Second tab content
      tabItem(tabName = "input",
              h2("Data Input and Preprocessing Options",align="center"),
              fluidRow( 
                h4(strong("Data import (.csv)")),
                fileInput("file", label = (""))
              ),
              fluidRow(
                h4(strong("Select variables:")), #tags$hr(), 
                column(3, numericInput( "start", label = h5("Feature start col"), value=2, min=1)),
                column(3, selectInput( "color", label = h5("Color"), "")),
                column(3, selectInput( "size", label = h5("Size"), "")),
                column(3, numericInput( "k", label = h5("#clusters"), value=4, min=1))
              ),
              fluidRow(
                h4(strong("Data transformation Options:")),
                column(3, checkboxInput("log",label = "Log-transform?", FALSE)),
                column(3, checkboxInput("std",label = "Standardize?", TRUE)), 
                column(3, checkboxInput("ellipses", "Confidence ellipses?", FALSE)) 
              ),
              fluidRow(
                h4(strong("Data Analysis Plots Options:")),
                column(4, sliderInput("scatterD3_labsize", h5("Labels size"), min = 5, max = 25, value = 11)),
                column(4, sliderInput("scatterD3_opacity", h5("Points opacity"), min = 0, max = 1, value = 1, step = 0.05)),
                column(3, actionButton("goButton", "Run Analysis"))
              ),
              tags$p(
                h4(strong("Additional Options:")),
                actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
                actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
                tags$a(id = "scatterD3-svg-export", href = "#",
                       class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG"))
              )
          
      ),
      tabItem(tabName = "pca",
              h2("Principal Component Analysis",align="center"),
              plotOutput("plot1", width = "850px", height = "400px"),
              scatterD3Output("plot2", width = "850px", height = "450px")
              
      ),
      tabItem(tabName = "clustering",
              h2("Clustering Analysis",align="center"),
              plotOutput("plot3", width = "100%")
      ),
      tabItem(tabName = "help",
              h2("PCCAT Help")
      )
    
    
  )
)
)