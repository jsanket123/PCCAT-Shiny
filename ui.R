library(shiny)
library(shinydashboard)
library(scatterD3)
library(shinyalert)
library(pca3d)
library(plotly)
library(RLumShiny)

dashboardPage(
  skin="green",
  
  dashboardHeader(title="PCCAT"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Introduction", tabName = "intro",icon= icon("info")),
      menuItem("Data Input", tabName = "input", icon = icon("file-upload")),
      menuItem("Data Analysis", icon = icon("bar-chart-o"),startExpanded = TRUE,
               menuSubItem("Principal Component Analysis", tabName = "pca"),
               menuSubItem("Clustering Analysis", tabName = "clustering")),
      menuItem("Help", tabName = "help",icon = icon("question"))
    )
    
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              img(src="MI_EGLE.png",height = '10%', width = '7.5%', align='left',alt='EGLE logo'),
              includeHTML("help.html")),
      
      # Second tab content
      tabItem(tabName = "input",
              h2("Data Input and Preprocessing Options",align="center"),
              
              h4(strong("Data import (.csv)")),
              fluidRow(
                #tags$style(".shiny-file-input-progress {display:none}"),
                column(12,fileInput("file", label = ("")),offset=0.2)
              ),
              
              h4(strong("Select variables:")),
              fluidRow(
                style = "margin-top:-1em",
                column(3, numericInput( "start", label = h5("Numerical Feature Start Column"), value=3, min=1),offset = 0.2)
              ),
              
              h4(strong("Data transformation Options:")),
              fluidRow(
                style = "margin-top:-1em",
                column(3, checkboxInput("std",label = "Standardize?", TRUE),offset = 0.2),
                column(3, checkboxInput("log",label = "Log-transform?", FALSE)) 
              ),
              
              br(),
              fluidRow(
                useShinyalert(),
                column(4, actionButton("goButton", "Run Analysis",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
              ),
              br(),
              
              # h4(strong("Additional Options:")),
              # fluidRow(
              #   tags$p(
              #   actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
              #   actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
              #   tags$a(id = "scatterD3-svg-export", href = "#",
              #          class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG"))
              # ))
              
      ),
      tabItem(tabName = "pca",
              h2("Principal Component Analysis",align="center"),
              fluidRow(
                tabBox(
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset", height = "400px",width = 12,
                  tabPanel("Variance Plot",plotOutput("pca_plot1", width = "100%", height = "450px"),textOutput("text1"),
                           tags$head(tags$style("#text1{color: blue;
                                 font-size: 18px;
                                 font-style: normal;
                                 }"))
                           ),
                  tabPanel("2D Scatter Plot",scatterD3Output("pca_plot2", width = "100%", height = "450px"),
                           fluidRow(
                             style = "margin-top:-1em",
                             column(4, selectInput( "color", label = h5("Color"), "")),
                             column(4, selectInput( "size", label = h5("Size"), ""))
                           ),
                           fluidRow(
                             column(4, sliderInput("scatterD3_labsize", h5("Labels size"), min = 5, max = 25, value = 11),offset = 0.2),
                             column(4, sliderInput("scatterD3_opacity", h5("Points opacity"), min = 0, max = 1, value = 1, step = 0.05))
                           ),
                           fluidRow(
                             column(4, checkboxInput("ellipses", "Confidence ellipses?", FALSE))
                           ),
                  ),
              
                  tabPanel("3D Scatter Plot",plotlyOutput("pca_plot4", width = "100%", height = "500px"),
                           fluidRow(
                             style = "margin-top:-1em",
                             column(4, selectInput( "color3D", label = h5("Color"), "")),
                             column(4, selectInput( "size3D", label = h5("Size"), "")),
                             column(4,popover(title="What does 3D plot show?", content="3D plot shows each observation on fisrt three PCs coordinate system"))
                           ),
                           fluidRow(
                             
                             column(4, sliderInput("opacity_3D", h5("Points opacity"), min = 0, max = 1, value = 1, step = 0.05))
                             
                           ),
                           
                          # fluidRow( popover(title=icon("question-circle"), content="3D plot shows each observation on fisrt three PCs coordinate system"))#try if it works   
                  )
                ))
      ),
      tabItem(tabName = "clustering",
              h2("Clustering Analysis",align="center"),
              fluidRow(
                tabBox(
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset", height = "400px",width = 12,
                  tabPanel("Partitional Clustering",plotOutput("cl_plot1", width = "100%", height = "400px"),
                           fluidRow(
                             column(3, numericInput( "k1", label = h5("Number of Clusters"), value=4, min=1))
                           )
                  ),
                  tabPanel("Hierarchical Clustering",plotOutput("cl_plot2", width = "100%", height = "400px"),
                           tableOutput("cl_table"),
                           fluidRow(
                             column(3, numericInput( "k2", label = h5("Number of Clusters"), value=4, min=1)),
                             column(3, numericInput("k3", label = h5("Column with Label"), value=1, min=1)),
                             column(3, numericInput("k4", label = h5("Which Cluster for Table"), value=1, min=1))
                           )
                  )
                ))
      ),
      tabItem(tabName = "help",
              h2("PCCAT Help")
      )
    )
  )
)

