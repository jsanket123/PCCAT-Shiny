###############################################################
#   PCCAT: Principle Component and Clustering Analysis Tool   #
###############################################################

# User Interface (ui.R)

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(scatterD3))
suppressMessages(library(shinyalert))
suppressMessages(library(pca3d))
suppressMessages(library(plotly))
suppressMessages(library(RLumShiny))
suppressMessages(library(shinyWidgets))

dashboardPage(
  skin="green",
  
  dashboardHeader(title="PCCAT"),
  
  dashboardSidebar(
    sidebarMenu(id='sidebar',
      menuItem("Introduction", tabName = "intro",icon= icon("info")),
      menuItem("Data Input", tabName = "input", icon = icon("file-upload")),
      menuItem("Data Analysis", icon = icon("bar-chart-o"),startExpanded = TRUE,
        menuSubItem("Data Exploration & Summary", tabName = "summary"),
        menuSubItem("Principal Component Analysis", tabName = "pca"),
        menuSubItem("Clustering Analysis", tabName = "clustering")),
      menuItem("Help", tabName = "help",icon = icon("question"))
    ),
    uiOutput('style_tag')
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
        fluidRow(
          column(2, style="text-align: center;", box(width = NULL, img(src="CSTATLogo.png",height = '100%', width = '100%', alt='EGLE logo'))),
          column(8, includeHTML("intro.html")),
          column(2, style="text-align: center;", box(width = NULL, img(src="MI_EGLE.png",height = '100%', width = '100%', align='middle',alt='EGLE logo')))
        )
      ),
      
      # Second tab content
      tabItem(tabName = "input",
        h2("Data Input and Preprocessing Options",align="center"),
        
        h4(strong("Data import (.csv)")),
        fluidRow(
          column(12,fileInput("file", label = ("")),offset=0.2)
        ),
        textOutput("text2"),
        tags$head(tags$style("#text2{color: blue; font-size: 18px; font-style: normal;}")),
      
        
        h4(strong("Select variables:")),
        fluidRow(
          style = "margin-top:-1em",
          column(3, numericInput( "start", label = h5("Numerical Variables Start Column"), value=3, min=1),offset = 0.2)
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
        br()
      ),
      
      # Third-I tab content
      tabItem(tabName = "summary",
        h2("Data Exploration & Summary", align = "center"),
        fluidRow(
          style = "margin-top:1em",
          column(6, selectInput( "var", label = h5("Variable"), ""))
        ),
        plotOutput("sum_plot", width = "75%", height = "600px"),
        br(),
        fluidRow(
          column(9, offset = 0.2,verbatimTextOutput("sum", placeholder = TRUE))
        )
      ),
      
      # Third-II tab content
      tabItem(tabName = "pca",
        h2("Principal Component Analysis",align="center"),
        fluidRow(
          tabBox(
            id = "tabset", height = "400px",width = 12,
            tabPanel("Variance Plot",
              plotOutput("pca_plot1", width = "100%", height = "450px"),
              textOutput("text1"),
              tags$head(tags$style("#text1{color: blue; font-size: 18px; font-style: normal;}"))
            ),
            tabPanel("2D Scatter Plot",
              fluidRow(
                column(4, selectInput( "D2_pc_x", label = h5("PC to be displayed on x-axis"), "")),
                column(4, selectInput( "D2_pc_y", label = h5("PC to be displayed on y-axis"), ""))
              ),
              br(),
              scatterD3Output("pca_plot2", width = "100%", height = "450px"),
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
              )
            ),
            tabPanel("3D Scatter Plot",
              fluidRow(
                column(4, selectInput( "D3_pc_x", label = h5("PC to be displayed on x-axis"), "")),
                column(4, selectInput( "D3_pc_y", label = h5("PC to be displayed on y-axis"), "")),
                column(4, selectInput( "D3_pc_z", label = h5("PC to be displayed on z-axis"), ""))
              ),
              br(),
              plotlyOutput("pca_plot4", width = "100%", height = "500px"),
              fluidRow(
                style = "margin-top:3em",
                column(4, selectInput( "color3D", label = h5("Color"), "")),
                column(4, selectInput( "size3D", label = h5("Size"), "")),
                column(4, popover(title="What does 3D plot show?", content="3D plot shows each observation on first three PCs coordinate system"))
              ),
              fluidRow(
                column(4, sliderInput("opacity_3D", h5("Points opacity"), min = 0, max = 1, value = 1, step = 0.05))
              )
            ),
            tabPanel("Variable Importance",
              fluidRow(
                column(4, selectInput( "pc_sort", label = h5("Sort the variables by their loading to PC"), ""))
              ),
              br(),
              #tableOutput("pca_table")
              div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", tableOutput('pca_table'))
            )
          )
        )
      ),
      
      # Third-III tab content
      tabItem(tabName = "clustering",
        h2("Clustering Analysis",align="center"),
        fluidRow(
          tabBox(
            id = "tabset", height = "400px",width = 12,
            tabPanel("Partitional Clustering",plotOutput("cl_plot1", width = "100%", height = "400px"),
              fluidRow(
                column(3, numericInput( "k1", label = h5("Number of Clusters"), value=2, min=1))
              )
            ),
            tabPanel("Hierarchical Clustering",plotOutput("cl_plot2", width = "100%", height = "400px"),
              fluidRow(
                column(3, numericInput( "k2", label = h5("Number of Clusters"), value=2, min=1)),
                column(3, numericInput("k3", label = h5("Column with Label"), value=1, min=1)),
                column(3, numericInput("k4", label = h5("Which Cluster for Table"), value=1, min=1))
              ),
              #tableOutput("cl_table")
              div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", tableOutput('cl_table'))
            )
          )
        )
      ),
      
      # Fourth tab content
      tabItem(tabName = "help",
        includeHTML("help.html")
      )
    )
  )
)