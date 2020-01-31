library(shiny)
library(shinydashboard)

dashboardPage(skin="green",
  dashboardHeader(title="PCCAT"),
  dashboardSidebar(
    
      sidebarMenu(
        menuItem("Introduction", tabName = "intro",icon= icon("info")),
        menuItem("Data Input", tabName = "input", icon = icon("file-upload")),
        menuItem("Principal Component Analysis", tabName = "pca",icon = icon("chart-bar")),
        menuItem("Clustering Analysis", tabName = "clustering",icon = icon("sitemap")),
        menuItem("Help", tabName = "help",icon = icon("question"))
      )
    
   ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              includeHTML("help.html")
      ),
      
      # Second tab content
      tabItem(tabName = "input",
              h2("Data Input and Preprocessing Options")
              
      ),
      tabItem(tabName = "pca",
              h2("Widgets tab content 2")
      ),
      tabItem(tabName = "clustering",
              h2("Widgets tab content 3")
      ),
      tabItem(tabName = "help",
              h2("Widgets tab content 4")
      )
    
    
  )
)
)