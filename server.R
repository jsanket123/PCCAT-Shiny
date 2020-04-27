###############################################################
#   PCCAT: Principle Component and Clustering Analysis Tool   #
###############################################################

# Server Side (server.R)

suppressMessages(library(scatterD3))
suppressMessages(library(gridExtra))
suppressMessages(library(ggplot2))
suppressMessages(library(ggbiplot))
suppressMessages(library(plotly))
suppressMessages(library(stringr))
suppressMessages(library(rgl))
suppressMessages(library(dendextend))
suppressMessages(library(summarytools))
suppressMessages(library(patchwork))

options(shiny.maxRequestSize=30*1024^2)  ##set file to 30MB

default_lines <- data.frame(
  slope = c(0, Inf),
  intercept = c(0, 0),
  stroke = "#000",
  stroke_width = 1,
  stroke_dasharray = c(5, 5)
)


shinyServer(function(input, output, session){
  
  output$style_tag <- renderUI({
    if(input$sidebar=='intro')
      {return(tags$head(tags$style(HTML('.content-wrapper {background-image:url(Great_Lakes.jpg);background-repeat: no-repeat;
      background-size: 100% 100%;}'))))}
    else
      {return(tags$head(tags$style(HTML('.content-wrapper {background-image:none;}'))))}
  })
  
  inFile <- reactive({
    if (is.null(input$file)) 
    {return(NULL)} 
    else 
    {input$file}
  })
  
  myData <- reactive({
    if (is.null(inFile())) 
    {return(NULL)} 
    else 
    {read.csv(inFile()$datapath)}
  })
  
  output$text2 <- renderText({
    if(!is.null(myData()))
    { 
      return("Please make sure the categorical variables are at the beginning followed by all the numerical variables.")
    }
  })
  
  observe({updateSelectInput(session,"color",choices=c('None',names(myData())), selected="None")})
  observe({updateSelectInput(session,"size",choices=c('None',names(myData())), selected="None")})
  observe({updateSelectInput(session,"var",choices=c(names(myData())), selected="")})
  observe({updateSelectInput(session,"color3D",choices=c('None',names(myData())), selected="None")})
  observe({updateSelectInput(session,"size3D",choices=c('None',names(myData())), selected="None")}) 
  
  observeEvent(
    input$goButton,{shinyalert("Check results in data analysis tab!",type="success")}
  )
  
  useData <- reactive({
    input$goButton
    isolate({
      if(!is.null(myData()))
      {
        nams <- names(mat <- myData())
        dat <- mat[, input$start:ncol(mat)]
        p <- ncol(dat)
        if(input$log) dat <- log(dat)
        if(input$std) dat <- scale(dat)
        pca <- prcomp(dat, center=FALSE, scale=FALSE)
        list(mat=mat, dat=dat, pca=pca)
      }
    })
  })
  
  #============== Data Exploration & Summary
  output$sum_plot <- renderPlot({
    input$goButton
    if(input$var!="")
    { 
      dat <- useData()$dat
      mat <- useData()$mat
      if(input$var %in% colnames(dat))
      {
      hist_plot <- ggplot(mat, aes(x=mat[,input$var])) + 
        geom_histogram(fill="steelblue",bins = 100, alpha=0.5, position="identity") +
        theme_bw(base_size = 20) + xlab(input$var)+theme( legend.position = "none")
      box_plot <- ggplot(mat, aes(x="",y=mat[,input$var])) +
        geom_boxplot(fill = "steelblue", color = "black") + 
        coord_flip() +  
        theme_bw(base_size = 20) + xlab("")  + ylab(input$var)
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
      hist_plot + box_plot + plot_layout(nrow = 2, heights = c(2, 1))
      }
      else
      {
        bar_plot <- ggplot(mat, aes(x=mat[,input$var])) +
          geom_bar(width=0.5, fill="steelblue") +
          theme_bw(base_size = 20) + xlab(input$var)+theme( legend.position = "none") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        bar_plot
      }
    }
  })
  output$sum <- renderPrint({
    input$goButton
    if(input$var!="")
    { 
      dat <- useData()$dat
      mat <- useData()$mat
      if(input$var %in% colnames(dat))
      {
        summary(mat[,input$var])
      }
      else
      {
        table(mat[,input$var])
      }
    }
  })
  
  #==============  Variance Plot for PCA
  output$pca_plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData()))
    { 
      isolate({
        pca <- useData()$pca; 
        p <- ncol(pca$x)

        percent1 <- 100*(pca$sdev^2)/sum(pca$sdev^2)
        percent2 <- 100*cumsum(pca$sdev^2)/sum(pca$sdev^2)
        
        percent2 <- percent2[(1:length(percent2))[percent2 <= 99]]
        percent1 <- percent1[1:length(percent2)]
        Var <- pca$sdev^2
        
        perc_data <- data.frame(percent1=percent1,percent2=percent2, PC=paste("PC",1:length(percent2),sep = ''), Var=Var[1:length(percent2)])
        
        Var_Plot <- ggplot(perc_data, aes(x=reorder(PC, -Var), y=Var)) +
          geom_bar(stat="identity") +
          ggtitle("Variances of Principle Components")+ 
          xlab("Principal Component") + ylab("Explained Variance") +
          geom_text(aes(label= paste(round(percent1,1),"% (",round(percent2, 1),"%)")), size=5, vjust=-.5) +
          ylim(0, pca$sdev[1]^2*1.2) + 
          theme(text = element_text(size=15),plot.title = element_text(hjust = 0.5))
        
        Var_Plot
      })
    }
  })
  
  output$text1 <- renderText({
    input$goButton
    if(!is.null(useData()))
    { 
      "Percentages on top of each bar represent the percentage of variance explained by that principle component and the cumulative percentage in the parentheses"
    }  
  })
  
  #==============  Interactive 2D Scatter Plot for PCA
  lines <- reactive({
    default_lines
  })
  
  output$pca_plot2 <- renderScatterD3({
    input$goButton
    if(!is.null(useData()))
    { 
      pca <- useData()$pca;
      p <- ncol(pca$x)
      mat <- useData()$mat; 
      mat$x <- pca$x[,1];
      mat$y <- pca$x[,2]; 
      mat$lab <- row.names(mat)
      mat$foo <- rep(1, nrow(mat))
      
      col_var <- if (input$color == "None") NULL else mat[,input$color]
      size_var <- if (input$size == "None") NULL else mat[,input$size]
      symbol_var <- NULL
      
      scatterD3(
        x = mat$x,
        y = mat$y,
        xlab='PC1',
        ylab='PC2',
        lab = mat$lab,
        col_var = col_var,
        col_lab = input$color,
        ellipses = input$ellipses,
        size_var = size_var,
        size_lab = input$size,
        symbol_var=symbol_var,
        symbol_lab = input$symbol,
        lines = lines(),
        lasso = TRUE,
        lasso_callback = "function(sel) {prompt('Copy to clipboard: Ctrl+C, Enter', sel.data().map(function(d) {return d.lab}).join(','));}",
        point_size=150,
        point_opacity = input$scatterD3_opacity,
        labels_size = input$scatterD3_labsize,
        hover_size = 3,
        axes_font_size = "150%",
        legend_font_size = "150%",
        transitions = TRUE
      )
    }
  })
  
  #============== Interactive 3D Scatter Plot for PCA
  
  output$pca_plot4 <- renderPlotly({
    input$goButton
    if(!is.null(useData()))
    { 
      pca <- useData()$pca; 
      p <- ncol(pca$x)
      
      mat <- useData()$mat; 
      mat$lab <- mat[,1]##suppose the 1st column has obs names
      
      col_3D <- if (input$color3D == "None") "None" else mat[,input$color3D]
      size_3D <- if (input$size3D == "None") 0.2 else mat[,input$size3D]
      
      ply <- plot_ly(x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3] ,color=~col_3D,showscale = TRUE,size = ~as.numeric(size_3D),sizes=c(10,20)) %>%
        add_markers(marker = list(symbol = 'circle', sizemode = 'diameter',opacity = input$opacity_3D,
                                  showlegend = T),
                    text = ~paste('Obsevation:', mat$lab),
                    hovertemplate = paste(
                      "<b>%{text}</b><br>",
                      "PC1: %{x}<br>",
                      "PC2: %{y}<br>",
                      "PC3: %{z}",
                      "<extra></extra>"
                    )) %>%
        layout(scene = list(xaxis = list(title = paste0("PC1 ","(",summary(pca)$importance[2,1]*100,"%)"),range = c(floor(min(pca$x[,1],pca$x[,2],pca$x[,3])),ceiling(max(pca$x[,1],pca$x[,2],pca$x[,3]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),
                            yaxis = list(title = paste0("PC2 ","(",summary(pca)$importance[2,2]*100,"%)"),range = c(floor(min(pca$x[,1],pca$x[,2],pca$x[,3])),ceiling(max(pca$x[,1],pca$x[,2],pca$x[,3]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),
                            zaxis = list(title = paste0("PC3 ","(",summary(pca)$importance[2,3]*100,"%)"),range = c(floor(min(pca$x[,1],pca$x[,2],pca$x[,3])),ceiling(max(pca$x[,1],pca$x[,2],pca$x[,3]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),camera = list(eye = list(x = -1.0, y = 1.25, z = 1.25),aspectmode = "manual",aspectratio = list(x=1, y=0.5, z=0.5))),
               paper_bgcolor = 'rgb(255, 255, 255)',
               plot_bgcolor = 'rgb(169, 236, 253)')
      
      ply
    }
  })

  #==============  Clustering
  output$cl_plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData())){ 
      
      dat <- useData()$dat
      
      require(cluster)
      cl <- pam(x=dat, k=input$k1, diss=FALSE, metric='euclidean')
      
      par(mfrow=c(1,2), mar=c(2,3,2,0)+.4,mgp=c(1.3,.3,0), tck=-0.02, cex.axis=1.3, 
          cex.lab=1.3, cex.main=1.3)
      plot(cl, which=1, main="Partitional Clustering Scatter Plot") #check ?plot.partition for interpretation and more options
      
    }
  })
  
  # Dendrogram
  output$cl_plot2 <- renderPlot({
    input$goButton
    if(!is.null(useData())){
      mat <- useData()$mat
      dat <- useData()$dat
      hc <- hclust(dist(dat))
      plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "", ylab= "", sub = "", labels = mat[,input$k3])
      rect.hclust(hc, k = input$k2, border = 1:input$k2)
    }
  })
  
  # Cluster Table
  output$cl_table <- renderTable({
    input$goButton
    if(!is.null(useData())){
      mat <- useData()$mat
      dat <- useData()$dat
      hc <- hclust(dist(dat))
      mat$clust <- cutree(hc, k = input$k2)
      subset(mat, clust == input$k4)
    }
  })
  
}) 