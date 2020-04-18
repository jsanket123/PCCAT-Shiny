#########
# PCCAT #
#########

library(scatterD3)
library(gridExtra)
library(ggplot2)
library(ggbiplot)
library(plotly)
library(stringr)
library(rgl)
library(dendextend)
library(summarytools)

options(shiny.maxRequestSize=30*1024^2)  ##set file to 30MB

default_lines <- data.frame(
  slope = c(0, Inf),
  intercept = c(0, 0),
  stroke = "#000",
  stroke_width = 1,
  stroke_dasharray = c(5, 5)
)


shinyServer(function(input, output, session){
  
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
  
  observe({updateSelectInput(session,"color",choices=c('None',names(myData())), selected="None")})
  observe({updateSelectInput(session,"size",choices=c('None',names(myData())), selected="None")})
  # observe({updateSelectInput(session,"symbol",choices=c('None',names(myData())), selected="None")})
  # observe({updateSelectInput(session,"opacity",choices=names(myData()), selected="")})
  observe({updateSelectInput(session,"color3D",choices=c('None',names(myData())), selected="None")})
  observe({updateSelectInput(session,"size3D",choices=c('None',names(myData())), selected="None")}) 
  
  observeEvent(input$goButton, {
    shinyalert("Check results in data analysis tab!",type="success")
  })
  
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
        pca <- prcomp(dat, center=FALSE, scale=FALSE) #Unlike princomp, variances are computed with the usual divisor N - 1.
        list(mat=mat, dat=dat, pca=pca)
      }
    })
  })
  
  #============== Data Summary
  output$sum_table <- renderUI({
    input$goButton
    if(!is.null(useData()))
    {
      dat <- useData()$dat
      print(dfSummary(dat), method = 'render', headings = FALSE, bootstrap.css = FALSE)
    }
  })
  
  #==============  Two regular plots for PCA
  output$pca_plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData()))
    { 
      isolate({
        pca <- useData()$pca; 
        p <- ncol(pca$x)
        #sumpca <- matrix(rep(0,3*p), nrow = 3)
        #sumpca[1,] <- pca$sdev^2
        #sumpca[2,] <- cumsum(pca$sdev^2)
        #sumpca[3,] <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
        
        
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
        # PC_Bi_Plot <- ggbiplot(pca,varname.size = 3)+
        #   ggtitle("PC1 vs PC2 Plot")+
        #   theme_minimal() + 
        #   theme(text = element_text(size=15),plot.title = element_text(hjust = 0.5),aspect.ratio = 1)
        # grid.arrange(Var_Plot, PC_Bi_Plot, ncol=2, widths=c(1,1))
        #
        # par(mfrow=c(1,2), mar=c(2,2,2,0)+.2,mgp=c(1.3,.3,0), tck=-0.02,
        #   cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
        # plot(pca, main='Variance of Principle Components', ylim = c(0,pca$sdev[1]^2*1.2))
        # with(pca,
        #    text(x = (1:p*1.1),
        #         y = pca$sdev^2,
        #         labels = paste(round(sumpca[3,]*100, 1),"%"),
        #         pos = 3, cex = 1.3))
        # biplot(pca, cex=1.3) #, scale=1
        
      })
    }
  })
  
  output$text1 <- renderText("Percentages on top of each bar represent the percentage of variance explained by that principle component and the cumulative percentage in the parentheses")
  
  lines <- reactive({
    # if (input$scatterD3_threshold_line) {
    #   return(rbind(default_lines, threshold_line))
    # }
    default_lines
  })
  
  #==============  Interactive 2D Scatter Plot for PCA
  output$pca_plot2 <- renderScatterD3({
    #input$goButton
    if(!is.null(useData()))
    { 
      
      
      
      pca <- useData()$pca;
      p <- ncol(pca$x)
      mat <- useData()$mat; #nams <- names(mat)
      mat$x <- pca$x[,1];
      mat$y <- pca$x[,2]; # you can add input to specify which 2 PC
      mat$lab <- row.names(mat)
      mat$foo <- rep(1, nrow(mat))
      
      col_var <- if (input$color == "None") NULL else mat[,input$color]
      size_var <- if (input$size == "None") NULL else mat[,input$size]
      # symbol_var <- if (input$symbol == "None") NULL else mat[,input$symbol]
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
               plot_bgcolor = 'rgb(169, 236, 253)')  #243, 243, 243
      
      ply
      
      
    }
  })
  
  
  
  
  ####output$pca_plot4 <- renderPlot({
  ###input$goButton
  ##if(!is.null(useData()))
  #{ 
  #isolate({
  # pca <- useData()$pca; 
  #        p <- ncol(pca$x)
  #        rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  #          if( new.device | rgl.cur() == 0 ) {
  #            rgl.open()
  #            par3d(windowRect = 50 + c( 0, 0, width, width ) )
  ##            rgl.bg(color = bg )
  #          }  #####function make sure rgl device open
  #          rgl.clear(type = c("shapes", "bboxdeco"))
  #          rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
  #        }#####function make sure rgl device open
  #        rgl_init()
  #        a<-rgl.spheres(pca$x[,1], pca$x[,2], pca$x[,3], r = 0.2, color = "yellow")  # Scatter plot
  
  #        a  
  
  #      })
  #    }
  #  })
  
  
  
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
      rect.hclust(hc, k = input$k2)
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


