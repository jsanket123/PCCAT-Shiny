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
suppressMessages(library(factoextra))

options(shiny.maxRequestSize=30*1024^2)  ##set file to 30MB

default_lines <- data.frame(
  slope = c(0, Inf),
  intercept = c(0, 0),
  stroke = "#000",
  stroke_width = 1,
  stroke_dasharray = c(5, 5)
)

set.seed(as.integer(Sys.time()))

shinyServer(function(input, output, session){
  
#============== Data Input & Preprocessing
  
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
  
  output$csv_name1 <- renderText({
    input$goButton
    if (!is.null(inFile()$datapath))
    {return(basename(inFile()$name))}
    else 
    {return(NULL)}
  })
  output$csv_name2 <- renderText({
    input$goButton
    if (!is.null(inFile()$datapath))
    {return(basename(inFile()$name))}
    else 
    {return(NULL)}
  })
  output$csv_name3 <- renderText({
    input$goButton
    if (!is.null(inFile()$datapath))
    {return(basename(inFile()$name))}
    else 
    {return(NULL)}
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
      return("Please follow the rules mentioned in help section for the input data file format.")
    }
  })
  
  output$text3 <- renderUI({
    if(!is.null(myData()))
    {
      HTML(paste("Ex.1: In Iris dataset numerical features start from column-3.",
              "Ex.2: In Mtcars dataset numerical features start from column-4.",sep="<br/>"))
    }
  })

  output$text4 <- renderText({
    if(!is.null(myData()))
    { 
      return("Please refer to the help section for when to apply these data transformations.")
    }
  })
  
  observe({updateSelectInput(session,"var",choices=c(names(myData()[,-1])), selected="")})

  observe({updateSelectInput(session,"color",choices=c('None',names(myData()[,-1])), selected="None")})
  observe({updateSelectInput(session,"size",choices=c('None',names(myData()[,-c(1:(input$start-1))])), selected="None")})
  
  observe({updateSelectInput(session,"color3D",choices=c('None',names(myData()[,-1])), selected="None")})
  observe({updateSelectInput(session,"size3D",choices=c('None',names(myData()[,-c(1:(input$start-1))])), selected="None")}) 
  
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
        
        observe({updateSelectInput(session,"D2_pc_x",choices=as.character(1:p), selected="1")})
        observe({updateSelectInput(session,"D2_pc_y",choices=as.character(c(1:p)[-as.numeric(input$D2_pc_x)]), selected="2")})
        
        observe({updateSelectInput(session,"pc_sort",choices=as.character(1:p), selected="1")})
        
        observe({updateSelectInput(session,"D3_pc_x",choices=as.character(1:p), selected="1")})
        observe({updateSelectInput(session,"D3_pc_y",choices=as.character(c(1:p)[-as.numeric(input$D3_pc_x)]), selected="2")})
        observe({updateSelectInput(session,"D3_pc_z",choices=as.character(c(1:p)[!c(1:p) %in% c(as.numeric(input$D3_pc_x),as.numeric(input$D3_pc_y))]), selected="3")})
        
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
  
  #==============  Principal Component Analysis
  
  #==============  Tab-1: Variance Plot
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
  
  #==============  Tab-2: 2D Scatter Plot
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
      first <- as.numeric(input$D2_pc_x)
      second <- as.numeric(input$D2_pc_y)
      mat$x <- pca$x[,first];
      mat$y <- pca$x[,second]; 
      mat$lab <- row.names(mat)
      mat$foo <- rep(1, nrow(mat))
      
      col_var <- if (input$color == "None") NULL else mat[,input$color]
      size_var <- if (input$size == "None") NULL else mat[,input$size]
      symbol_var <- NULL
      
      scatterD3(
        x = mat$x,
        y = mat$y,
        xlab=paste("PC",first,sep=''),
        ylab=paste("PC",second,sep=''),
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
  
  #==============  Tab-3: 3D Scatter Plot
  output$pca_plot4 <- renderPlotly({
    input$goButton
    if(!is.null(useData()))
    { 
      pca <- useData()$pca; 
      p <- ncol(pca$x)
      
      mat <- useData()$mat; 
      mat$lab <- mat[,1]##suppose the 1st column has obs names
      
      i <- as.numeric(input$D3_pc_x)
      j <- as.numeric(input$D3_pc_y)
      k <- as.numeric(input$D3_pc_z)
      
      col_3D <- if (input$color3D == "None") "None" else mat[,input$color3D]
      size_3D <- if (input$size3D == "None") 0.2 else mat[,input$size3D]
      
      ply <- plot_ly(x = ~pca$x[,i], y = ~pca$x[,j], z = ~pca$x[,k] ,color=~col_3D,showscale = TRUE,size = ~as.numeric(size_3D),sizes=c(10,20)) %>%
        add_markers(marker = list(symbol = 'circle', sizemode = 'diameter',opacity = input$opacity_3D,
                                  showlegend = T),
                    text = ~paste('Observation:', mat$lab),
                    hovertemplate = paste(
                      "<b>%{text}</b><br>",
                      "PC",input$D3_pc_x,": %{x}<br>",
                      "PC",input$D3_pc_y,": %{y}<br>",
                      "PC",input$D3_pc_z,": %{z}",
                      "<extra></extra>"
                    ,sep = '')) %>%
        layout(scene = list(xaxis = list(title = paste0("PC",i,"(",round(summary(pca)$importance[2,i]*100,1),"%)",sep=''),range = c(floor(min(pca$x[,i],pca$x[,j],pca$x[,k])),ceiling(max(pca$x[,i],pca$x[,j],pca$x[,k]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),
                            yaxis = list(title = paste0("PC",j,"(",round(summary(pca)$importance[2,j]*100,1),"%)",sep=''),range = c(floor(min(pca$x[,i],pca$x[,j],pca$x[,k])),ceiling(max(pca$x[,i],pca$x[,j],pca$x[,k]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),
                            zaxis = list(title = paste0("PC",k,"(",round(summary(pca)$importance[2,k]*100,1),"%)",sep=''),range = c(floor(min(pca$x[,i],pca$x[,j],pca$x[,k])),ceiling(max(pca$x[,i],pca$x[,j],pca$x[,k]))),dtick = 1,gridwidth = 2, backgroundcolor='rgb(230, 230,230)',gridcolor='rgb(255, 255, 255)',zerolinecolor='rgb(255, 255, 255)',showbackground=TRUE),camera = list(eye = list(x = -1.0, y = 1.25, z = 1.25),aspectmode = "manual",aspectratio = list(x=1, y=0.5, z=0.5))),
               paper_bgcolor = 'rgb(255, 255, 255)',
               plot_bgcolor = 'rgb(169, 236, 253)')
      
      ply
    }
  })
  
  #==============  Tab-4: Variable Importance
  output$pca_table <- renderTable({
    input$goButton
    if(!is.null(useData()))
    {
        mat <- myData()
        nams <- names(mat)[input$start:ncol(mat)]
        pca <- useData()$pca
        p <- ncol(pca$x)
        loadpca <- pca$rotation
        Ind <- order(abs(loadpca[,as.numeric(input$pc_sort)]),decreasing = T)
        pcMat <- matrix(c(nams[Ind],round(loadpca[Ind,1:p],3)),ncol=p+1)
        namMat <- c('Variable', paste("PC",1:p,sep = ''))
        rbind(as.character(namMat), pcMat)
    }
  },include.colnames=FALSE,fixedHeader=TRUE,
  fixedColumns = list(leftColumns = 1, rightColumns = 0))
  
  
  #==============  Clustering Analysis
  
  #==============  Tab-1: Partitional Clustering
  output$cl_plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData())){ 
      
      dat <- useData()$dat
      
      require(cluster)
      cl <- pam(x=dat, k=input$k1, diss=FALSE, metric='euclidean')
      
      par(mfrow=c(1,2), mar=c(2,3,2,0)+.4,mgp=c(1.3,.3,0), tck=-0.02, cex.axis=1.3, 
          cex.lab=1.3, cex.main=1.3)
      #plot(cl, which=1, main="Partitional Clustering Scatter Plot") #check ?plot.partition for interpretation and more options
      fviz_cluster(cl,ellipse.type = "norm")+theme_bw()+
        ggtitle("Partitional Clustering Scatter Plot")+ 
        xlab("Component-1") + ylab("Component-2") +
        theme(axis.text.x = element_text(size = 15, color = "red"),text = element_text(size=15),plot.title = element_text(size = 18, color = "black",hjust = 0.5))
      
    }
  })
  
  #==============  Tab-2: Hierarchical Clustering 
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
  
  # Data table corresponding to each cluster
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