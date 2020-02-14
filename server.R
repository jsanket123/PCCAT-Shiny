#########
# PCCAT #
#########

library(scatterD3)
library(gridExtra)
library(ggplot2)
library(ggbiplot)
library(plotly)

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
        
        percent <- 100*cumsum(pca$sdev^2)/sum(pca$sdev^2)

        perc_data <- data.frame(percent=percent, PC=1:length(percent), Var = pca$sdev^2)
        
        Var_Plot <- ggplot(perc_data, aes(x=PC, y=Var)) +
          geom_bar(stat="identity") +
          ggtitle("Variance of Principle Components")+ 
          xlab("Principal Component") + ylab("Variance") +
          geom_text(aes(label= paste(round(percent, 1),"%")), size=5, vjust=-.5) +
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
  
  lines <- reactive({
    # if (input$scatterD3_threshold_line) {
    #   return(rbind(default_lines, threshold_line))
    # }
    default_lines
  })
  
  #==============  Interactive PCA
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
  
  
  #==============  Clustering
  output$cl_plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData())){ 
        
        dat <- useData()$dat
        
        require(cluster)
        cl <- pam(x=dat, k=input$k, diss=FALSE, metric='euclidean')

        par(mfrow=c(1,2), mar=c(2,3,2,0)+.4,mgp=c(1.3,.3,0), tck=-0.02, cex.axis=1.3, 
            cex.lab=1.3, cex.main=1.3)
        plot(cl, which=1, main="Partitioning Clustering Scatter Plot") #check ?plot.partition for interpretation and more options
        
    }
  })
  
  # Dendogram
  output$cl_plot2 <- renderPlot({
    input$goButton
    if(!is.null(useData())){
      isolate({
        dat <- useData()$dat
        hc <- hclust(dist(dat))
        plot(hc, main = "Heirarchical Clustering Model")
        rect.hclust(hc, k = input$k)
      })
    }
  })
  
  
  #------------------- useful chunks
  
  #  output$tab <- renderTable({
  #    input$goButton
  #    isolate({
  #     if(!is.null(useData())){
  #      mat <- useData()$mat
  #      inds <- which(mat$ME >= 0.15)
  #      mat[,-1] <- round(mat[,-1], 3) # the first column is group name
  #      if(length(inds)>0) mat[inds, 'ME'] <- paste('<div style="width: 100%; height: 100%; z-index: 0; background-color: rgba(255,0,0,.4); position:absolute; top: 0; left: 0; padding:5px;">
  #<span>', mat[inds, 'ME'], '</span></div>')
  #      names(mat) <- c('Group(s)','para1','para2','para3','Model error') 
  #      mat        
  #    }
  #    })
  #  }, digits=3, include.rownames=FALSE, sanitize.text.function=function(x){x}
  #    #autoWidth = TRUE,
  ##    columnDefs = list(list(width = '800px', targets = "_all"))
  #  )
  
  #  output$text1 <- renderUI({
  #   input$goButton
  #   isolate({
  #    if(!is.null(myData())){
  #      str1 <- paste('Note model error is the minimal relative error to pass the Chi-square goodness-of-fit test for model.')
  #      str2 <- paste('Usually for a good model it should be < 0.15.')
  #      HTML(paste(str1, str2, sep = '<br/>'))
  #    }
  #   })
  #  })
  
  #  output$downloadData <- downloadHandler(
  #      filename = function() { paste('Output.csv', sep='') },   #Sys.Date()
  #      content = function(files) {
  #        mat <- useData()$mat
  #        names(mat) <- c('Group(s)','para1','para2','para3','Model error') 
  #        write.csv(mat, files, row.names = FALSE)
  #      }
  #  )

  
}) 


