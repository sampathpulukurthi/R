require(shiny)
require(ggplot2)
require(googleVis)
#require(RCurl)

shinyServer(
  
  function(input,output){
    
    ## Reading data from local machine
    datasetInput <- reactive({
      Prediction_data(nfl.data,input$sub.season)
    })
    
    ## Render local data output to shiny
    output$table1 <- renderGvis({
      
      gvisTable(datasetInput()[[1]],options=list(page='enable',width=800, height=550,
                                                 cssClassNames = "{headerRow: 'myTableHeadrow', tableRow: 'myTablerow'}", 
                                                 alternatingRowStyle = FALSE), chartid = "mytable")
      #print(as.numeric(nrow(datasetInput()[[1]])))
      
      
    })
    
    output$chart <- renderPlot({
      
        ggplot(datasetInput()[[1]],aes_string(x=input$univariate))+geom_bar(fill = "#98AFC7")+theme_bw()
      
    })
    
    output$chart2 <- renderPlot({
      
      p <- ggplot(datasetInput()[[1]],aes_string(x=input$univariate,y = input$bivariate,fill = input$univariate))+
        geom_bar(stat = "identity")+
        #scale_y_continuous(labels = c("0M","50M","100M","150M"))+
        ggtitle("Total Sales by Season")+ theme_bw()
      
      p + theme(axis.text.x=element_text(angle=90,hjust=1))
    })
    
    output$chart3 <- renderPlot({
      
#       ggplot(nfl.data,aes_string(x=input$univariate,y=input$bivariate,group=input$multivariate,fill=input$multivariate))+
#         geom_bar(stat="identity",position="dodge")+ 
#         scale_y_continuous(labels = c("0M","10M","20M","30M"))+
#         scale_x_discrete(limits=unique(nfl.data$Week))+theme_bw()
#       
      p <- ggplot(datasetInput()[[1]], aes_string(x=input$univariate,y=input$bivariate,group=input$multivariate,color = input$multivariate)) + 
        geom_line(size=0.8) + geom_point(size=3)+
        scale_y_continuous(labels = c("0M","10M","20M","30M"))+
        scale_x_discrete(limits=unique(datasetInput()[[1]]$Week))+ggtitle("Sales Trend by Season")+ theme_bw()
      p + theme(axis.text.x=element_text(angle=90,hjust=1))
      
    })
  
    
    ## Render model accuracy as a gauge chart
    output$view2 <- renderGvis({
      
      base_accuracy = round((100 - 5.456),2)
      df2 <- data.frame(Label = "Accuracy(%)", Value = base_accuracy)
      
      gvisGauge(df2,
                options=list(title="Base Accuracy",min=0, max=100, greenFrom=90,
                             greenTo=100, yellowFrom=75, yellowTo=89.99,
                             redFrom=0, redTo=74.99, width=300, height=300));  
      
    })
    
    
    ## Creating download button to download data in csv format
#     output$downloadData <- downloadHandler(
#       
#       
#       filename = function() {
#         paste("filename", input$filetype, sep = ".")
#       },
#       
#       
#       content = function(file) {
#         
#         sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
#         
#         
#         write.table(datasetInput()[[1]], file, sep = sep,
#                     row.names = FALSE)
#         
#       }
#       
#     )
#     
    
    
    
  }
)
