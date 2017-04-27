#rm(list=ls())
#setwd("E:/Projects/Real_Estate/Application")

#Loading the library
require("shiny")

# Defining the UI 
shinyUI(fluidPage(
  
  tags$head(
    tags$style("body {background-color:  #4C3327; }")
  ),   
  
  tags$head(
    tags$style(".span8 {background-color:  #F2EFE4; }")
  ), 
  
  tags$head(
    tags$style(".well {background-color:  #F5DEB3; }")
  ), 
  
  # Header or Title Panel
  headerPanel(
    
    h1("NFL Analysis",align="center",
       style = "font-family: 'Verdana', cursive;
               font-weight: 100; line-height: 1.1; 
       color: #FFFFF0;"
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput('univariate', 'Attribute1',att_levels,selected="Season"),
      
      selectInput('bivariate', 'Attribute2', att_levels,selected="Sales"),
      
      selectInput('multivariate', 'Attribute3', att_levels,selected="Sales"),
      
      selectInput('sub.season', 'Season', season_levels,selected="All"),
      
      downloadButton('downloadData', 'Download'),
      br(),
      br(),
      
      actionButton("Run","Run Model"),width=4
    ),
    
    mainPanel(
      #htmlOutput(("chart"))
      tabsetPanel(
        
        #tabPanel("Forecast ",htmlOutput("chart"))
        
        tabPanel("View Data",htmlOutput("table1"),
                 tags$head(tags$style(type="text/css",
                                      ".myTableHeadrow {background-color:#4EB1BA;} .myTablerow {background-color:#B2CECF;}"         
                 ))     
                 
        ),
        
        tabPanel("Univariate ",plotOutput("chart")
                 
        ),
        
        tabPanel("Bivariate ",plotOutput("chart2")
                 
        ),
        
        tabPanel("Multivariate ",plotOutput("chart3")
                 
        ),
        
        
        
        tabPanel("Accuracy Gauge",
                 column(4,htmlOutput("view2")))
        
      )),
    
  )
)
)