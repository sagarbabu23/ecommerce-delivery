library(caret)
library(doParallel)
library(randomForest)
library(xlsx)
library(lubridate)
library(C50)
library(gbm)
library(verification)



library(shiny)
library(shinydashboard)
#install.packages("profvis")
#library(profvis)



ui <-  dashboardPage(
  
  dashboardHeader(title = " analysis of retail"),
  dashboardSidebar(
    
    
    
    fileInput("file1", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    tags$hr(),
    sidebarMenu(
      menuItem("randomforest", tabName = "rf"),
      menuItem("c.50", tabName = "c50"),
      menuItem("gradient boost", tabName = "GB"))
  ),
  
  
  dashboardBody( 
    tabItems(
      tabItem(tabName = "rf",
              fluidRow(
                #sidebarLayout(
                #sidebarPanel(),
                mainPanel(
                  tabsetPanel(type = "tab",
                              tabPanel("Main data",tableOutput("data")),
                              tabPanel("summary",tableOutput("summary")),
                              tabPanel( "predict",tableOutput("predict")),
                              tabPanel( "accuracy",tableOutput("accuracy"))
                  )
                )
              )
      ),  
      
      
      tabItem(tabName = "c50",
              fluidRow(
                #sidebarLayout(
                #sidebarPanel(),
                mainPanel(
                  tabsetPanel(type = "tab",
                              #tabPanel("datatable",tableOutput("data50")),
                              # tabPanel("c50_summary",textOutput("c50_summary")),
                              tabPanel( "c50_predict",tableOutput("c50_predict")),
                              tabPanel( "c50_accuracy",tableOutput("c50_accuracy"))
                  )
                )
              )
      ),
      
      
      tabItem(tabName = "GB",
              fluidRow(
                # sidebarLayout(
                #sidebarPanel(
                
                # p("Select the inputs from below"),
                #sliderInput(inputId = "interaction.depth", label = "interaction depth in xgb", value = 20, min = 0, max = 100),
                #sliderInput(inputId = "n.trees", label = "number of tree", value = 200, min = 100, max = 1500)
                #),
                mainPanel(
                  tabsetPanel(type = "tab",
                              #tabPanel("datatable",tableOutput("data50")),
                              # tabPanel("c50_summary",textOutput("c50_summary")),
                              tabPanel( "gbm_predict",tableOutput("gbm_predict")),
                              tabPanel( "gbm_accuracy",tableOutput("gbm_accuracy"))
                  )
                )
              )
      )
      
      
      
      
      
      
      
    ) ))





server <- function(input, output) {
  
  
  maindata <- reactive({                             ### maindata() is function now
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)                                       ### input should be  defined with null in reactive.
    
    read.csv(inFile$datapath, header = input$header) ## last line is always treted as output in r shiny
    
    
  })
  
  output$data <-renderTable(maindata())
  
  output$summary <-  renderTable({ summary(maindata())})
  
  
  models <- reactive({
    
    
    #set.seed(15788)
    sheet <- maindata()    ## for data we can write seperate function and call that function
    
    attach(sheet)
    payment <-  model.matrix(~ fpaymentmode-1,sheet) 
    sheet[,'shipment_transit'] <-as.numeric( difftime(shipment_date,order_date)) ## or sheet[,'shipment_transit'] <- as.Date(shipment_date,"%y/%m/%d") - as.Date(order_date,"%y/%m/%d")
    sheet[,'delivery_transit'] <- as.numeric(difftime(expected_delivery,shipment_date))## or sheet[,'delivery_transit'] <- as.Date(expected_delivery,"%y/%m/%d")-as.Date(shipment_date,"%y/%m/%d")
    sheet <-cbind(sheet,payment)
    sheet <- sheet[,-c(12:14,3)]
    
    
    sheet_rf <- randomForest(fdeliver_status ~ .,data = data.frame(sheet),importance= T,ntree=100,mtry=1)
    predict1 <- predict(sheet_rf,newdata = data.frame(sheet)) ### when u refer data u shuld define as data.freame as r shiny gives only attributes
    
    output$predict <- renderTable({ predict1})#### output can be created anywhere or end of the function
    
    
    table(predict1,factor(sheet$fdeliver_status))           ### while specifying one variable u should always define that variabe as factor/ numeric
    values <- mean(predict1==factor(sheet$fdeliver_status)) ### atleast one output render is called out from main code in server and remaining can be definde inside.
    
    output$accuracy <- renderTable({mean(predict1==factor(sheet$fdeliver_status))})  
    
    
    ##### C.50  data #####
    
    #set.seed(7676)
    
    sheet_c50 <- C5.0(fdeliver_status ~ .,data = data.frame(sheet))
    
    predict.C5.0 <- predict(sheet_c50,newdata = data.frame(sheet))
    
    output$c50_predict <- renderTable({predict(sheet_c50,newdata = data.frame(sheet))})
    
    
    table(predict.C5.0 ,factor(sheet$fdeliver_status))
    mean(predict.C5.0 ==factor(sheet$fdeliver_status))
    
  })
  
  output$c50_accuracy <- renderTable({models()}) 
  
  
  #########gradient boost #########     
  Gradient <- reactive({
    
    
    set.seed(5678)
    
    sheet_gbm<- maindata()
    sheet_gbm <-  sapply(sheet,as.numeric)
    
    View(sheet_gbm)
    sheet_gbm[,"fdeliver_status"] <- sheet_gbm[,"fdeliver_status"]-1
    sheet_gbm <- data.frame(sheet_gbm)
    
    model_gbm <- gbm(fdeliver_status ~ .,data = data.frame(sheet_gbm),distribution = "bernoulli",interaction.depth = 20, n.trees =250)
    
    
    pred_gbm <- predict(model_gbm,newdata = data.frame(sheet_gbm),n.trees=250)
    
    output$gbm_predict <- renderTable(predict(model_gbm,newdata = data.frame(sheet_gbm),n.trees=250))
    
    pred_gbm <- ifelse(pred_gbm>.5,1,0)
    gbmtable <- table(factor(sheet_gbm$fdeliver_status ),pred_gbm)
    
    
    #accuracy
    # output$gbm_accuracy <- renderTable({ sum(diag(gbmtable))/sum(gbmtable)})
    accuracy_gbm <-  sum(diag(gbmtable))/sum(gbmtable)
  })
  
  output$gbm_accuracy <- renderTable(Gradient())
  
  
  
  
  
  
  
}

shinyApp(ui, server)


#profvis(runApp())
