#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("slate"),
                
                # App title ----
                titlePanel("Uploading Files"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    # Input: Select quotes ----
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head")
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    tableOutput("contents")
                    
                  )
                  
                ),
                
                # ************* Begin Data Analysis Section UI **************
                # App title ----
                titlePanel("Data Transformation App"),
                sidebarLayout(
                  sidebarPanel(
                    actionButton("run", "Process Data")
                  ),
                  mainPanel(
                    plotOutput("plotVWC")
                  )
                ),
                
                # ***********  Begin Download Section UI *********************
                # App title ----
                titlePanel("Downloading Data"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Choose dataset ----
                    selectInput("dataset", "Choose a dataset:",
                                choices = c("Processed Data")),
                    
                    # Button
                    downloadButton("downloadData", "Download")
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    tableOutput("table")
                    
                  )
                  
                )
)



# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  # ************* Begin Data Analysis Definitions ***************************
  #  data_upload <- reactive({
  #    mtcars
  #  })
  
  #  output$download <- downloadHandler(
  #    filename = function() {
  #      paste("data-processed-", Sys.Date(), ".csv", sep = "")
  #    },
  #    content = function(con) {
  #      req(data_upload())
  #      
  #      dat <- data_upload()
  
  observeEvent(input$run, {
    source("runData.R", local = TRUE)
  })  
  
  #      write.csv(dat, con)
  
  output$plotVWC <- renderPlot({
    VWC
  }) %>% bindEvent(input$button)
  
  # output$plot <- renderPlot({
  #   EC
  # })
  
  #  )
  
  # ************* Begin Download  Definitions *******************************
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Processed Data" = df3)
  }) %>% bindEvent(input$button)
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }) 
}

# Create Shiny app ----
shinyApp(ui, server)