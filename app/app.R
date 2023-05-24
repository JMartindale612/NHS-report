library(shiny)
library(tidyverse)

ui <- fluidPage(
  title = "TEST_APP",
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')
      )
    ),
    mainPanel(
      downloadButton("btn", "Generate Reports") #,
      # DT::dataTableOutput('contents',ir width=750) # show the data (if useful)
    )
  )
)


server <- function(input, output, session) {

  inputData<-reactive({
    if (is.null(input$file1))
      return(NULL)                
    read.csv(input$file1$datapath)
  })
  
  
  cleandata <- reactive({
    if (is.null(inputData()))
      return(NULL) 
    inputData() |> 
      mutate(across(where(is.numeric), ~round(scale(.),3)))
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(cleandata(), options = list(scrollX = TRUE))     
  })
  
  
  output$btn <- downloadHandler(
    filename = function() {paste0("reports", ".zip")},
    content = function(file) {
      
      rep_env <- new.env()
      assign("thedata",cleandata(),envir = rep_env)
      
      fs <- c()
      
      for(i in 1:nrow(cleandata())){
        path = paste0("testreport_",i,".html")
        rmarkdown::render("testreport.Rmd", output_format = "html_document", 
                          output_file = path,
                          params = list(rowid = i), 
                          envir = rep_env,
                          clean=F,
                          encoding="utf-8"
        )
        fs <- c(fs, path)
      }
      zip(file, fs)
    },
    contentType = "application/zip"
  )
  
  # delete all HTML files in wd() at end of session
  session$onSessionEnded(function() {
    system(paste0("rm -f *.html"))
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
