library(tidyverse)

# Define UI for app
ui <- fluidPage(
  # File upload input
  fileInput("file", "Choose CSV file"),
  
  # Generate report button
  actionButton("generate", "Generate report")
)

# Define server for app
server <- function(input, output) {
  # Load data from uploaded file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Generate report when button is clicked
  observeEvent(input$generate, {
    # Render Rmarkdown file to HTML
    render("report.Rmd", output_file = "report.html",
           output_dir = getwd(),
           params = list(data = data()))
  })
}

# Run app
shinyApp(ui, server)