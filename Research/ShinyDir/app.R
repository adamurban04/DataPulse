library(shiny)

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
  
)


server <- function(input, output, session) {
  
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets") #the input will be "dataset"
  })
  
  # <- defining the recipe for output with id: "summary"
  output$summary <- renderPrint({ #renderX are functions that produce a particular type of output
    summary(dataset())  #summary of the dataset
  })
    
  # <- defining the recipe for output with id: "table"
  output$table <- renderTable({
    dataset() #shows the dataset in a table
  })
  
}








shinyApp(ui, server)