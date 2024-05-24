# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "My first app",
                  tabPanel("Name combiner",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", "")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             h4("Output 1"),
                             verbatimTextOutput("txtout")
                           ) # mainPanel
                  ), # Navbar 1, tabPanel
                  tabPanel("Calculator",
                           sidebarPanel(
                             textInput("num1", "First Number", value = "0"),
                             textInput("num2", "Second Number", value = "0"),
                             selectInput("operation", "Operation", 
                                         choices = list("Add" = "add", 
                                                        "Subtract" = "subtract", 
                                                        "Multiply" = "multiply", 
                                                        "Divide" = "divide")),
                             actionButton("calculate", "Calculate")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             h4("Output 1"),
                             verbatimTextOutput("result")
                           ) # mainPanel
                  ), # tabPanel
                  tabPanel("Cancer",
                           sidebarPanel(
                             selectInput("info", "Cancer Info", 
                                         choices = list("Mortality Rate" = "Mrate", 
                                                        "Diagnosed in 2024" = "Drate", 
                                                        "Most Common Male Diagnoses" = "Mdiag", 
                                                        "Most Common Female Diagnoses" = "Fdiag")),
                           ),
                           
                           mainPanel(
                             plotOutput("plot")
                           ),
                           
                           
                           
                           
                  )
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    
    info <- input$info
    
    if (info=="Mrate"){
      rate = c(42, 51, 125, 53, 339)
      barplot(rate)
      axis(1, at=1:5, lab=c("Breast","Pancreas","Lung","Colon","Other"))
      title(ylab= "Deaths(1:100)", col.lab=rgb(0,0.5,0))
    }
    else if (info == "Drate"){
      rate= c(313, 299, 234, 152, 1000)
      barplot(rate)
      axis(1, at=1:5, lab=c("Breast","Prostate","Lung","Colon","Other"))
      title(ylab= "Diagnoses(1:100)", col.lab=rgb(0,0.5,0))
    }
    else if (info == "Mdiag"){
      rate= c(299, 63, 116, 81, 59)
      barplot(rate)
      axis(1, at=1:5, lab=c("Prostate","bladder","Lung","Colon","skin"))
      title(ylab= "Male Diagnoses(1:100)", col.lab=rgb(0,0.5,0))
    }
    else if (info == "Fdiag"){
      rate= c(310, 67, 118, 71, 41)
      barplot(rate)
      axis(1, at=1:5, lab=c("Breast","Uterus","Lung","Colon","Skin"))
      title(ylab= "Female Diagnoses(1:100)", col.lab=rgb(0,0.5,0))
    }
    else{
      rate=c(0,0,0,0,0)
      plot(rate)
      axis(1, at=1:5, lab=c("Breast","Prostate","Lung","Colon","Other"))
    }
    
    
  })
  
  
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
  })
  
  result <- eventReactive(input$calculate, {
    num1 <- as.numeric(input$num1)
    num2 <- as.numeric(input$num2)
    operation <- input$operation
    
    if (is.na(num1) | is.na(num2)) {
      return("Invalid input. Please enter numeric values.")
    }
    
    if (operation == "add") {
      result <- num1 + num2
    } 
    else if (operation == "subtract") {
      result <- num1 - num2
    } 
    else if (operation == "multiply") {
      result <- num1 * num2
    } 
    else if (operation == "divide") {
      if (num2 == 0) {
        result <- "Cannot divide by zero"
      } 
      else {
        result <- num1 / num2
      }
    } else {
      result <- "Error: Invalid operation"
    }
    
    result
  })
  
  output$result <- renderText({
    result()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)