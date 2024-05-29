# Load R packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)

# Read the txt file for Report
text_content <- readLines("www/Report.txt")
text_content <- paste(text_content, collapse = "\n")

# Read the TSV file for Dataset

data <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")


# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "UPSTaRT",
                  tabPanel("DATASET",
                           mainPanel(
                             h1("LUAD Dataset"),
                             h4("head(dataset)"),
                             tableOutput("head"),
                             h4("summary(dataset)"),
                             verbatimTextOutput("summary")
                           )
                  ),
                  tabPanel("PLOTS",
                           sidebarPanel(
                             
                             radioButtons("plot", "Choose Plot:", 
                                         choices = list("Age" = "age", 
                                                        "Gender" = "gender")),
                             
                           ),
                           mainPanel(
                             
                           )
                           
                  ),
                  tabPanel("REPORT",
            
                                    mainPanel(
                                      h3("Research: Lung adenocarcinoma (LUAD)"),
                                      h4(pre(textOutput("research_report")))                                      
                                    )
                                    
                           ),
),
)


# Define server function  
server <- function(input, output) {
  
  # Output for head of the file
  output$head <- renderTable({
    head(data)
  })
  
  # Output for summary of the file
  output$summary <- renderPrint({
    summary(data)
  })
  
  
  
  # Output for the report text file
  
  output$research_report <- renderText({
    text_content
  })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)