# Load R packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr) # Data Manipulation (%>%)

# Read the txt file for Report
text_content <- readLines("www/Report.txt")
text_content <- paste(text_content, collapse = "\n")

# Read the TSV file for Dataset

luad_data <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")


# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Navigation
                navbarPage(
                  "UPSTaRT",
                  
                  # Tab: DATASET
                  tabPanel("DATASET",
                           mainPanel(
                             h1("LUAD Dataset"),
                             h4("head(dataset)"),
                             tableOutput("head"),
                             h4("summary(dataset)"),
                             verbatimTextOutput("summary")
                           )
                  ),
                  
                  # Tab: PLOTS
                  tabPanel("PLOTS",
                           sidebarPanel(
                             
                             radioButtons(inputId = "plot_choice", label = "Choose Plot:", 
                                         choices = list("Diagnosis Age" = "age", 
                                                        "Sex" = "sex")),
                           ),
                           mainPanel(
                             plotOutput(outputId = "dynamic_plot")
                           )
                           
                  ),
                  
                  # Tab: REPORT
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
  
# Tab: DATASET
  
  # Output for head of the dataset
  output$head <- renderTable({
    head(luad_data)
  })
  
  # Output for summary of the dataset
  output$summary <- renderPrint({
    summary(luad_data)
  })
  
  
# Tab: PLOTS
  
  
  
  output$dynamic_plot <- renderPlot({ # prints a plot based on the input
    
    plot <- switch(input$plot_choice,
                   
                   "age" = {
                     
                     # Filter out NA values for Age
                     filtered_age <- luad_data %>%
                       filter(!is.na(`Diagnosis Age`)) # using `` backticks to reference column name with spaces
                     
                     # Count Valid Samples in Age
                     total_samples <- nrow(filtered_age)
                     
                     # Count the Average Diagnosis Age
                     average_age <- mean(filtered_age$`Diagnosis Age`)
                     average_age <- round(average_age)
                     
                     # Create the plot
                     filtered_age %>%
                       ggplot(aes(x = `Diagnosis Age`)) +
                       geom_bar() +
                       labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count") +
                       geom_text(aes(label = paste("Total samples:", total_samples)), 
                                 x = Inf, y = Inf, hjust = 1.2, vjust = 1.9) +
                       geom_text(aes(label = paste("Average Diagnosis Age:", average_age)), 
                                 x = Inf, y = Inf, hjust = 3.75, vjust = 2)
                   },
                   
                   "sex" = {
                     
                     # Filter out NA values for Sex
                     filtered_sex <- luad_data %>%
                       filter(!is.na(Sex))
                     
                     # Count Valid Samples in Sex
                     total_samples <- nrow(filtered_sex)
                     
                     # Male/Female Count
                     sex_counts <- filtered_sex %>%
                       count(Sex) # (Sex column in the filtered_sex dataframe)
                     
                     # Create the pie chart
                     ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Male/Female Diagnosed Ratio") +
                       scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       geom_text(aes(x = Inf, y = Inf, label = paste("Total samples:", total_samples)), vjust = -1.1, hjust = 0.6)
                   
                   })
    
    print(plot)
  })
  

  
  
  
# Tab: REPORT  
  
  # Output for the report text file
  
  output$research_report <- renderText({
    text_content
  })
  
  
}





# Create Shiny object
shinyApp(ui = ui, server = server)
