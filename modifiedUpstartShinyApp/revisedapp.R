# Load R packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2) 
library(dplyr) # for DataTables
library(DT)    # for adding searchability to said Datatables


# Note Path files of txt files may need to be adjusted depending on directory and OS used

# Reads the txt file for Report
text_content <- readLines("C:\\Users\\jamco\\Downloads\\test\\Report.txt")
text_content <- paste(text_content, collapse = "\n")

# Read the TSV file for Dataset
luad_data <- read_tsv("C:/Users/jamco/Downloads/test/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Custom CSS to style the text box and additional table functionality
                
                tags$head(tags$style(HTML("
                  .large-text-box {
                    width: 160%;
                    height: 600px;
                    overflow-y: scroll;
                    white-space: pre-wrap;
                    word-wrap: break-word;
                    border: 1px solid #ccc;
                    padding: 10px;
                    background-color: #f9f9f9;
                  }
                  .scrollable-table {
                    overflow-x: auto;
                    white-space: nowrap;
                     width: 150%;
                  }
                  .scrollable-table table {
                    width: auto;
                  }
                  .summary-text-box {
                  width: 150%;
                  height: 50%; 
                  overflow-y: scroll;
                  white-space: pre-wrap;
                  word-wrap: break-word;
                  border: 1px solid #ccc;
                  padding: 10px;
                  background-color: #f9f9f9;
  }
                "))),
                
                # Navigation
                navbarPage(
                  "UPSTaRT",
                  
                  # Tab: DATASET
                  tabPanel("DATASET",
                           mainPanel(
                             h1("LUAD Dataset"),
                             h4("Head of Dataset"),
                             div(class = "scrollable-table", DTOutput("head")),
                             h4("Summary of Dataset"),
                             div(class = "summary-text-box", verbatimTextOutput("summary"))
                           )
                  ),
                  
                  # Tab: PLOTS
                  tabPanel("PLOTS",
                           sidebarPanel(
                             radioButtons(inputId = "plot_choice", label = "Choose Plot:",
                                          choices = list("Age" = "age",
                                                         "Sex" = "sex",
                                                         "Race" = "race",
                                                         "Survival Status" = "survival_status",
                                                         "Mutation Count" = "mutation_count"))
                           ),
                           mainPanel(
                             plotOutput(outputId = "dynamic_plot")
                           )
                  ),
                  
                  # Tab: REPORT
                  tabPanel("REPORT",
                           mainPanel(
                             h3("Research: Lung adenocarcinoma (LUAD)"),
                             h4(pre(textOutput("research_report"), class = "large-text-box"))
                           )
                  )
                )
)

# Define server function  
server <- function(input, output) {
  
  # Tab: DATASET
  
  # Output for head of the dataset
  output$head <- renderDataTable({
    datatable(luad_data, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
    # -1 is chosen as the final option as it will output the entire table regardess of dataset size
  })
  
  # Output for summary of the dataset
  output$summary <- renderPrint({
    summary(luad_data)
  })
  
  # Tab: PLOTS
  output$dynamic_plot <- renderPlot({ # prints a plot based on the input
    
    plot <- switch(input$plot_choice,
                   
                   # Bar Chart, Pie Chart, Density Plot
                   
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
                       annotate("text", label = paste("Total samples:", total_samples), 
                                x = Inf, y = Inf, hjust = 1.2, vjust = 1.9) +
                       annotate("text", label = paste("Average Diagnosis Age:", average_age), 
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
                       geom_text(aes(label =
                                       n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Total samples:", total_samples), hjust = 0.5, vjust = -1)
                   },
                   
                   "race" = {
                     # Filter out NA values for Race
                     filtered_race <- luad_data %>%
                       filter(!is.na(`Race Category`))
                     
                     # Count Valid Samples in Race
                     total_samples <- nrow(filtered_race)
                     
                     # Race Category Count
                     race_counts <- filtered_race %>%
                       count(`Race Category`) # (Race Category column in the filtered_race dataframe)
                     
                     # Create the pie chart
                     ggplot(race_counts, aes(x = "", y = n, fill = `Race Category`)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Race Category Diagnosed Ratio") +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Total samples:", total_samples), hjust = 0.5, vjust = -1)
                   },
                   
                   "survival_status" = {
                     # Filter out NA values for Survival Status
                     filtered_survival_status <- luad_data %>%
                       filter(!is.na(`Overall Survival Status`))
                     
                     # Count Valid Samples in Survival Status
                     total_samples <- nrow(filtered_survival_status)
                     
                     # Survival Status Count
                     survival_status_counts <- filtered_survival_status %>%
                       count(`Overall Survival Status`) # (Overall Survival Status column in the filtered_survival_status dataframe)
                     
                     # Create the pie chart
                     ggplot(survival_status_counts, aes(x = "", y = n, fill = `Overall Survival Status`)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Survival Status") +
                       scale_fill_manual(values = c("#5CB85C", "#D9534F")) +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Total samples:", total_samples), hjust = 0.5, vjust = -1)
                   },
                   
                   "mutation_count" = {
                     # Filter out NA values for Mutation Count
                     filtered_mutation_count <- luad_data %>%
                       filter(!is.na(`Mutation Count`))
                     
                     # Count Valid Samples in Mutation Count
                     total_samples <- nrow(filtered_mutation_count)
                     
                     # Create the plot
                     filtered_mutation_count %>%
                       ggplot(aes(x = `Mutation Count`)) +
                       geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.8) +
                       labs(title = "Density of Mutation Count", x = "Mutation Count", y = "Density")
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
