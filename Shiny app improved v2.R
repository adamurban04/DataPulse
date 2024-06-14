# Load R packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr) # Data Manipulation (%>%)
library(DT)    # for adding searchability to said Datatables
library(markdown)


# Read the TSV file for Dataset

luad_data <- read_tsv("C:/Users/jamco/Downloads/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")



# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                
                # Custom CSS to style the Research Report text box (cmnd+opt+L to fold)
                
                tags$head(tags$style(HTML("
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
                  
                  # Tab: CLINICAL DATA
                  # Changed from previous design of using a sidebar panel in order 
                  # to facilitate a neater presentation of the data
                  tabPanel("DATA",
                           fluidRow(
                             column(12, 
                                    selectInput("data_choice", "Choose Data Type:", 
                                                choices = c("Clinical Data" = "clinical", "Image Data" = "image")),
                                    conditionalPanel(
                                      condition = "input.data_choice == 'clinical'",
                                      h1("LUAD Clinical Data"),
                                      div(class = "scrollable-table", DTOutput("clinical_dsout")),
                                      h4("Summary of Clinical Data"),
                                      div(class = "summary-text-box", verbatimTextOutput("clinical_summary"))
                                    ),
                                    conditionalPanel(
                                      condition = "input.data_choice == 'image'",
                                      h1("Image Data"),
                                      div(class = "scrollable-table", DTOutput("image_dsout")),
                                      h4("Summary of Image Data"),
                                      div(class = "summary-text-box", verbatimTextOutput("image_summary"))
                                    )
                             )
                           )
                  ),
                  
                  # Tab: PLOTS
                  tabPanel("PLOTS",
                           sidebarPanel(
                             
                             radioButtons(inputId = "plot_choice", label = "Choose Plot:", 
                                          choices = list("Age" = "age", 
                                                         "Sex" = "sex",
                                                         "Race" = "race",
                                                         "Ethinicity" = "ethinicity"
                                          ))
                           ),
                           mainPanel(
                             plotOutput(outputId = "dynamic_plot")
                           )
                           
                  ),
                  
                  # Tab: REPORT
                  tabPanel("REPORT",
                           
                           mainPanel(
                             includeMarkdown("C:/Users/jamco/Downloads/Report.md")                                      
                           )
                  ),
                ),
)


# Define server function  
server <- function(input, output) {
  
  
  # Tab: DATASET
  
  # Output the clinical dataset
  output$clinical_dsout <- renderDataTable({
    datatable(luad_data, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })
  
  # Output for summary of the dataset
  output$summary <- renderPrint({
    summary(luad_data)
  })
  
  
  # Tab: PLOTS
  
  total_samples <- nrow(luad_data)
  
  # Reactive expression to filter data based on plot choice
  filtered_data <- reactive({
    switch(input$plot_choice,
           "age" = luad_data %>% filter(!is.na(`Diagnosis Age`)),
           "sex" = luad_data %>% filter(!is.na(Sex)),
           "race" = luad_data %>% filter(!is.na(`Race Category`)),
           "ethinicity" = luad_data %>% filter(!is.na(`Ethnicity Category`)),
           
    )
  })
  
  
  output$dynamic_plot <- renderPlot({ # prints a plot based on the input
    
    # Get the total number of valid samples
    filt_data <- filtered_data()
    total_filtered_samples <- nrow(filt_data)
    
    plot <- switch(input$plot_choice,
                   
                   
                   
                   # Bar Chart, Pie Chart, Density Plot
                   
                   "age" = {
                     
                     # Count the Average Diagnosis Age
                     average_age <- mean(filt_data$`Diagnosis Age`)
                     average_age <- round(average_age)
                     
                     # Create the plot
                     filt_data %>%
                       ggplot(aes(x = `Diagnosis Age`)) +
                       geom_bar() +
                       labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count") +
                       annotate("text", label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)), 
                                x = Inf, y = Inf, hjust = 1.0, vjust = 1.5) +
                       annotate("text", label = paste("Average Diagnosis Age:", average_age), 
                                x = Inf, y = Inf, hjust = 1.1, vjust = 3.8)
                   },
                   
                   "sex" = {
                     
                     # Male/Female Count
                     sex_counts <- filt_data %>%
                       count(Sex) # (Sex column in the filtered_sex dataframe)
                     
                     # Create the pie chart
                     ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Male/Female Diagnosed Ratio") +
                       scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)),  hjust = 0.5, vjust = -1)
                   },
                   
                   
                   "race" = {
                     
                     # Race Category Count
                     race_counts <- filt_data %>%
                       count(`Race Category`) # (Sex column in the filtered_sex dataframe)
                     
                     # Create the pie chart
                     ggplot(race_counts, aes(x = "", y = n, fill = `Race Category`)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Race Category Diagnosed Ratio") +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)), hjust = 0.5, vjust = -1)
                   },
                   
                   "ethinicity" = {
                     
                     # Ethnicity Category Count
                     ethnicity_counts <- filt_data %>%
                       count(`Ethnicity Category`) # (Sex column in the filtered_sex dataframe)
                     
                     # Create the pie chart
                     ggplot(ethnicity_counts, aes(x = "", y = n, fill = `Ethnicity Category`)) +
                       geom_bar(stat = "identity", width = 1) +
                       coord_polar("y", start = 0) +  # Making it a Pie Chart
                       theme_void() + # Remove background, grid, and axis marks
                       labs(title = "Ethnicity Category Diagnosed Ratio") +
                       geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +  # Add counts as labels
                       annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)),  hjust = 0.5, vjust = -1)
                   }
                   
    )
    
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