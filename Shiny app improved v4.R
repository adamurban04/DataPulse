# Load R packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr) # Data Manipulation (%>%)
library(DT)    # for adding searchability to said Datatables
library(markdown)


# Note to any editors or reviewers, this code will not operate on your machines
# Without editing the file path for both Luad data (line 15) and Report (line 113)

# Read the TSV file for Dataset
luad_data <- read_tsv("C:/Users/jamco/Downloads/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")


# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Custom CSS to style the Research Report text box (cmnd+opt+L to fold)
                tags$head(tags$style(HTML("
                  .scrollable-table {
                    overflow-x: auto;
                    white-space: nowrap;
                    width: 100%;
                  }
                  .scrollable-table table {
                    width: 100%;
                  }
                  .summary-text-box {
                    width: 100%;
                    height: 150px; 
                    overflow-y: scroll;
                    white-space: pre-wrap;
                    word-wrap: break-word;
                    border: 1px solid #ccc;
                    padding: 10px;
                    background-color: #f9f9f9;
                  }
                  .compact-container {
                    margin: 0;
                    padding: 10px;
                  }
                  .compact-select {
                    margin-bottom: 10px;
                  }
                "))),
                
                # Navigation
                navbarPage(
                  "UPSTaRT",
                  
                  # Tab: DATA
                  tabPanel("DATA",
                           fluidRow(
                             column(12, class = "compact-container",
                                    div(class = "compact-select",
                                        selectInput("data_choice", "Choose Dataset:", 
                                                    choices = c("Clinical Data" = "clinical", "Image Data" = "image"))
                                    ),
                                    # conditional panels will be very useful in showing different Datasets
                                    conditionalPanel(
                                      condition = "input.data_choice == 'clinical'",
                                      h1("LUAD Clinical Data"),
                                      div(class = "scrollable-table", DTOutput("clinical_dsout")),
                                      h4("Summary of Clinical Data"),
                                      div(class = "summary-text-box", verbatimTextOutput("clinical_summary"))
                                    ),
                                    conditionalPanel(
                                      # placeholder text until image data is provided
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
                           fluidRow(
                             column(12, class = "compact-container",
                                    div(class = "compact-select",
                                        selectInput("plot_data_choice", "Choose Data Type:", 
                                                    choices = c("Clinical Data" = "clinical", "Image Data" = "image"))
                                    ),
                                    conditionalPanel(
                                      # conditional panels used for same reasons as above 
                                      condition = "input.plot_data_choice == 'clinical'",
                                      radioButtons(inputId = "plot_choice_clinical", label = "Choose Plot:", 
                                                   choices = list("Age" = "age", 
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "ethnicity"))
                                    ),
                                    conditionalPanel(
                                      #placeholder text until image data is provided
                                      condition = "input.plot_data_choice == 'image'",
                                      radioButtons(inputId = "plot_choice_image", label = "Choose Plot:", 
                                                   choices = list("Resolution" = "resolution"))
                                    ),
                                    plotOutput(outputId = "dynamic_plot")
                             )
                           )
                  ),
                  
                  # Tab: REPORT
                  tabPanel("REPORT",
                           mainPanel(
                             includeMarkdown("C:/Users/jamco/Downloads/Report.md")                                      
                           )
                  )
                )
)

# Define server function  
server <- function(input, output) {
  
  # Output the clinical dataset
  output$clinical_dsout <- renderDataTable({
    datatable(luad_data, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })
  
  # Output for summary of the clinical dataset
  output$clinical_summary <- renderPrint({
    summary(luad_data)
  })
  
  # Output the image dataset
  output$image_dsout <- renderDataTable({
    datatable(image_data, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })
  
  # Output for summary of the image dataset
  output$image_summary <- renderPrint({
    summary(image_data)
  })
  
  # Tab: PLOTS
  
  total_samples <- nrow(luad_data)
  
  # Reactive expression to filter data based on plot choice for clinical data
  filtered_data_clinical <- reactive({
    switch(input$plot_choice_clinical,
           "age" = luad_data %>% filter(!is.na(`Diagnosis Age`)),
           "sex" = luad_data %>% filter(!is.na(Sex)),
           "race" = luad_data %>% filter(!is.na(`Race Category`)),
           "ethnicity" = luad_data %>% filter(!is.na(`Ethnicity Category`))
    )
  })
  
  # Reactive expression to filter data based on plot choice for image data
  filtered_data_image <- reactive({
    switch(input$plot_choice_image,
           "resolution" = image_data %>% filter(!is.na(Resolution))
    )
  })
  
  output$dynamic_plot <- renderPlot({
    if (input$plot_data_choice == "clinical") {
      filt_data <- filtered_data_clinical()
      total_filtered_samples <- nrow(filt_data)
      
      plot <- switch(input$plot_choice_clinical,
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
                       sex_counts <- filt_data %>% count(Sex)
                       
                       # Create the pie chart
                       ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Male/Female Diagnosed Ratio") +
                         scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)),  hjust = 0.5, vjust = -1)
                     },
                     "race" = {
                       # Race Category Count
                       race_counts <- filt_data %>% count(`Race Category`)
                       
                       # Create the pie chart
                       ggplot(race_counts, aes(x = "", y = n, fill = `Race Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Race Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)), hjust = 0.5, vjust = -1)
                     },
                     "ethnicity" = {
                       # Ethnicity Category Count
                       ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
                       
                       # Create the pie chart
                       ggplot(ethnicity_counts, aes(x = "", y = n, fill = `Ethnicity Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Ethnicity Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples-total_filtered_samples)),  hjust = 0.5, vjust = -1)
                     }
      )
    } else {
      filt_data <- filtered_data_image()
      total_filtered_samples <- nrow(filt_data)
      
      plot <- switch(input$plot_choice_image,
                     "resolution" = {
                       resolution_counts <- filt_data %>% count(Resolution)
                       
                       ggplot(resolution_counts, aes(x = "", y = n, fill = Resolution)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Image Resolution Distribution") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples), hjust = 0.5, vjust = -1)
                     }
      )
    }
    
    print(plot)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
