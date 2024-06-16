# Load R packages
library(shiny)  # Shiny App
library(shinythemes)
library(readr)
library(ggplot2)  # Data Visualisation
library(dplyr) # Data Manipulation (%>%)
library(DT)    # for adding searchability to said Datatables
library(markdown)
library(jpeg)  # For handling JPG images



# Read the TSV/CSV file for Datasets

# https://www.cbioportal.org/study/summary?id=luad_tcga_pan_can_atlas_2018
luad_data1 <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")
# https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
nsclc_data2 <- read.csv("www/manifest1603198545583/NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019.csv")
# https://www.cbioportal.org/study/clinicalData?id=luad_tcga
luad_data3 <- read_tsv("www/luad_tcga_firehose_legacy_clinical_data.tsv")

# Helper function to read JPG files
read_jpg_files <- function(path) {
  files <- list.files(path, full.names = TRUE, pattern = "\\.jpg$")
  return(files)
}

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
                                        selectInput("data_choice", "Choose Data Type:", 
                                                    choices = c("Tabular Data" = "tabular", "Image Data" = "image"))
                                    ),
                                    # Tabular
                                    conditionalPanel(
                                      condition = "input.data_choice == 'tabular'",
                                      radioButtons(inputId = "data_choice_tabular", label = "Choose Dataset:", 
                                                   choices = list("LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1", 
                                                                  "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
                                                                  "LUAD TCGA Firehose Legacy Clinical Data" = "luad_data3"))
                                    ),
                                    # Image
                                    conditionalPanel(
                                      condition = "input.data_choice == 'image'",
                                      radioButtons(inputId = "data_choice_image", label = "Choose Dataset:", 
                                                   choices = list("NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2"))
                                    )
                             )
                           ),
                           # Tabular LUAD 1
                           conditionalPanel(
                             condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data1'",
                             h1("LUAD Clinical Data"),
                             div(class = "scrollable-table", DTOutput("data_dsout1")),
                             h4("Summary of LUAD Clinical Data"),
                             div(class = "summary-text-box", verbatimTextOutput("data_summary1"))
                           ),
                           # Tabular NSCLC
                           conditionalPanel(
                             condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'nsclc_data2'",
                             h1("NSCLC Clinical Data"),
                             div(class = "scrollable-table", DTOutput("data_dsout2")),
                             h4("Summary of NSCLC Clinical Data"),
                             div(class = "summary-text-box", verbatimTextOutput("data_summary2"))
                           ),
                           # Tabular LUAD 3
                           conditionalPanel(
                             condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data3'",
                             h1("LUAD TCGA Firehose Legacy Clinical Data"),
                             div(class = "scrollable-table", DTOutput("data_dsout3")),
                             h4("Summary of LUAD TCGA Firehose Legacy Clinical Data"),
                             div(class = "summary-text-box", verbatimTextOutput("data_summary3"))
                           ),
                           # Image NSCLC
                           conditionalPanel(
                             condition = "input.data_choice == 'image' && input.data_choice_image == 'nsclc_data2'",
                             h1("NSCLC Image Data"),
                             h4("Dimensions: 512x512"),
                             h4("Format: DICOM"),
                             h4("Number of samples: 422"),
                             h4("Number of Images: 52,073"),
                             h4("Modality: CT/RT")
                           )
                  ),
                  
                  # Tab: PLOTS
                  tabPanel("PLOTS",
                           fluidRow(
                             column(12, class = "compact-container",
                                    div(class = "compact-select",
                                        selectInput("plot_data_choice", "Choose Data Type:", 
                                                    choices = c("Tabular Data" = "tabular", "Image Data" = "image"))
                                    ),
                                    # Tabular
                                    conditionalPanel(
                                      condition = "input.plot_data_choice == 'tabular'",
                                      radioButtons(inputId = "plot_data_choice_tabular", label = "Choose Dataset:", 
                                                   choices = list("LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1", 
                                                                  "NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019" = "nsclc_data2",
                                                                  "LUAD TCGA Firehose Legacy Clinical Data" = "luad_data3"))
                                    ),
                                    # LUAD Tabular
                                    conditionalPanel(
                                      condition = "input.plot_data_choice == 'tabular' && input.plot_data_choice_tabular == 'luad_data1'",
                                      radioButtons(inputId = "plot_choice_tabular1", label = "Choose Plot:", 
                                                   choices = list("Age" = "age", 
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "ethnicity"))
                                    ),
                                    # NSCLC Tabular
                                    conditionalPanel(
                                      condition = "input.plot_data_choice == 'tabular' && input.plot_data_choice_tabular == 'nsclc_data2'",
                                      radioButtons(inputId = "plot_choice_tabular2", label = "Choose Plot:", 
                                                   choices = list("Age" = "age2", 
                                                                  "Sex" = "sex2"))
                                    ),
                                    # LUAD Tabular 3
                                    conditionalPanel(
                                      condition = "input.plot_data_choice == 'tabular' && input.plot_data_choice_tabular == 'luad_data3'",
                                      radioButtons(inputId = "plot_choice_tabular3", label = "Choose Plot:", 
                                                   choices = list("Age" = "age3", 
                                                                  "Sex" = "sex3",
                                                                  "Race" = "race3",
                                                                  "Ethnicity" = "ethnicity3"))
                                    ),    
                                    
                                    # Image
                                    conditionalPanel(
                                      condition = "input.plot_data_choice == 'image'",
                                      radioButtons(inputId = "plot_data_choice_image", label = "Choose Dataset:", 
                                                   choices = list("NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019" = "nsclc_data2"))
                                    ),
                                    plotOutput(outputId = "dynamic_plot")
                             )
                           )
                  ),
                  
                  # Tab: IMAGES
                  tabPanel("IMAGES",
                           fluidRow(
                             column(3, class = "compact-container",
                                    div(class = "compact-select",
                                        selectInput("jpg_file_choice", "Choose JPG File:", 
                                                    choices = list.files("www/manifest1603198545583/NSCLCRadiomics/one", pattern = "\\.jpg$"))
                                    )
                             ),
                             column(9,
                                    plotOutput("jpg_image"),
                                    textOutput("file_path_debug")  # Add a text output for debugging
                             )
                           )
                  ),
                  
                  # Tab: REPORT
                  tabPanel("REPORT",
                           mainPanel(
                             includeMarkdown("www/Report.md")                                    
                           )
                  )
                  
                )
)



# Define server function  
server <- function(input, output) {
  
  
  # Tab: DATASET
  
  # Datasets
  output$data_dsout1 <- renderDataTable({
    datatable(luad_data1, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })
 
  output$data_dsout2 <- renderDataTable({
    datatable(nsclc_data2, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })

  output$data_dsout3 <- renderDataTable({
    datatable(luad_data3, options = list(scrollX = TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  })
  
  # Summaries
  output$data_summary1 <- renderPrint({
    summary(luad_data1)
  })
  
  output$data_summary2 <- renderPrint({
    summary(nsclc_data2)
  })
  
  output$data_summary3 <- renderPrint({
    summary(luad_data3)
  })
  
  # Function to render selected JPG image
  output$jpg_image <- renderPlot({
    req(input$jpg_file_choice)  # Ensure a file is selected
    
    file_path <- file.path("www/manifest1603198545583/NSCLCRadiomics/one", input$jpg_file_choice)
    
    # Use jpeg package to read and display the JPG image
    img <- readJPEG(file_path)
    
    # Plot the image
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)
    rasterImage(img, 0, 0, 1, 1)
    
  })
  
  # Tab: PLOTS
  total_samples_luad <- nrow(luad_data1)
  total_samples_nsclc <- nrow(nsclc_data2)
  total_samples_luad3 <- nrow(luad_data3)
  
  # Reactive expression to filter data based on plot choice
  filtered_data <- reactive({
    if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "luad_data1") {
      switch(input$plot_choice_tabular1,
             "age" = luad_data1 %>% filter(!is.na(`Diagnosis Age`)),
             "sex" = luad_data1 %>% filter(!is.na(Sex)),
             "race" = luad_data1 %>% filter(!is.na(`Race Category`)),
             "ethnicity" = luad_data1 %>% filter(!is.na(`Ethnicity Category`))
      )
    } else if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "nsclc_data2") {
      switch(input$plot_choice_tabular2,
             "age2" = nsclc_data2 %>% filter(!is.na(age)),
             "sex2" = nsclc_data2 %>% filter(!is.na(gender))
      )
    } else if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "luad_data3") {
      switch(input$plot_choice_tabular3,
             "age3" = luad_data3 %>% filter(!is.na(`Diagnosis Age`)),
             "sex3" = luad_data3 %>% filter(!is.na(Sex)),
             "race3" = luad_data3 %>% filter(!is.na(`Race Category`)),
             "ethnicity3" = luad_data3 %>% filter(!is.na(`Ethnicity Category`))
      )
    }
  })
  
  output$dynamic_plot <- renderPlot({
    # Get the filtered data
    filt_data <- filtered_data()
    
    if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "luad_data1") {
      total_filtered_samples <- nrow(filt_data)
      
      plot <- switch(input$plot_choice_tabular1,
                     "age" = {
                       average_age <- mean(filt_data$`Diagnosis Age`, na.rm = TRUE)
                       ggplot(filt_data, aes(x = `Diagnosis Age`)) +
                         geom_bar() +
                         labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count") +
                         annotate("text", label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad - total_filtered_samples)), 
                                  x = Inf, y = Inf, hjust = 1.0, vjust = 1.5) +
                         annotate("text", label = paste("Average Diagnosis Age:", round(average_age, 2)), 
                                  x = Inf, y = Inf, hjust = 1.1, vjust = 3.8)
                     },
                     "sex" = {
                       sex_counts <- filt_data %>% count(Sex)
                       ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Male/Female Diagnosed Ratio") +
                         scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad - total_filtered_samples)), hjust = 0.5, vjust = -1)
                     },
                     "race" = {
                       race_counts <- filt_data %>% count(`Race Category`)
                       ggplot(race_counts, aes(x = "", y = n, fill = `Race Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Race Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad - total_filtered_samples)), hjust = 0.5, vjust = -1)
                     },
                     "ethnicity" = {
                       ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
                       ggplot(ethnicity_counts, aes(x = "", y = n, fill = `Ethnicity Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Ethnicity Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad - total_filtered_samples)),  hjust = 0.5, vjust = -1)
                     }
      )
      
      print(plot)
      
    } else if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "nsclc_data2") {
      total_filtered_samples <- nrow(filt_data)
      
      plot <- switch(input$plot_choice_tabular2,
                     "age2" = {
                       # Create a histogram instead of a bar plot for continuous age
                       ggplot(filt_data, aes(x = age)) +
                         geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
                         labs(title = "Distribution of Age", x = "Age", y = "Count") +
                         annotate("text", label = paste("Valid Filtered Samples:", total_filtered_samples), 
                                  x = Inf, y = Inf, hjust = 1.0, vjust = 1.5)
                     },
                     "sex2" = {
                       sex_counts <- filt_data %>% count(gender)
                       ggplot(sex_counts, aes(x = "", y = n, fill = gender)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Male/Female Diagnosed Ratio") +
                         scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples),  hjust = 0.5, vjust = -1)
                     }
      )
      
      print(plot)
    } else if (input$plot_data_choice == "tabular" && input$plot_data_choice_tabular == "luad_data3") {
      total_filtered_samples <- nrow(filt_data)
      
      plot <- switch(input$plot_choice_tabular3,
                     "age3" = {
                       average_age <- mean(filt_data$`Diagnosis Age`, na.rm = TRUE)
                       ggplot(filt_data, aes(x = `Diagnosis Age`)) +
                         geom_bar() +
                         labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count") +
                         annotate("text", label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad3 - total_filtered_samples)), 
                                  x = Inf, y = Inf, hjust = 1.0, vjust = 1.5) +
                         annotate("text", label = paste("Average Diagnosis Age:", round(average_age, 2)), 
                                  x = Inf, y = Inf, hjust = 1.1, vjust = 3.8)
                     },
                     "sex3" = {
                       sex_counts <- filt_data %>% count(Sex)
                       ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Male/Female Diagnosed Ratio") +
                         scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad - total_filtered_samples)), hjust = 0.5, vjust = -1)
                     },
                     "race3" = {
                       race_counts <- filt_data %>% count(`Race Category`)
                       ggplot(race_counts, aes(x = "", y = n, fill = `Race Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Race Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad3 - total_filtered_samples)), hjust = 0.5, vjust = -1)
                     },
                     "ethnicity3" = {
                       ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
                       ggplot(ethnicity_counts, aes(x = "", y = n, fill = `Ethnicity Category`)) +
                         geom_bar(stat = "identity", width = 1) +
                         coord_polar("y", start = 0) +
                         theme_void() +
                         labs(title = "Ethnicity Category Diagnosed Ratio") +
                         geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
                         annotate("text", x = Inf, y = Inf, label = paste("Valid Filtered Samples:", total_filtered_samples, "NA:", (total_samples_luad3 - total_filtered_samples)),  hjust = 0.5, vjust = -1)
                     }
      )
      
      print(plot)
    }
  })
  
  # Tab: REPORT
  # Output for the report text file
  output$research_report <- renderText({
    text_content
  })
  
}



# Create Shiny object
shinyApp(ui = ui, server = server)