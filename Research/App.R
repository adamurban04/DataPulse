# Load R packages
library(shiny)  # Shiny App
library(shinythemes)
library(readr)
library(ggplot2)  # Data Visualisation
library(dplyr) # Data Manipulation (%>%)
library(DT)    # Searchability to Datatables
library(markdown)
library(jpeg)  # JPG images
library(Seurat) # scRNA-seq data analysis
library(plotly) # Interactive Plots
library(corrplot) # Correlation Matrix

#Cmd+Shift+A to reformat code

# Read the TSV/CSV file for Datasets

# https://www.cbioportal.org/study/clinicalData?id=luad_tcga_pan_can_atlas_2018
luad_data1 <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")
# https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
nsclc_data2 <- read.csv("www/manifest1603198545583/NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019.csv")
# https://www.cbioportal.org/study/clinicalData?id=luad_tcga
luad_data3 <- read_tsv("www/luad_tcga_firehose_legacy_clinical_data.tsv")
# https://www.cbioportal.org/study/clinicalData?id=luad_oncosg_2020
luad_data4 <- read_tsv("www/luad_oncosg_2020_clinical_data.tsv")





# Helper function to read JPG files
read_jpg_files <- function(path) {
  files <- list.files(path, full.names = TRUE, pattern = "\\.jpg$")
  return(files)
}






# Define UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  
  # Custom CSS to style the Research Report text box (cmnd+opt+L to fold)
  tags$head(tags$style(
    HTML(
      "
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
                          .image-box {
                            width: 512px;
                            height: 512px;

                          }
                           .plot-container {
                             margin-top: 20px;
                             margin-bottom: 20px;
                             padding: 10px;
                             border: 1px solid #ddd;
                             border-radius: 5px;
                           }
                        "
    )
  )),
  
  # Navigation
  navbarPage(
    "UPSTaRT",
    # UI Tab 1: DATA
    tabPanel(
      "DATA",
      fluidRow(
        column(
          12,
          class = "compact-container",
          div(
            class = "compact-select",
            selectInput(
              "data_choice",
              "Choose Data Type:",
              choices = c("Tabular Data" = "tabular", "Image Data" = "image")
            )
          ),
          # Tabular
          conditionalPanel(
            condition = "input.data_choice == 'tabular'",
            radioButtons(
              inputId = "data_choice_tabular",
              label = "Choose Dataset:",
              choices = list(
                "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
                "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
                "LUAD TCGA Firehose Legacy Clinical Data" = "luad_data3",
                "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4"
              )
            )
          ),
          # Image
          conditionalPanel(
            condition = "input.data_choice == 'image'",
            radioButtons(
              inputId = "data_choice_image",
              label = "Choose Dataset:",
              choices = list("NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2")
            )
          )
        )
      ),
      # Tabular LUAD 1
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data1'",
        h1("LUAD TCGA Pan Can Atlas 2018 Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout1")),
        h4("Summary of LUAD TCGA Pan Can Atlas 2018 Clinical Data"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary1"))
      ),
      # Tabular NSCLC 2
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'nsclc_data2'",
        h1("NSCLC-Radiomics Lung1 Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout2")),
        h4("Summary of NSCLC-Radiomics Lung1 Clinical Data"),
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
      # Tabular LUAD 4
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data4'",
        h1("LUAD (OncoSG, Nat Genet 2020) Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout4")),
        h4("Summary of LUAD (OncoSG, Nat Genet 2020) Clinical Data"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary4"))
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
    
    # UI Tab 2: PLOTS
    tabPanel(
      "PLOTS",
      fluidRow(
        column(
          12,
          class = "compact-container",
          div(
            class = "compact-select",
            radioButtons(
              inputId = "plot_data_choice_tabular",
              label = "Choose Dataset:",
              choices = list(
                "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
                "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
                "LUAD TCGA Firehose Legacy Clinical Data" = "luad_data3",
                "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4"
              )
            )
          ),
          
          # LUAD Tabular 1
          conditionalPanel(
            condition = "input.plot_data_choice_tabular == 'luad_data1'",
            radioButtons(
              inputId = "plot_choice_tabular1",
              label = "Choose Plot:",
              choices = list(
                "Age" = "age",
                "Sex" = "sex",
                "Race" = "race",
                "Ethnicity" = "ethnicity"
              )
            )
          ),
          # NSCLC Tabular 2
          conditionalPanel(
            condition = "input.plot_data_choice_tabular == 'nsclc_data2'",
            radioButtons(
              inputId = "plot_choice_tabular2",
              label = "Choose Plot:",
              choices = list("Age" = "age2", "Sex" = "sex2")
            )
          ),
          # LUAD Tabular 3
          conditionalPanel(
            condition = "input.plot_data_choice_tabular == 'luad_data3'",
            radioButtons(
              inputId = "plot_choice_tabular3",
              label = "Choose Plot:",
              choices = list(
                "Age" = "age3",
                "Sex" = "sex3",
                "Race" = "race3",
                "Ethnicity" = "ethnicity3"
              )
            )
          ),
          
          # LUAD Tabular 4
          conditionalPanel(
            condition = "input.plot_data_choice_tabular == 'luad_data4'",
            radioButtons(
              inputId = "plot_choice_tabular4",
              label = "Choose Plot:",
              choices = list(
                "Age" = "age4",
                "Sex" = "sex4",
                "Ethnicity" = "ethnicity4"
              )
            )
          ),
          
          plotlyOutput(outputId = "dynamic_plot")
          
        ),
      ),
      column(12, class = "compact-container", div(class = "compact-select", htmlOutput("total_samples")))
    ),
    
    # UI Tab 3: CORRELATION MATRIX
    tabPanel("CORRELATION MATRIX", fluidRow(
      mainPanel(
        radioButtons(
          inputId = "cor_matrix_choice",
          label = "Choose Dataset:",
          choices = list(
            "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
            "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
            "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4"
          )
        ),
        class = "compact-select",
        plotOutput("corrPlot", height = "800px")
      ),
      
      
      
    )),
    
    # UI Tab 4: IMAGES
    tabPanel("IMAGES", fluidRow(
      column(
        3,
        class = "compact-container",
        div(
          class = "compact-select",
          selectInput(
            "jpg_file_choice",
            "Choose JPG File:",
            choices = list.files("www/manifest1603198545583/NSCLCRadiomics/images", pattern = "\\.jpg$")
          )
        ),
        h4(HTML("<u>x.abc.jpg</u>")),
        h5(HTML("<i>x</i> for patient")),
        h5(HTML("<i>abc</i> for picture in sequence"))
        
        
      ),
      
      column(12, class = "image-box", plotOutput("jpg_image"), )
    )),
    
    # UI Tab 5: REPORT
    tabPanel("REPORT", mainPanel(includeMarkdown("www/Report.md")))
    
  )
)



# Define server function
server <- function(input, output) {
  
  # SERVER Tab 1: DATA
  
  # List of data frames
  data_list <- list(luad_data1, nsclc_data2, luad_data3, luad_data4)
  
  # Names for the outputs
  output_names <- c("data_dsout1", "data_dsout2", "data_dsout3", "data_dsout4")
  summary_names <- c("data_summary1", "data_summary2", "data_summary3", "data_summary4")
  
  # Function to create DataTable
  render_data_table <- function(data, name) {
    output[[name]] <- renderDataTable({
      datatable(data, options = list(scrollX = TRUE, lengthMenu = list(
        c(10, 25, 50, -1), c('10', '25', '50', 'All')
      )))
    })
  }
  
  # Function to create summary
  render_data_summary <- function(data, name) {
    output[[name]] <- renderPrint({
      summary(data)
    })
  }
  
  # Apply the functions to each data frame and name
  mapply(render_data_table, data_list, output_names)
  mapply(render_data_summary, data_list, summary_names)
  
  
  
  # SERVER Tab 2: PLOTS
  
  # Reactive expression to filter data based on plot choice
  filtered_data <- reactive({
    if (input$plot_data_choice_tabular == "luad_data1") {
      switch(
        input$plot_choice_tabular1,
        "age" = luad_data1 %>% filter(!is.na(`Diagnosis Age`)),
        "sex" = luad_data1 %>% filter(!is.na(Sex)),
        "race" = luad_data1 %>% filter(!is.na(`Race Category`)),
        "ethnicity" = luad_data1 %>% filter(!is.na(`Ethnicity Category`))
      )
    } else if (input$plot_data_choice_tabular == "nsclc_data2") {
      switch(
        input$plot_choice_tabular2,
        "age2" = nsclc_data2 %>% filter(!is.na(age)),
        "sex2" = nsclc_data2 %>% filter(!is.na(gender))
      )
    } else if (input$plot_data_choice_tabular == "luad_data3") {
      switch(
        input$plot_choice_tabular3,
        "age3" = luad_data3 %>% filter(!is.na(`Diagnosis Age`)),
        "sex3" = luad_data3 %>% filter(!is.na(Sex)),
        "race3" = luad_data3 %>% filter(!is.na(`Race Category`)),
        "ethnicity3" = luad_data3 %>% filter(!is.na(`Ethnicity Category`))
      )
    } else if (input$plot_data_choice_tabular == "luad_data4") {
      switch(
        input$plot_choice_tabular4,
        "age4" = luad_data4 %>% filter(!is.na(Age)),
        "sex4" = luad_data4 %>% filter(!is.na(Sex)),
        "ethnicity4" = luad_data4 %>% filter(!is.na(`Ethnicity Category`))
      )
    }
  })
  
  # Plot Output
  
  output$dynamic_plot <- renderPlotly({
    # Get the filtered data
    filt_data <- filtered_data()
    
    plot <- NULL
    if (input$plot_data_choice_tabular == "luad_data1") {
      plot <- switch(
        input$plot_choice_tabular1,
        "age" = {
          ggplot(filt_data, aes(x = `Diagnosis Age`)) +
            geom_bar() +
            labs(title = "Distribution of Diagnosis Age",
                 x = "Age", y = "Count")
        },
        "sex" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
            layout(title = "Male/Female Diagnosed Ratio")
        },
        "race" = {
          race_counts <- filt_data %>% count(`Race Category`)
          plot_ly(race_counts, labels = ~`Race Category`, values = ~n, type = 'pie', textinfo = 'label+percent', textposition = 'auto', hoverinfo = 'label+value') %>%
            layout(title = 'Race Category Diagnosed Ratio', showlegend = TRUE, legend = list(x = 1, y = 0.5), margin = list(l = 20, r = 20, t = 30, b = 20))
        },
        "ethnicity" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie', textinfo = 'label+percent', textposition = 'auto', hoverinfo = 'label+value') %>%
            layout(title = 'Ethnicity Category Diagnosed Ratio', showlegend = TRUE, legend = list(x = 1.1, y = 0.5), margin = list(l = 20, r = 20, t = 30, b = 20))
        }
      )
    } else if (input$plot_data_choice_tabular == "nsclc_data2") {
      plot <- switch(
        input$plot_choice_tabular2,
        "age2" = {
          ggplot(filt_data, aes(x = age)) +
            geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
            labs(title = "Distribution of Age", x = "Age", y = "Count")
        },
        "sex2" = {
          sex_counts <- filt_data %>% count(gender)
          plot_ly(sex_counts, labels = ~gender, values = ~n, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
            layout(title = "Male/Female Diagnosed Ratio")
        }
      )
    } else if (input$plot_data_choice_tabular == "luad_data3") {
      plot <- switch(
        input$plot_choice_tabular3,
        "age3" = {
          ggplot(filt_data, aes(x = `Diagnosis Age`)) +
            geom_bar() +
            labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count")
        },
        "sex3" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
            layout(title = "Male/Female Diagnosed Ratio")
        },
        "race3" = {
          race_counts <- filt_data %>% count(`Race Category`)
          plot_ly(race_counts, labels = ~`Race Category`, values = ~n, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
            layout(title = "Race Category Diagnosed Ratio")
        },
        "ethnicity3" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie', textinfo = 'label+percent', textposition = 'auto', hoverinfo = 'label+value') %>%
            layout(title = 'Ethnicity Category Diagnosed Ratio', showlegend = TRUE, legend = list(x = 1.1, y = 0.5), margin = list(l = 20, r = 20, t = 30, b = 20))
        }
      )
    } else if (input$plot_data_choice_tabular == "luad_data4") {
      plot <- switch(
        input$plot_choice_tabular4,
        "age4" = {
          ggplot(filt_data, aes(x = Age)) +
            geom_bar() +
            labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count")
        },
        "sex4" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
            layout(title = "Male/Female Diagnosed Ratio")
        },
        "ethnicity4" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie', textinfo = 'label+percent', textposition = 'auto', hoverinfo = 'label+value') %>%
            layout(title = 'Ethnicity Category Diagnosed Ratio', showlegend = TRUE, legend = list(x = 1.1, y = 0.5), margin = list(l = 20, r = 20, t = 30, b = 20))
        }
      )
    }
    
    plot
  })
  
  total_samples <- reactive({
    dataset_choice <- switch(
      input$plot_data_choice_tabular,
      "luad_data1" = nrow(luad_data1),
      "nsclc_data2" = nrow(nsclc_data2),
      "luad_data3" = nrow(luad_data3),
      "luad_data4" = nrow(luad_data4)
    )
    dataset_choice
  })
  
  # Output the total samples
  output$total_samples <- renderText({
    paste(
      "Total samples in selected dataset:", total_samples(), "<br>",
      "Total filtered samples in selected dataset:", nrow(filtered_data())
    )
  })
  
  # SERVER Tab 3: CORRELATION MATRIX
  
  # Diagnosis Age: Has a negative correlation with variables such as Overall Survival (Months) and Progress Free Survival (Months), indicating that older patients may have poorer outcomes
  
  # Select numeric columns for correlation analysis
  
  
  numeric_data <- reactive({
    selected_data <- switch(
      input$cor_matrix_choice,
      "luad_data1" = luad_data1 %>% select_if(is.numeric) %>% na.omit(),
      "nsclc_data2" = nsclc_data2 %>% select_if(is.numeric) %>% na.omit(),
      "luad_data4" = luad_data4 %>% select_if(is.numeric) %>% na.omit())
    
    # Check for columns with zero variance and remove them
    selected_data <- selected_data[, apply(selected_data, 2, function(x) var(x) != 0)]
    return(selected_data)
  })
  
  
  
  
  # Render the correlation plot
  output$corrPlot <- renderPlot({
    if (ncol(numeric_data()) > 1) {
      cor_matrix <- cor(numeric_data(), use = "complete.obs")
      par(mar = c(2, 2, 1, 1))  # Adjust margin if necessary
      corrplot(cor_matrix, method = "color", tl.cex = 0.7, 
               title = "Correlation Matrix", mar = c(0, 0, 1, 0),
               col = colorRampPalette(c("blue", "white", "red"))(100),
               number.cex = 0.7, tl.col = "black")
    } else {
      plot.new()
      text(0.5, 0.5, "Not enough numeric columns for correlation analysis.")
    }
  })
  
  # SERVER Tab 4: IMAGES
  
  # Function to render selected JPG image
  output$jpg_image <- renderPlot({
    req(input$jpg_file_choice)  # Ensure a file is selected
    
    file_path <- file.path("www/manifest1603198545583/NSCLCRadiomics/images", input$jpg_file_choice)
    
    # Use jpeg package to read and display the JPG image
    img <- readJPEG(file_path)
    
    # Plot the image
    plot(
      0, 0, type = "n",
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = "", ylab = "", axes = FALSE
    )
    rasterImage(img, 0, 0, 1, 1)
  })
  
  # SERVER Tab 5: REPORT
  
  # Output for the report text file
  output$research_report <- renderText({
    text_content
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
