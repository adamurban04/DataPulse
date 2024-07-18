# FULL SHINY APP FOR SHINYAPPS.IO DEPLOYMENT

# Load R packages
library(shiny)  # Shiny App
library(shinythemes)
library(readr)
library(ggplot2)  # Data Visualisation
library(dplyr) # Data Manipulation (%>%)
library(DT)    # Searchability to Datatables
library(markdown)
library(jpeg)  # JPG images
library(plotly) # Interactive Plots
library(corrplot) # Correlation Matrix
library(table1) # Descriptive Tables
library(tidyr)

# Read the TSV/CSV file for Datasets

# https://www.cancerrxgene.org/downloads/drug_data?pathway=All&tissue=LUAD
drug_data <- read_csv("www/LUAD_IC_Sun_Jul_14_11_36_44_2024.csv")
# https://www.cancerrxgene.org/downloads/drug_data?screening_set=GDSC1&tissue=LUAD
drug_data2 <- read_csv("www/LUAD_IC_Thu_Jul_18_13_08_48_2024.csv")

# List of drug datasets
datasets_drug <- list(
  "GDSC1" = "drug_data",
  "GDSC2" = "drug_data2"
)

# https://www.cbioportal.org/study/clinicalData?id=luad_tcga_pan_can_atlas_2018
luad_data1 <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")
# https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
nsclc_data2 <- read.csv("www/manifest1603198545583/NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019.csv")
# https://www.cbioportal.org/study/clinicalData?id=luad_tcga
luad_data3 <- read_tsv("www/luad_tcga_firehose_legacy_clinical_data.tsv")
# https://www.cbioportal.org/study/clinicalData?id=luad_oncosg_2020
luad_data4 <- read_tsv("www/luad_oncosg_2020_clinical_data.tsv")

datasets_clinical <- list("LUAD TCGA" = luad_data1, "NSCLC Radiomics" = nsclc_data2, "LUAD TCGA Firehose" = luad_data3, "LUAD OncoSG" = luad_data4)

# Datasets of images
datasets <- list(
  "NSCLC LUNG1-001" = "www/0.000000-NA-20785",
  "NSCLC LUNG1-002" = "www/0.000000-NA-82046",
  "NSCLC LUNG1-003" = "www/1.000000-NA-28595",
  "NSCLC LUNG1-004" = "www/1.000000-NA-61228")

# Helper function to read JPG files
read_jpg_files <- function(path) {
  files <- list.files(path, full.names = TRUE, pattern = "\\.jpg$")
  return(files)
}

# Helper Function to read and convert DICOM to PNG
convert_dicom_to_png <- function(file_path, output_dir) {
  dicom_data <- readDICOMFile(file_path)
  img_data <- dicom_data$img
  
  # Extract original file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Ensure the image data is numeric and properly scaled
  if (is.matrix(img_data) || is.array(img_data)) {
    img_data <- as.numeric(img_data)
    img_data <- img_data / max(img_data)  # Scale to [0, 1]
    img_data <- matrix(img_data, nrow = nrow(dicom_data$img), ncol = ncol(dicom_data$img))
    
    # Construct output path
    output_path <- file.path(output_dir, paste0(file_name, ".png"))
    writePNG(img_data, output_path)
    
    # Return the output path
    return(output_path)
  } else {
    stop("Image data is not in the correct format")
  }
}

# Helper Function to scan all DICOM files in a directory
scan_dicom_files <- function(directory_path) {
  if (dir.exists(directory_path)) {
    dicom_files <- list.files(directory_path, pattern = "\\.dcm$", full.names = TRUE)
    if (length(dicom_files) > 0) {
      png_files <- lapply(dicom_files, function(dcm_file) {
        png_file <- tools::file_path_sans_ext(dcm_file) %>% paste0(".png")
        if (!file.exists(png_file)) {
          tryCatch({
            im <- oro.dicom::readDICOM(dcm_file)
            png::writePNG(im, png_file)
          }, error = function(e) {
            warning(paste("Error converting", dcm_file, ": ", e$message))
          })
        }
        png_file
      })
      return(unlist(png_files))
    } else {
      warning("No DICOM files found in directory:", directory_path)
      return(NULL)
    }
  } else {
    stop("Directory does not exist:", directory_path)
  }
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  
  # HTML to style certain containers (cmnd+opt+L to fold)
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
                            height: 350px;
                            overflow-y: scroll;
                            white-space: pre-wrap;
                            word-wrap: break-word;
                            border: 1px solid #ccc;
                            padding: 20px;
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
                           .image-container {

                             display: flex;

                             justify-content: center;

                             align-items: center;

                             width: 100%;

                             height: auto;

                             }

                            .image-container img {

                            max-width: 100%;

                             height: auto;

                            }
                            .space-before-panel {
                            margin-top: 50px;
                            }
                            .container-box {
                            background-color: #f9f9f9;
                            padding: 20px;
                            border-radius: 10px;
                            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                            }
                            .table1-output {
                            width: 100%;
                            overflow-x: auto;
                            }
      "
    )
  )),
  
  # Navigation
  navbarPage(
    "UPSTaRT",
    # UI Tab 1: DATA
    tabPanel(
      "DATA OVERVIEW",
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
              label = "Choose Dataset to view:",
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
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary1"))
      ),
      # Tabular NSCLC 2
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'nsclc_data2'",
        h1("NSCLC-Radiomics Lung1 Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout2")),
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary2"))
      ),
      # Tabular LUAD 3
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data3'",
        h1("LUAD TCGA Firehose Legacy Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout3")),
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary3"))
      ),
      # Tabular LUAD 4
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'luad_data4'",
        h1("LUAD (OncoSG, Nat Genet 2020) Clinical Data"),
        div(class = "scrollable-table", DTOutput("data_dsout4")),
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
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
      ),
      
    ),
    
    # UI Tab 2: STRATIFIED ANALYSIS
    
    tabPanel(
      "STRATIFIED ANALYSIS",
      fluidRow(
        
        h3("Descriptive Statistics with table1 Package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        selectInput('dataset_t1_choice', 'Select Dataset', choices = names(datasets_clinical)),
        uiOutput("variable_select"), # Dynamic variable selection based on chosen dataset
        
        div(style = "border-color: #C0C0C0; border-width: 1px; border-style: solid; padding: 15px;",
            h3(""),
            div(class = "table1-output", tableOutput("T1"))
        )
      )
    ),
    
    
    # UI Tab 3: PLOTS
    tabPanel(
      "PLOTS",
      fluidRow(
        h3("Data Visualisation with plotly Package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
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
    
    # UI Tab 4: CORRELATION MATRIX
    tabPanel("CORRELATION MATRIX", fluidRow(
      h3("Correlation Matrix with corrPlot package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      column(
        12,
        class = "compact-container",
        div(
          class = "compact-select",
          radioButtons(
          inputId = "cor_matrix_choice",
          label = "Choose Dataset:",
          choices = list(
            "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
            "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
            "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4"
          ))
        ),
        class = "compact-select",
        plotOutput("corrPlot", width = "800px", height = "800px")
      ),
    )),
    
    # UI Tab 5: DRUG SENSITIVITY
    tabPanel("DRUG SENSITIVITY", fluidRow(
      h3("Drug Sensitivity T-test Analysis", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset_choice", "Select Dataset:", choices = names(datasets_drug)),
          # The datasets in the app are only lung_NSCLC_adenocarcinoma "Tissue Sub-type"
          selectInput("group_var", "Select Grouping Variable:", choices = c("Drug Name", "Cell Line Name", "Tissue Sub-type")),
          uiOutput("level1_ui"),
          uiOutput("level2_ui"),
          actionButton("run_test", "Run T-test")
        ),
        mainPanel(
          verbatimTextOutput("t_test_result")
        )
      ))
    ),
    
    # UI Tab 6: RADIOGENOMIC ANALYSIS
    tabPanel("RADIOGENOMIC ANALYSIS", fluidRow(
      h3("Radiogenomic Analysis with RadioGx", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      sidebarLayout(
        sidebarPanel(
          selectInput("output_choice", "Select Output to Display:", 
                      choices = list(
                        "Radiogenomic Data" = "data",
                        "Molecular Feature Data" = "molecularFeatureData",
                        "Sensitivity Data" = "sensitivity",
                        "Linear Quadratic Model" = "LQ_model",
                        "Survival Fraction After 2 Units" = "survFracAfter2Units",
                        "Dose for 10% Survival" = "dose10PercentSurv",
                        "Area Under Dose-Response Curve" = "areaUnderDoseRespCurve",
                        "Dose-Response Curve" = "doseResponseCurve"
                      )
          ),
          uiOutput("cell_line_ui")
        ),
        mainPanel(
          lapply(c("data", "molecularFeatureData", "sensitivity", "LQ_model", "survFracAfter2Units", 
                   "dose10PercentSurv", "areaUnderDoseRespCurve", "doseResponseCurve"), function(choice) {
                     conditionalPanel(
                       condition = paste0("input.output_choice == '", choice, "'"),
                       if (choice == "doseResponseCurve") {
                         plotOutput(paste0("radioGx_", choice))
                       } else {
                         verbatimTextOutput(paste0("radioGx_", choice))
                       }
                     )
                   })
        )
      )
    )),
    
    # UI Tab 7: IMAGES
    tabPanel("IMAGES", fluidRow(
      h3("DICOM Viewer", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset_selector", "Choose Dataset:", choices = names(datasets)),
          sliderInput("image_slider", "Image Number", min = 1, max = 1, value = 1, step = 1)
        ),
        mainPanel(
          textOutput("image_title"),
          plotOutput("image_display", width = "512px", height = "512px")
        )
      ))
    ),
    
    # UI Tab 8: REPORT
    tabPanel("REPORT", 
             h3("Research Report", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
             mainPanel(includeMarkdown("www/Report.md")))
    
  )
)

# Define server function
server <- function(input, output, session) {
  
  # SERVER Tab 1: DATA OVERVIEW
  
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
  
  
  
  
  
  # SERVER Tab 2: STRATIFIED ANALYSIS
  
  # Table1 Descriptive Statistics
  # (general statistics and stratified analysis by a grouping variable)
  
  # Reactive expression to get selected dataset
  selected_data_t1 <- reactive({
    datasets_clinical[[input$dataset_t1_choice]]
  })
  
  # Dynamic UI for selecting variable based on selected dataset
  output$variable_select <- renderUI({
    req(selected_data_t1())
    
    # CAN LATER REMOVE UNWANTED VARIABLES:
    # cat_vars <- setdiff(names(selected_data()), "PatientID")
    
    selectInput('cat_var', 'Variable', choices = names(selected_data_t1()))
  })
  
  # Reactive expression to filter data based on selected group
  filtered_data_t1 <- reactive({
    data_t1 <- selected_data_t1()
    
    # Common columns to use if they exist in the dataset
    common_columns_t1 <- c("Diagnosis Age", "age", "Age", "sex", "Sex", "Gender", "Race Category", "Neoplasm Disease Stage American Joint Committee on Cancer Code")
    
    # Check which common columns are present in the dataset
    present_common_columns_t1 <- intersect(common_columns_t1, colnames(data_t1))
    
    if (!is.null(input$cat_var) && input$cat_var != "" && input$cat_var %in% colnames(data_t1)) {
      selected_columns_t1 <- c(present_common_columns_t1, input$cat_var)
      
      selected_data_t1 <- data_t1 %>%
        select(any_of(selected_columns_t1)) %>%
        drop_na()
      
      return(selected_data_t1)
    } else {
      return(data_t1 %>% select(any_of(present_common_columns_t1)) %>% drop_na())
    }
  })
  
  # Render the table
  output$T1 <- renderTable({
    data_t1 <- filtered_data_t1()
    
    # Generate descriptive statistics table with stratification
    if (!is.null(input$cat_var) && input$cat_var != "" && input$cat_var %in% colnames(data_t1)) {
      stratified_var <- input$cat_var
      table1(~ . | data_t1[[stratified_var]], data = data_t1, render.continuous=c(.="Mean (SD)"))
    } else {
      table1(~ ., data = data_t1, render.continuous=c(.="Mean (SD)"))
    }
  })
  
  
  # SERVER Tab 3: PLOTS
  
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
            geom_bar(fill = "skyblue",  color = "black") +
            labs(title = "Distribution of Diagnosis Age",
                 x = "Age", y = "Count")
        },
        "sex" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', insidetextorientation = 'radial',
                  marker = list(colors = c('pink', 'skyblue'))) %>%
            layout(title = list(text = "Male/Female Diagnosed Ratio", font = list(size = 20)),
                   font = list(size = 18),
                   margin = list(t = 80))
          
          
        },
        "race" = {
          race_counts <- filt_data %>% count(`Race Category`)
          
          # Define a palette of colors for the pie chart
          colors <- c('skyblue', '#fcad65', '#b7999c', '#cbc1b4', '#d62728', '#2ca02c', '#9467bd', '#e377c2', '#bcbd22', '#17becf')
          
          plot_ly(race_counts, labels = ~`Race Category`, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', textposition = 'outside', 
                  hoverinfo = 'label+value',
                  marker = list(colors = colors, 
                                line = list(color = '#000000', width = 1)),
                  textfont = list(color = colors)) %>%
            layout(title = list(text = 'Race Category Diagnosed Ratio', font = list(size = 20)),
                   showlegend = TRUE, 
                   legend = list(x = 1, y = 0.5),
                   margin = list(l = 20, r = 20, t = 80, b = 20),
                   font = list(size = 15),
                   uniformtext = list(minsize = 10, mode = 'show'))
          
          
        },
        "ethnicity" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          
          colors <- c('#e5c787', '#fcad65', '#b7999c', '#cbc1b4', '#d62728', '#2ca02c', '#9467bd', '#e377c2', '#bcbd22', '#17becf')
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie',
                  textinfo = 'label+percent', textposition = 'outside', 
                  hoverinfo = 'label+value',
                  marker = list(colors = colors, 
                                line = list(color = '#000000', width = 1)),
                  textfont = list(color = colors)) %>%
            layout(title = list(text = 'Ethnicity Category Diagnosed Ratio', font = list(size = 20)),
                   showlegend = TRUE, 
                   legend = list(x = 1, y = 0.5),
                   margin = list(l = 20, r = 20, t = 80, b = 50),
                   font = list(size = 15),
                   uniformtext = list(minsize = 10, mode = 'show'))
        }
      )
    } else if (input$plot_data_choice_tabular == "nsclc_data2") {
      plot <- switch(
        input$plot_choice_tabular2,
        "age2" = {
          ggplot(filt_data, aes(x = age)) +
            geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
            labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count")
        },
        "sex2" = {
          sex_counts <- filt_data %>% count(gender)
          plot_ly(sex_counts, labels = ~gender, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', insidetextorientation = 'radial',
                  marker = list(colors = c('pink', 'skyblue'))) %>%
            layout(title = list(text = "Male/Female Diagnosed Ratio", font = list(size = 20)),
                   font = list(size = 18),
                   margin = list(t = 80))
        }
      )
    } else if (input$plot_data_choice_tabular == "luad_data3") {
      plot <- switch(
        input$plot_choice_tabular3,
        "age3" = {
          ggplot(filt_data, aes(x = `Diagnosis Age`)) +
            geom_bar(fill = "skyblue",  color = "black") +
            labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count")
        },
        "sex3" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', insidetextorientation = 'radial',
                  marker = list(colors = c('pink', 'skyblue'))) %>%
            layout(title = list(text = "Male/Female Diagnosed Ratio", font = list(size = 20)),
                   font = list(size = 18),
                   margin = list(t = 80))
        },
        "race3" = {
          race_counts <- filt_data %>% count(`Race Category`)
          
          # Define a palette of colors for the pie chart
          colors <- c('skyblue', '#fcad65', '#b7999c', '#cbc1b4', '#d62728', '#2ca02c', '#9467bd', '#e377c2', '#bcbd22', '#17becf')
          
          plot_ly(race_counts, labels = ~`Race Category`, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', textposition = 'outside', 
                  hoverinfo = 'label+value',
                  marker = list(colors = colors, 
                                line = list(color = '#000000', width = 1)),
                  textfont = list(color = colors)) %>%
            layout(title = list(text = 'Race Category Diagnosed Ratio', font = list(size = 20)),
                   showlegend = TRUE, 
                   legend = list(x = 1, y = 0.5),
                   margin = list(l = 20, r = 20, t = 80, b = 20),
                   font = list(size = 15),
                   uniformtext = list(minsize = 10, mode = 'show'))
        },
        "ethnicity3" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          
          colors <- c('#e5c787', '#fcad65', '#b7999c', '#cbc1b4', '#d62728', '#2ca02c', '#9467bd', '#e377c2', '#bcbd22', '#17becf')
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie',
                  textinfo = 'label+percent', textposition = 'outside', 
                  hoverinfo = 'label+value',
                  marker = list(colors = colors, 
                                line = list(color = '#000000', width = 1)),
                  textfont = list(color = colors)) %>%
            layout(title = list(text = 'Ethnicity Category Diagnosed Ratio', font = list(size = 20)),
                   showlegend = TRUE, 
                   legend = list(x = 1, y = 0.5),
                   margin = list(l = 20, r = 20, t = 80, b = 50),
                   font = list(size = 15),
                   uniformtext = list(minsize = 10, mode = 'show'))
        }
      )
    } else if (input$plot_data_choice_tabular == "luad_data4") {
      plot <- switch(
        input$plot_choice_tabular4,
        "age4" = {
          ggplot(filt_data, aes(x = Age)) +
            geom_bar(fill = "skyblue",  color = "black") +
            labs(title = "Distribution of Diagnosis Age", x = "Age", y = "Count")
        },
        "sex4" = {
          sex_counts <- filt_data %>% count(Sex)
          plot_ly(sex_counts, labels = ~Sex, values = ~n, type = 'pie', 
                  textinfo = 'label+percent', insidetextorientation = 'radial',
                  marker = list(colors = c('pink', 'skyblue'))) %>%
            layout(title = list(text = "Male/Female Diagnosed Ratio", font = list(size = 20)),
                   font = list(size = 18),
                   margin = list(t = 80))
        },
        "ethnicity4" = {
          ethnicity_counts <- filt_data %>% count(`Ethnicity Category`)
          
          colors <- c('#e5c787', '#fcad65', '#b7999c', '#cbc1b4', '#d62728', '#2ca02c', '#9467bd', '#e377c2', '#bcbd22', '#17becf')
          plot_ly(ethnicity_counts, labels = ~`Ethnicity Category`, values = ~n, type = 'pie',
                  textinfo = 'label+percent', textposition = 'outside', 
                  hoverinfo = 'label+value',
                  marker = list(colors = colors, 
                                line = list(color = '#000000', width = 1)),
                  textfont = list(color = colors)) %>%
            layout(title = list(text = 'Ethnicity Category Diagnosed Ratio', font = list(size = 20)),
                   showlegend = TRUE, 
                   legend = list(x = 1, y = 0.5),
                   margin = list(l = 20, r = 20, t = 80, b = 50),
                   font = list(size = 15),
                   uniformtext = list(minsize = 10, mode = 'show'))
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
  
  # SERVER Tab 4: CORRELATION MATRIX
  
  # (Diagnosis Age: Has a negative correlation with variables such as Overall Survival (Months) and Progress Free Survival (Months), indicating that older patients may have poorer outcomes
  
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
  
  
  
  
  # Define the reactive expression for selected columns
  selected_columns <- reactive({
    # Use switch to select the dataset based on user input
    switch(
      input$cor_matrix_choice,
      "luad_data1" = numeric_data()[, c("Diagnosis Age", "Mutation Count", "Overall Survival (Months)")],
      "nsclc_data2" = numeric_data()[, c("age", "Survival.time", "deadstatus.event")],
      "luad_data4" = numeric_data()[, c("Age", "Mutation Count", "Overall survival months", "Person Cigarette Smoking History Pack Year Value")]
    )
  })
  
  # Render the correlation plot
  output$corrPlot <- renderPlot({
    # Check if there are more than one numeric columns
    if (ncol(selected_columns()) > 1) {
      # Calculate the correlation matrix
      cor_matrix <- cor(selected_columns(), use = "complete.obs")
      # Set the margins for the plot
      par(mar = c(2, 2, 1, 1))
      # Plot the correlation matrix
      corrplot(cor_matrix, method = "color", tl.cex = 1.4,
               diag = FALSE,
               type = "upper",
               col = colorRampPalette(c("blue", "white", "red"))(100),
               number.cex = 0.7, tl.col = "black")
    } else {
      # Display a message if not enough numeric columns are present
      plot.new()
      text(0.5, 0.5, "Not enough numeric columns for correlation analysis.")
    }
  })
  
  
  
  # SERVER Tab 5: DRUG SENSITIVITY
  
  # Generate UI for selecting the first level
  output$level1_ui <- renderUI({
    req(input$dataset_choice, input$group_var)
    dataset <- get(datasets_drug[[input$dataset_choice]])
    levels <- unique(dataset[[input$group_var]])
    selectInput("level1", "Select First Level:", choices = levels)
  })
  
  # Generate UI for selecting the second level based on the first level
  output$level2_ui <- renderUI({
    req(input$dataset_choice, input$group_var, input$level1)
    dataset <- get(datasets_drug[[input$dataset_choice]])
    levels <- setdiff(unique(dataset[[input$group_var]]), input$level1)
    selectInput("level2", "Select Second Level:", choices = levels)
  })
  
  observeEvent(input$run_test, {
    req(input$dataset_choice, input$group_var, input$level1, input$level2)
    dataset <- get(datasets_drug[[input$dataset_choice]])
    # Filter the data based on the selected levels
    filtered_data <- dataset %>% 
      filter(get(input$group_var) %in% c(input$level1, input$level2))
    
    # Perform the t-test based on the selected grouping variable
    t_test_result <- t.test(IC50 ~ get(input$group_var), data = filtered_data)
    
    # Display the t-test result
    output$t_test_result <- renderPrint({
      t_test_result
    })
  })
  
  
  # SERVER Tab 6: RADIOGENOMIC ANALYSIS  
  
  # Load or define dataset
  data(clevelandSmall)
  
  # UI for selecting cell line
  output$cell_line_ui <- renderUI({
    if (input$output_choice %in% c("LQ_model", "survFracAfter2Units", "dose10PercentSurv", "areaUnderDoseRespCurve", "doseResponseCurve")) {
      selectInput("cell_line", "Select Cell Line:", choices = cellInfo(clevelandSmall)$sampleid)
    }
  })
  
  # Function to compute metrics based on the selected cell line
  computeMetrics <- reactive({
    req(input$cell_line)
    sensRaw <- sensitivityRaw(clevelandSmall)
    cell_index <- which(cellInfo(clevelandSmall)$sampleid == input$cell_line)
    selected_sensRaw <- sensRaw[cell_index, , ]
    radiationDoses <- selected_sensRaw[, 'Dose']
    survivalFractions <- selected_sensRaw[, 'Viability']
    LQmodel <- linearQuadraticModel(D = radiationDoses, SF = survivalFractions)
    list(
      LQmodel = LQmodel,
      survFracAfter2Units = computeSF2(pars = LQmodel),
      dose10PercentSurv = computeD10(pars = LQmodel),
      areaUnderDoseRespCurve = RadioGx::computeAUC(D = radiationDoses, pars = LQmodel, lower = 0, upper = 1)
    )
  })
  
  output$radioGx_data <- renderPrint({
    cat("Radiogenomic dataset containing metadata/annotations, molecular data and radiation response data.\n\n")
    clevelandSmall
  })
  
  output$radioGx_molecularFeatureData <- renderPrint({
    # Access the molecular feature data
    mProf <- molecularProfiles(clevelandSmall, 'rnaseq')
    selected_rows <- mProf[1:5, ]  # Select the first 5 rows
    knitr::kable(selected_rows)
  })
  
  output$radioGx_sensitivity <- renderPrint({
    cat("Shows radiation doses and associated survival data for each cell line.\n\n")
    knitr::kable(sensitivityRaw(clevelandSmall))
  })
  
  output$radioGx_LQ_model <- renderPrint({
    metrics <- computeMetrics()
    req(metrics)
    cat("Based on the attribute we can see if the model fit for this data is good, with x% of observed variance explained by the model.\n\n")
    metrics$LQmodel
  })
  
  output$radioGx_survFracAfter2Units <- renderPrint({
    metrics <- computeMetrics()
    req(metrics)
    cat("Surviving fraction after the cancer cells are exposed to radiation twice.\n\n")
    print(metrics$survFracAfter2Units)
  })
  
  output$radioGx_dose10PercentSurv <- renderPrint({
    metrics <- computeMetrics()
    req(metrics)
    cat("Needed number of units of radiation on average to be administered to result in 10% cell-line survival.\n\n")
    print(metrics$dose10PercentSurv)
  })
  
  output$radioGx_areaUnderDoseRespCurve <- renderPrint({
    metrics <- computeMetrics()
    req(metrics)
    cat("Cell kill proportion after 1 Gy radiation.\n\n")
    print(metrics$areaUnderDoseRespCurve)
  })
  
  output$radioGx_doseResponseCurve <- renderPlot({
    req(input$cell_line)
    # Dose-Response Curve
    doseResponseCurve(
      rSets = list(clevelandSmall),
      cellline = input$cell_line
    )
  })
  
  
  
  # SERVER Tab 7: IMAGES
  
  # Initialize reactiveVal to hold images
  images <- reactiveVal(NULL)
  
  # Observe the dataset_selector input
  observeEvent(input$dataset_selector, {
    selected_path <- datasets[[input$dataset_selector]]
    if (!is.null(selected_path)) {
      all_images <- scan_dicom_files(selected_path)
      images(all_images)  # Update the reactiveVal images with all_images
      updateSliderInput(session, "image_slider", min = 1, max = length(all_images), value = 1)
    }
  }, ignoreNULL = TRUE)
  
  # Render the image based on the slider input
  output$image_display <- renderImage({
    req(images())  # Ensure images() is not NULL before rendering
    list(src = images()[input$image_slider], contentType = "image/png")
  }, deleteFile = FALSE)
  
  
  
  # SERVER Tab 8: REPORT
  
  # Output for the report text file
  output$research_report <- renderText({
    text_content
  })
}




shinyApp(ui = ui, server = server)
