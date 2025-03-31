# ONCOEX SHINY APP


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
library(RadioGx)
library(PharmacoGx)




# Load the dataset for hip fracture
hip_fracture_data <- read.csv("hip_fracture.csv")

# Subset for filtering columns for hip fracture
filter_columns <- c("gender", "age", "LOSdays", "marital_status", "ethnicity", "admit_type")
filtered_data_initial <- hip_fracture_data[filter_columns]

# Read the TSV/CSV file for Datasets


# https://www.cancerrxgene.org/downloads/drug_data?screening_set=GDSC1&tissue=LUAD
drug_data <- read_csv("www/LUAD_IC_Sun_Jul_14_11_36_44_2024.csv", show_col_types = FALSE)
# https://www.cancerrxgene.org/downloads/drug_data?screening_set=GDSC2&tissue=LUAD
drug_data2 <- read_csv("www/LUAD_IC_Thu_Jul_18_13_08_48_2024.csv", show_col_types = FALSE)


# List of drug datasets
datasets_drug <- list(
  "GDSC1" = "drug_data",
  "GDSC2" = "drug_data2"
)


# https://www.cbioportal.org/study/clinicalData?id=luad_tcga_pan_can_atlas_2018
luad_data1 <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv", show_col_types = FALSE)
# https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
nsclc_data2 <- read_csv("www/manifest1603198545583/NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019.csv", show_col_types = FALSE)
# https://www.cbioportal.org/study/clinicalData?id=luad_tcga
luad_data3 <- read_tsv("www/luad_tcga_firehose_legacy_clinical_data.tsv", show_col_types = FALSE)
# https://www.cbioportal.org/study/clinicalData?id=luad_oncosg_2020
luad_data4 <- read_tsv("www/luad_oncosg_2020_clinical_data.tsv", show_col_types = FALSE)


# CHANGE TO:


# CLINICAL TABULAR DATA
# https://www.cbioportal.org/study/clinicalData?id=nsclc_tcga_broad_2016














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
  
  tags$head(tags$style(
    HTML("
      .scrollable-table { overflow-x: auto; white-space: nowrap; width: 100%; }
      .scrollable-table table { width: 100%; }
      .summary-text-box { width: 100%; height: 350px; overflow-y: scroll; 
                         white-space: pre-wrap; word-wrap: break-word; 
                         border: 1px solid #ccc; padding: 20px; background-color: #f9f9f9; }
      .compact-container { margin: 0; padding: 10px; }
      .compact-select { margin-bottom: 10px; }
      .image-box { width: 512px; h  eight: 512px; }
      .plot-container { margin-top: 20px; margin-bottom: 20px; padding: 10px; 
                       border: 1px solid #ddd; border-radius: 5px; }
      .image-container { display: flex; justify-content: center; align-items: center; 
                        width: 100%; height: auto; }
      .image-container img { max-width: 100%; height: auto; }
      .space-before-panel { margin-top: 50px; }
      .container-box { background-color: #f9f9f9; padding: 20px; border-radius: 10px; 
                      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); }
      .table1-output { width: 100%; overflow-x: auto; }
    ")
  )),
  
  navbarPage(
    "DataPulse",
    tabPanel(
      "UPLOAD DATA",
      fluidRow(
        h3("Upload Your Own Data", style = "background-color: #f0f0f0; color: #333; 
           padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        column(
          12,
          fileInput("file_upload", "Choose File(s)", 
                    multiple = TRUE,
                    accept = c(".csv", ".tsv", ".xls", ".xlsx", ".json")),
          selectInput("upload_file_type", "File Type:",
                      choices = c("Auto-detect" = "auto",
                                  "CSV" = "csv", 
                                  "TSV" = "tsv",
                                  "Excel" = "excel",
                                  "JSON" = "json")),
          checkboxInput("header", "Header row", TRUE),
          actionButton("load_uploaded", "Load Data", class = "btn-primary"),
          hr(),
          h4("Uploaded Datasets"),
          DTOutput("uploaded_datasets_table")
        )
      )
    ),
    
    # DATA OVERVIEW tab
    tabPanel(
      "DATA OVERVIEW",
      fluidRow(
        h3("Data Overview", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
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
          
          conditionalPanel(
            condition = "input.data_choice == 'tabular'",
            selectInput(
              inputId = "data_choice_tabular",
              label = "Choose Dataset to view:",
              choices = list(
                "LUAD TCGA Pan Can Atlas 2018" = "luad_data1",
                "NSCLC-Radiomics Lung1" = "nsclc_data2",
                "LUAD TCGA Firehose Legacy" = "luad_data3",
                "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4",
                "GDSC1" = "drug_data",
                "GDSC2" = "drug_data2"
              )
            )
          ),
          conditionalPanel(
            condition = "input.data_choice == 'image'",
            selectInput(
              inputId = "data_choice_image",
              label = "Choose Dataset:",
              choices = list("NSCLC-Radiomics Lung1" = "nsclc_data2")
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
      # Tabular DRUG 5
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'drug_data'",
        h1("GDSC1 Drug Data"),
        div(class = "scrollable-table", DTOutput("data_dsout5")),
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary5"))
      ),
      # Tabular DRUG 6
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular == 'drug_data2'",
        h1("GDSC2 Drug Data"),
        div(class = "scrollable-table", DTOutput("data_dsout6")),
        div(class = "space-before-panel"),
        
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary6"))
      ),
      # Image NSCLC
      conditionalPanel(
        condition = "input.data_choice == 'image' && input.data_choice_image == 'nsclc_data2'",
        div(
          style = "border: 2px solid #606060; border-radius: 5px; padding: 15px; background-color: #f9f9f9; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
          h3("NSCLC Image Data", style = "color: #4f4f4f;"),
          h4("Dimensions: 512x512"),
          h4("Format: DICOM"),
          h4("Number of samples: 422"),
          h4("Number of Images: 52,073"),
          h4("Modality: CT/RT")
        )
      ),
      conditionalPanel(
        condition = "input.data_choice == 'tabular' && input.data_choice_tabular.startsWith('uploaded_')",
        h1("Uploaded Dataset"),
        div(class = "scrollable-table", DTOutput("uploaded_data_display")),
        div(class = "space-before-panel"),
        h3("Summary in R", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        div(class = "summary-text-box", verbatimTextOutput("uploaded_data_summary"))
      )
      
      
    ),
    
    # UI Tab 2: STRATIFIED ANALYSIS
    
    tabPanel(
      "STRATIFIED ANALYSIS",
      fluidRow(
        h3("Descriptive Statistics with table1 Package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        column(
          12,
          selectInput('dataset_t1_choice', 'Select Dataset', 
                      choices = c(names(datasets_clinical))),
          uiOutput("variable_select"),
          conditionalPanel(
            condition = "input.dataset_t1_choice.startsWith('uploaded_')",
            uiOutput("uploaded_var_selector_t1")
          ),
          div(style = "border-color: #C0C0C0; border-width: 1px; border-style: solid; padding: 15px;",
              h3(""),
              div(class = "table1-output", tableOutput("T1"))
          )
        )
      )),
    
    
    tabPanel(
      "PLOTS",
      fluidRow(
        h3("Data Visualization with plotly Package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        column(
          12,
          class = "compact-container",
          div(
            class = "compact-select",
            radioButtons(
              inputId = "plot_data_choice_tabular",
              label = "Choose Dataset:",
              choices = c(
                "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
                "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2",
                "LUAD TCGA Firehose Legacy Clinical Data" = "luad_data3",
                "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4",
                "Uploaded Data" = "uploaded_data"
              )
            ),
            
            # Dynamic UI for built-in datasets
            conditionalPanel(
              condition = "input.plot_data_choice_tabular != 'uploaded_data'",
              uiOutput("builtin_var_selector"),
              selectInput(
                "plot_type_tabular",
                "Plot Type:",
                choices = c("Histogram", "Boxplot", "Scatter", "Bar Chart")
              ),
              conditionalPanel(
                condition = "input.plot_type_tabular == 'Scatter'",
                uiOutput("scatter_yvar_selector")
              )
            ),
            
            # Dynamic UI for uploaded data
            conditionalPanel(
              condition = "input.plot_data_choice_tabular == 'uploaded_data'",
              selectInput(
                "uploaded_data_plot", 
                "Select Uploaded Dataset:",
                choices = NULL
              ),
              uiOutput("uploaded_var_selector"),
              selectInput(
                "uploaded_plot_type",
                "Plot Type:",
                choices = c("Histogram", "Boxplot", "Scatter", "Bar Chart")
              ),
              conditionalPanel(
                condition = "input.uploaded_plot_type == 'Scatter'",
                uiOutput("uploaded_scatter_yvar_selector")
              )
            ),
            
            # Main plot output
            plotlyOutput(outputId = "dynamic_plot", height = "600px")
          )
        )
      )
    ),
    
    
    
    
    # CORRELATION MATRIX tab - add option for uploaded data
    tabPanel(
      "CORRELATION MATRIX", 
      fluidRow(
        h3("Correlation Matrix with corrplot Package", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        column(
          12,
          class = "compact-container",
          div(
            class = "compact-select",
            radioButtons(
              inputId = "cor_matrix_choice",
              label = "Choose Dataset:",
              choices = c(
                "LUAD TCGA Pan Can Atlas 2018 Clinical Data" = "luad_data1",
                "NSCLC-Radiomics Lung1 Clinical Data" = "nsclc_data2", 
                "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4",
                "Uploaded Data" = "uploaded_data"
              )
            ),
            conditionalPanel(
              condition = "input.cor_matrix_choice == 'uploaded_data'",
              selectInput(
                "uploaded_cor_data",
                "Select Uploaded Dataset:",
                choices = NULL
              ),
              sliderInput(
                "cor_threshold",
                "Minimum Correlation Threshold:",
                min = 0, max = 1, value = 0.3, step = 0.1
              )
            )
          ),
          plotOutput("corrPlot", width = "800px", height = "800px")
        )
      )
    ),
    
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
    tabPanel("RADIOGX ANALYSIS", fluidRow(
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
                        "Dose-Response Curve" = "doseResponseCurve",
                        "Radiation Sensitivity Signatures" = "radiationSensitivitySignatures"
                      )
          ),
          uiOutput("cell_line_ui")
        ),
        mainPanel(
          lapply(c("data", "molecularFeatureData", "sensitivity", "LQ_model", "survFracAfter2Units", 
                   "dose10PercentSurv", "areaUnderDoseRespCurve", "doseResponseCurve", "radiationSensitivitySignatures"), function(choice) {
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
    
    # UI Tab 7: PHARMACOGENOMIC ANALYSIS
    tabPanel("PHARMACOGX ANALYSIS", fluidRow(
      h3("Pharmacogenomic Analysis with PharmacoGx", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      sidebarLayout(
        sidebarPanel(
          selectInput("pharmaco_output_choice", "Select Output to Display:", 
                      choices = list(
                        "Show Pharmacogenomic Data" = "pharmacoData",
                        "Drug Dose Response Curve" = "doseResponseCurve",
                        "Sensitivity Signatures" = "sensitivitySignatures"
                      )
          ),
          uiOutput("pharmaco_cell_line_ui"),
          uiOutput("pharmaco_drug_ui")
        ),
        mainPanel(
          lapply(c("pharmacoData", "doseResponseCurve", "sensitivitySignatures"), function(choice) {
            conditionalPanel(
              condition = paste0("input.pharmaco_output_choice == '", choice, "'"),
              if (choice == "doseResponseCurve") {
                plotOutput(paste0("pharmacoGx_", choice))
              } else {
                verbatimTextOutput(paste0("pharmacoGx_", choice))
              }
            )
          })
        )
      )
    )),
    
    # UI Tab 8: IMAGES
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
    #,
    # UI Tab 9: REPORT
    #tabPanel("REPORT", 
    #h3("Research Report", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
    #mainPanel(includeMarkdown("www/Report.md")))
    
    tabPanel("HIP FRACTURE ANALYSIS", 
    h3("MIMIC III - Hip Fracture Analysis", style = "background-color: #f0f0f0; color: #333; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        uiOutput("filters_ui"),
        hr(),
        h4("Graph Settings"),
        selectInput("x_var", "X-Axis Variable", choices = filter_columns),
        selectInput("y_var", "Y-Axis Variable", choices = c("None", filter_columns)),
        selectInput("plot_type", "Plot Type", choices = c("Bar Plot", "Scatter Plot", "Box Plot", "Histogram"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Filtered Data", DTOutput("filtered_table")),
          tabPanel("Visualization", plotOutput("dynamic_plot"))
        )
      )
    )
    
  )
))


# Define server function
server <- function(input, output, session) {
  
  # Reactive value to store uploaded datasets
  shared_data <- reactiveValues(
    uploaded = list(),
    uploaded_names = character(0),
    combined = list(
      clinical = NULL,
      drug = NULL,
      image = NULL
    )
  )
  
  # Observer for uploaded data
  observeEvent(input$load_uploaded, {
    req(input$file_upload)
    
    tryCatch({
      files <- input$file_upload
      for (i in seq_along(files$datapath)) {
        df_name <- tools::file_path_sans_ext(files$name[i])
        
        # Determine file type
        file_type <- if (input$upload_file_type == "auto") {
          tolower(tools::file_ext(files$name[i]))
        } else {
          input$upload_file_type
        }
        
        # Read data based on file type
        data <- switch(file_type,
                       "csv" = read_csv(files$datapath[i], show_col_types = FALSE, col_names = input$header),
                       "tsv" = read_tsv(files$datapath[i], show_col_types = FALSE, col_names = input$header),
                       "xls" = readxl::read_excel(files$datapath[i], col_names = input$header),
                       "xlsx" = readxl::read_excel(files$datapath[i], col_names = input$header),
                       "json" = jsonlite::fromJSON(files$datapath[i]),
                       read.csv(files$datapath[i], header = input$header)
        )
        
        # Store with uploaded_ prefix
        uploaded_name <- paste0("uploaded_", df_name)
        shared_data$uploaded[[uploaded_name]] <- data
        shared_data$uploaded_names <- c(shared_data$uploaded_names, df_name)
        
        # Classify data type
        if (any(grepl("IC50|AUC|drug", names(data), ignore.case = TRUE))) {
          shared_data$combined$drug <- if (is.null(shared_data$combined$drug)) {
            data
          } else {
            bind_rows(shared_data$combined$drug, data)
          }
        } else if (any(grepl("patient|diagnosis|clinical", names(data), ignore.case = TRUE))) {
          shared_data$combined$clinical <- if (is.null(shared_data$combined$clinical)) {
            data
          } else {
            bind_rows(shared_data$combined$clinical, data)
          }
        }
      }
      
      # Update all select inputs
      updateSelectInput(session, "data_choice_tabular", 
                        choices = c(
                          "LUAD TCGA Pan Can Atlas 2018" = "luad_data1",
                          "NSCLC-Radiomics Lung1" = "nsclc_data2",
                          "LUAD TCGA Firehose Legacy" = "luad_data3",
                          "LUAD (OncoSG, Nat Genet 2020)" = "luad_data4",
                          "GDSC1" = "drug_data",
                          "GDSC2" = "drug_data2",
                          setNames(paste0("uploaded_", shared_data$uploaded_names), 
                                   shared_data$uploaded_names)
                        ))
      
      updateSelectInput(session, "dataset_t1_choice", 
                        choices = c(
                          names(datasets_clinical),
                          setNames(paste0("uploaded_", shared_data$uploaded_names), 
                                   shared_data$uploaded_names)
                        ))
      
      showNotification("Data uploaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Display uploaded datasets table
  # Server logic for uploaded datasets

  output$uploaded_data_display <- renderDT({

    req(input$data_choice_tabular)

    

    # Get the dataset name from the selected choice

    selected_data_name <- input$data_choice_tabular

    

    # If the selected dataset starts with "uploaded_", get the dataset from shared_data

    if (startsWith(selected_data_name, "uploaded_")) {

      data <- shared_data$uploaded[[selected_data_name]]

      

      datatable(data, options = list(scrollX = TRUE, lengthMenu = list(

        c(10, 25, 50, -1), c('10', '25', '50', 'All')

      )))

    }

  }, server = TRUE)

  

  # Summary for the uploaded dataset

  output$uploaded_data_summary <- renderPrint({

    req(input$data_choice_tabular)

    

    # Get the dataset name from the selected choice

    selected_data_name <- input$data_choice_tabular

    

    # If the selected dataset starts with "uploaded_", get the dataset from shared_data

    if (startsWith(selected_data_name, "uploaded_")) {

      data <- shared_data$uploaded[[selected_data_name]]

      summary(data)

    }

  })
  
  output$uploaded_datasets_table <- renderDT({
    
    req(length(shared_data$uploaded) > 0)
    
    data.frame(
      
      Dataset = shared_data$uploaded_names,
      
      Rows = sapply(shared_data$uploaded, nrow),
      
      Columns = sapply(shared_data$uploaded, ncol),
      
      Type = sapply(shared_data$uploaded, function(x) {
        
        if (any(grepl("IC50|AUC|drug", names(x), ignore.case = TRUE))) "Drug Sensitivity" else
          
          if (any(grepl("patient|diagnosis|clinical", names(x), ignore.case = TRUE))) "Clinical" else
            
            "Other"
        
      }),
      
      stringsAsFactors = FALSE
      
    )
    
  }, options = list(pageLength = 5, scrollX = TRUE))

  
  
  
  
  
  # List of data frames
  data_list <- list(luad_data1, nsclc_data2, luad_data3, luad_data4, drug_data, drug_data2)
  
  # Names for the outputs
  output_names <- c("data_dsout1", "data_dsout2", "data_dsout3", "data_dsout4", "data_dsout5", "data_dsout6")
  summary_names <- c("data_summary1", "data_summary2", "data_summary3", "data_summary4", "data_summary5", "data_summary6")
  
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
  
  # Reactive expression to get selected dataset (preset or uploaded)
  selected_data_t1 <- reactive({
    if (startsWith(input$dataset_t1_choice, "uploaded_")) {
      # Handle uploaded datasets
      shared_data$uploaded[[input$dataset_t1_choice]]
    } else {
      # Handle preset datasets
      datasets_clinical[[input$dataset_t1_choice]]
    }
  })
  
  
  # Dynamic UI for selecting variable based on selected dataset
  output$variable_select <- renderUI({
    req(selected_data_t1())
    
    # Get column names from the selected dataset
    column_names <- names(selected_data_t1())
    
    # Filter out 'PatientID' and 'Patient ID' from the list of variables
    cat_vars <- setdiff(column_names, c("PatientID", "Patient ID", "Sample ID"))
    selectInput('cat_var', 'Variable', choices = cat_vars)
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
  
  # Render the table for stratified analysis
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
  # PLOTS tab server logic
  
  # Update uploaded data choices
  observe({
    updateSelectInput(session, "uploaded_data_plot", 
                      choices = shared_data$uploaded_names)
  })
  
  # Variable selector for built-in datasets
  output$builtin_var_selector <- renderUI({
    req(input$plot_data_choice_tabular)
    
    data <- switch(input$plot_data_choice_tabular,
                   "luad_data1" = luad_data1,
                   "nsclc_data2" = nsclc_data2,
                   "luad_data3" = luad_data3,
                   "luad_data4" = luad_data4)
    
    selectInput(
      "plot_choice_tabular_dynamic",
      "Choose Variable:",
      choices = names(data)
    )
  })
  
  # Y variable selector for scatter plots (built-in)
  output$scatter_yvar_selector <- renderUI({
    req(input$plot_data_choice_tabular, input$plot_choice_tabular_dynamic)
    
    data <- switch(input$plot_data_choice_tabular,
                   "luad_data1" = luad_data1,
                   "nsclc_data2" = nsclc_data2,
                   "luad_data3" = luad_data3,
                   "luad_data4" = luad_data4)
    
    available_vars <- setdiff(names(data), input$plot_choice_tabular_dynamic)
    
    selectInput(
      "scatter_yvar",
      "Select Y Variable:",
      choices = available_vars
    )
  })
  
  # Variable selector for uploaded data
  output$uploaded_var_selector <- renderUI({
    req(input$uploaded_data_plot)
    
    data <- shared_data$uploaded[[paste0("uploaded_", input$uploaded_data_plot)]]
    selectInput(
      "uploaded_plot_var",
      "Select Variable:",
      choices = names(data)
    )
  })
  
  # Y variable selector for scatter plots (uploaded)
  output$uploaded_scatter_yvar_selector <- renderUI({
    req(input$uploaded_data_plot, input$uploaded_plot_var)
    
    data <- shared_data$uploaded[[paste0("uploaded_", input$uploaded_data_plot)]]
    available_vars <- setdiff(names(data), input$uploaded_plot_var)
    
    selectInput(
      "uploaded_scatter_yvar",
      "Select Y Variable:",
      choices = available_vars
    )
  })
  
  # Main plot rendering function
  output$dynamic_plot <- renderPlotly({
    # Get the appropriate dataset
    data <- if (input$plot_data_choice_tabular == "uploaded_data") {
      req(input$uploaded_data_plot, input$uploaded_plot_var)
      shared_data$uploaded[[paste0("uploaded_", input$uploaded_data_plot)]]
    } else {
      switch(input$plot_data_choice_tabular,
             "luad_data1" = luad_data1,
             "nsclc_data2" = nsclc_data2,
             "luad_data3" = luad_data3,
             "luad_data4" = luad_data4)
    }
    
    # Get plot parameters
    if (input$plot_data_choice_tabular == "uploaded_data") {
      xvar <- input$uploaded_plot_var
      plot_type <- input$uploaded_plot_type
      yvar <- if (plot_type == "Scatter") input$uploaded_scatter_yvar else NULL
    } else {
      xvar <- input$plot_choice_tabular_dynamic
      plot_type <- input$plot_type_tabular
      yvar <- if (plot_type == "Scatter") input$scatter_yvar else NULL
    }
    
    # Filter NA values for relevant variables
    data <- data %>% filter(!is.na(get(xvar)))
    if (!is.null(yvar)) {
      data <- data %>% filter(!is.na(get(yvar)))
    }
    
    # Create plot based on type
    p <- switch(plot_type,
                "Histogram" = {
                  plot_ly(data, x = ~get(xvar), type = "histogram",
                          marker = list(color = '#4E79A7')) %>%
                    layout(xaxis = list(title = xvar),
                           yaxis = list(title = "Count"),
                           title = paste("Distribution of", xvar))
                },
                "Boxplot" = {
                  plot_ly(data, y = ~get(xvar), type = "box",
                          marker = list(color = '#E15759'),
                          line = list(color = '#E15759')) %>%
                    layout(yaxis = list(title = xvar),
                           title = paste("Boxplot of", xvar))
                },
                "Bar Chart" = {
                  counts <- data %>%
                    dplyr::count(!!sym(xvar)) %>%
                    as.data.frame() %>%  # Ensure counts is a data frame
                    dplyr::rename(Category = 1, Count = n)
                  
                  plot_ly(counts, x = ~Category, y = ~Count, type = "bar",
                          marker = list(color = '#59A14F')) %>%
                    layout(xaxis = list(title = xvar),
                           yaxis = list(title = "Count"),
                           title = paste("Frequency of", xvar))
                },
                
                "Scatter" = {
                  req(yvar)
                  plot_ly(data, 
                          x = ~get(xvar), 
                          y = ~get(yvar),
                          type = "scatter",
                          mode = "markers",
                          marker = list(size = 8, opacity = 0.7, color = '#76B7B2'),
                          text = ~paste(xvar, ": ", get(xvar), "<br>", yvar, ": ", get(yvar)),
                          hoverinfo = 'text') %>%
                    layout(xaxis = list(title = xvar),
                           yaxis = list(title = yvar),
                           title = paste(yvar, "vs", xvar))
                }
    )
    
    # Add some common styling
    p %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                 font = list(family = "Arial"))
  })
  # Server continuation for correlation matrix
  observe({
    updateSelectInput(
      session,
      "uploaded_cor_data",
      choices = shared_data$uploaded_names
    )
  })
  
  # Correlation matrix data preparation
  corr_data <- reactive({
    if (input$cor_matrix_choice == "uploaded_data") {
      req(input$uploaded_cor_data)
      dataset <- shared_data$uploaded[[paste0("uploaded_", input$uploaded_cor_data)]]
      
      # Select only numeric columns
      num_data <- dataset %>% select_if(is.numeric)
      
      # Remove columns with zero variance
      num_data[, apply(num_data, 2, function(x) var(x, na.rm = TRUE) > 0)]
    } else {
      # Original logic for built-in datasets
      switch(input$cor_matrix_choice,
             "luad_data1" = luad_data1 %>% select_if(is.numeric),
             "nsclc_data2" = nsclc_data2 %>% select_if(is.numeric),
             "luad_data4" = luad_data4 %>% select_if(is.numeric))
    }
  })
  
  # Correlation matrix plot
  output$corrPlot <- renderPlot({
    data <- corr_data()
    req(ncol(data) > 1)  # Need at least 2 columns for correlation
    
    # Calculate correlation matrix
    cor_mat <- cor(data, use = "complete.obs")
    
    # Apply threshold if uploaded data
    if (input$cor_matrix_choice == "uploaded_data") {
      cor_mat[abs(cor_mat) < input$cor_threshold] <- 0
    }
    
    # Create correlation plot
    corrplot(cor_mat,
             method = "color",
             type = "upper",
             tl.col = "black",
             tl.cex = 0.8,
             addCoef.col = "black",
             number.cex = 0.7,
             diag = FALSE,
             cl.ratio = 0.2,
             mar = c(0, 0, 1, 0),
             title = ifelse(input$cor_matrix_choice == "uploaded_data",
                            paste("Correlation Matrix for", input$uploaded_cor_data),
                            paste("Correlation Matrix for", input$cor_matrix_choice)))
  })
  
  
  # SERVER Tab 5: DRUG SENSITIVITY
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
  
  # SERVER Tab 6: RADIOGX ANALYSIS  
  
  # Load or define dataset
  data("clevelandSmall")
  
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
  
  # Render analysis results
  output$analysis_results <- renderPrint({
    if (input$output_choice %in% c("LQ_model", "survFracAfter2Units", "dose10PercentSurv", "areaUnderDoseRespCurve", "doseResponseCurve", "radiationSensitivitySignatures")) {
      metrics <- computeMetrics()
      req(metrics)
      if (input$output_choice == "LQ_model") {
        print(metrics$LQmodel)
      } else if (input$output_choice == "survFracAfter2Units") {
        print(metrics$survFracAfter2Units)
      } else if (input$output_choice == "dose10PercentSurv") {
        print(metrics$dose10PercentSurv)
      } else if (input$output_choice == "areaUnderDoseRespCurve") {
        print(metrics$areaUnderDoseRespCurve)
      } else if (input$output_choice == "radiationSensitivitySignatures") {
        print(metrics$radSensSig@.Data)
      }
    }
  })
  
  # Render dose response plot
  output$radioGx_doseResponseCurve <- renderPlot({
    req(input$cell_line)
    # Dose-Response Curve
    doseResponseCurve(
      rSets = list(clevelandSmall),
      cellline = input$cell_line
    )
  })
  
  output$radioGx_data <- renderPrint({
    cat("Radiogenomic dataset containing metadata/annotations, molecular data and radiation response data.\n\n")
    clevelandSmall
  })
  
  output$radioGx_molecularFeatureData <- renderPrint({
    cat("Shows first 10 rows of molecular feature data.\n\n")
    # Access the molecular feature data
    mProf <- molecularProfiles(clevelandSmall, 'rnaseq')
    selected_rows <- mProf[1:10, ]  # Select the first 10 rows
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
  
  output$radioGx_radiationSensitivitySignatures <- renderPrint({
    radSensSig <- radSensitivitySig(clevelandSmall, mDataType='rna', features=fNames(clevelandSmall, 'rna')[1:10], nthread=1)
    cat("Radiation sensitivity signatures (first 10 rows selected).\n\n
    
        Estimate: Positive values indicate that higher expression of the gene is associated with increased radio-sensitivity,\n
        while negative values indicate association with radio-resistance.\n\n
        
        Low p-values (typically < 0.05) and FDR values suggest that the association is statistically significant\n\n.
        
        T-Statistic and F-Statistic: Higher values indicate stronger evidence against the null hypothesis,\n
        implying a significant relationship between gene expression and radiation sensitivity.\n\n
        ")
    radSensSig@.Data
  })
  
  # SERVER Tab 7: PHARMACOGX ANALYSIS 
  
  # Load or define dataset
  data("CCLEsmall")
  
  # UI for selecting cell line
  output$pharmaco_cell_line_ui <- renderUI({
    if (input$pharmaco_output_choice %in% c("doseResponseCurve")) {
      selectInput("pharmaco_cell_line", "Select Cell Line:", choices = cellInfo(CCLEsmall)$sampleid)
    }
  })
  
  # UI for selecting drug
  output$pharmaco_drug_ui <- renderUI({
    if (input$pharmaco_output_choice %in% c("doseResponseCurve", "sensitivitySignatures")) {
      selectInput("pharmaco_drug", "Select Drug:", choices = drugInfo(CCLEsmall)$treatmentid)
    }
  })
  
  # Show a warning message when the "Drug Dose Response Curve" is selected
  observeEvent(input$pharmaco_output_choice, {
    if (input$pharmaco_output_choice == "doseResponseCurve") {
      showModal(modalDialog(
        title = "Warning",
        "Please note that many drug-cell line combinations have not been tested. You may encounter cases where the selected combination was not tested.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Function to compute drug sensitivity based on the selected drug
  computeDrugSensitivity <- reactive({
    req(input$pharmaco_drug)
    drug_response <- summarizeSensitivityProfiles(pSet = CCLEsmall, sensitivity.measure = "auc_published", summary.stat = "median")
    selected_drug_response <- drug_response[input$pharmaco_drug, ]
    selected_drug_response
  })
  
  # Render pharmacogenomic data
  output$pharmacoGx_pharmacoData <- renderPrint({
    cat("Pharmacogenomic dataset containing metadata/annotations, molecular data and drug response data.\n\n")
    CCLEsmall
  })
  
  # Render drug dose response curve
  output$pharmacoGx_doseResponseCurve <- renderPlot({
    req(input$pharmaco_cell_line, input$pharmaco_drug)
    drugDoseResponseCurve(
      pSet = CCLEsmall,
      drug = input$pharmaco_drug,
      cell = input$pharmaco_cell_line
    )
  })
  
  # Render sensitivity signatures
  output$pharmacoGx_sensitivitySignatures <- renderPrint({
    req(input$pharmaco_cell_line, input$pharmaco_drug)
    sensSig <- drugSensitivitySig(CCLEsmall, mDataType='rna', drug = input$pharmaco_drug, cellline = input$pharmaco_cell_line, features=fNames(CCLEsmall, 'rna')[1:10], nthread=1)
    cat("Drug sensitivity signatures (first 10 rows selected).\n\n
    
        Estimate: Positive values indicate that higher expression of the gene is associated with increased drug sensitivity,\n
        while negative values indicate association with drug resistance.\n\n
        
        Low p-values (typically < 0.05) and FDR values suggest that the association is statistically significant\n\n.
        
        T-Statistic and F-Statistic: Higher values indicate stronger evidence against the null hypothesis,\n
        implying a significant relationship between gene expression and drug sensitivity.\n\n
        ")
    sensSig@.Data
  })
  
  
  
  # SERVER Tab 8: IMAGES
  
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
  
  
  
  # SERVER Tab 9: REPORT
  
  # Output for the report text file
  #output$research_report <- renderText({
  # text_content
  #})
  # Dynamically create filters for specified columns
  output$filters_ui <- renderUI({
    lapply(filter_columns, function(col) {
      if (is.numeric(filtered_data_initial[[col]])) {
        sliderInput(
          paste0("filter_", col), col,
          min = min(filtered_data_initial[[col]], na.rm = TRUE),
          max = max(filtered_data_initial[[col]], na.rm = TRUE),
          value = c(min(filtered_data_initial[[col]], na.rm = TRUE), 
                    max(filtered_data_initial[[col]], na.rm = TRUE))
        )
      } else {
        checkboxGroupInput(
          paste0("filter_", col), col,
          choices = unique(filtered_data_initial[[col]]),
          selected = unique(filtered_data_initial[[col]])
        )
      }
    })
  })
  
  # Reactive filtering of the dataset
  filtered_data <- reactive({
    data <- hip_fracture_data
    for (col in filter_columns) {
      filter_input <- input[[paste0("filter_", col)]]
      if (!is.null(filter_input)) {
        if (is.numeric(data[[col]])) {
          data <- data %>% filter(data[[col]] >= filter_input[1] & data[[col]] <= filter_input[2])
        } else {
          data <- data %>% filter(data[[col]] %in% filter_input)
        }
      }
    }
    data
  })
  
  # Render filtered data table
  output$filtered_table <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  # Render dynamic plot
  output$dynamic_plot <- renderPlot({
    data <- filtered_data()
    x_var <- input$x_var
    y_var <- input$y_var
    
    if (input$plot_type == "Bar Plot") {
      ggplot(data, aes_string(x = x_var)) +
        geom_bar(fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = "Bar Plot", x = x_var, y = "Count")
    } else if (input$plot_type == "Scatter Plot" && y_var != "None") {
      ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_point(color = "blue") +
        theme_minimal() +
        labs(title = "Scatter Plot", x = x_var, y = y_var)
    } else if (input$plot_type == "Box Plot" && y_var != "None") {
      ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        theme_minimal() +
        labs(title = "Box Plot", x = x_var, y = y_var)
    } else if (input$plot_type == "Histogram" && is.numeric(filtered_data_initial[[x_var]])) {
      ggplot(data, aes_string(x = x_var)) +
        geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Histogram", x = x_var, y = "Frequency")
    } else {
      ggplot() +
        labs(title = "Select appropriate variables for the chosen plot type.")
    }
  })
}


shinyApp(ui = ui, server = server)
