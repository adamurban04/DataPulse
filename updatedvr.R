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


# Helper Function to read and convert DICOM to PNG with meaningful file names
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
    
    # Construct output path with meaningful file name
    output_path <- file.path(output_dir, paste0(file_name, ".png"))
    
    writePNG(img_data, output_path)
    
    # Return the output path
    return(output_path)
  } else {
    stop("Image data is not in the correct format")
  }
}

# Define datasets with titles and corresponding paths
datasets <- list(
  "09-24-2006-StudyID-NA-27873" = "www/0.000000-NA-20785",
  "09-18-2008-StudyID-NA-69331" = "www/0.000000-NA-82046",
  "01-01-2014-StudyID-NA-34270" = "www/1.000000-NA-28595",
  "01-01-2014-StudyID-NA-85095" = "www/1.000000-NA-61228")

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
                            height: 500px;
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
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset_selector", "Choose Dataset:", choices = names(datasets)),
          sliderInput("image_slider", "Image Number", min = 1, max = 1, value = 1, step = 1),
          # Adding headers here
          h4(HTML("<u>x.abc.jpg</u>")),
          h5(HTML("<i>x</i> for patient")),
          h5(HTML("<i>abc</i> for picture in sequence"))
        ),
        mainPanel(
          textOutput("image_title"),
          plotOutput("image_display")
        )
      )
    )),
    
    # UI Tab 5: REPORT
    tabPanel("REPORT", mainPanel(includeMarkdown("www/Report.md")))
    
  )
)



# Define server function
server <- function(input, output, session) {
  
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
  
  
  
  
  # Define the reactive expression for selected columns
  selected_columns <- reactive({
    # Use switch to select the dataset based on user input
    switch(
      input$cor_matrix_choice,
      "luad_data1" = numeric_data()[, c("Diagnosis Age", "Mutation Count", "Overall Survival (Months)")],
      "nsclc_data2" = numeric_data()[, c("age", "Survival.time")],
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
               title = "Correlation Matrix", mar = c(0, 0, 1, 0),
               col = colorRampPalette(c("blue", "white", "red"))(100),
               number.cex = 0.7, tl.col = "black")
    } else {
      # Display a message if not enough numeric columns are present
      plot.new()
      text(0.5, 0.5, "Not enough numeric columns for correlation analysis.")
    }
  })
  
  # SERVER Tab 4: IMAGES
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
  
  # SERVER Tab 5: REPORT
  
  # Output for the report text file
  output$research_report <- renderText({
    text_content
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)