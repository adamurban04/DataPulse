library(shiny)  # Shiny App
library(shinythemes)
library(readr)
library(ggplot2)  # Data Visualization
library(dplyr) # Data Manipulation (%>%)
library(DT)    # for adding searchability to said Datatables
library(markdown)
library(jpeg)  # For handling JPG images
library(oro.dicom)
library(png)
library(plotly)

# Function to read and convert DICOM to PNG with error handling and data conversion
convert_dicom_to_png <- function(file_path, output_path) {
  if (!file.exists(file_path)) {
    stop("DICOM file not found: ", file_path)
  }
  
  dicom_data <- tryCatch({
    readDICOMFile(file_path)
  }, error = function(e) {
    stop("Error reading DICOM file: ", file_path, " - ", e$message)
  })
  
  img_data <- tryCatch({
    dicom_data$img
  }, error = function(e) {
    stop("Error extracting image data from DICOM file: ", file_path, " - ", e$message)
  })
  
  # Ensure the image data is a numeric matrix
  img_data <- tryCatch({
    img_matrix <- as.numeric(img_data)
    dim(img_matrix) <- dim(img_data)
    img_matrix
  }, error = function(e) {
    stop("Error converting image data to numeric matrix: ", file_path, " - ", e$message)
  })
  
  tryCatch({
    writePNG(img_data, output_path)
  }, error = function(e) {
    stop("Error writing PNG file: ", output_path, " - ", e$message)
  })
}

# Example usage with error handling
tryCatch({
  convert_dicom_to_png("C:/Users/jamco/Downloads/adamurban04 UPSTaRT main Research/www/0.000000-NA-82046", 
                       "C:/Users/jamco/Downloads/adamurban04 UPSTaRT main Research/www/imgout.png")
}, error = function(e) {
  print(e$message)
})

ui <- fluidPage(
  titlePanel("DICOM Image Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dicom_files", "Upload DICOM Files", multiple = TRUE, accept = ".dcm"),
      verbatimTextOutput("file_error")  # Output for error messages
    ),
    mainPanel(
      plotlyOutput("image_display"),
      actionButton("prev_image", "Previous"),
      actionButton("next_image", "Next")
    )
  )
)

server <- function(input, output, session) {
  images <- reactiveVal(list())
  current_image <- reactiveVal(1)
  
  observeEvent(input$dicom_files, {
    req(input$dicom_files)
    files <- input$dicom_files$datapath
    
    img_paths <- tryCatch({
      lapply(files, function(f) {
        output_path <- paste0(tempfile(), ".png")
        convert_dicom_to_png(f, output_path)
        output_path
      })
    }, error = function(e) {
      output$file_error <- renderText({ e$message })
      return(NULL)
    })
    
    if (!is.null(img_paths)) {
      images(img_paths)
      current_image(1)
    }
  })
  
  observeEvent(input$prev_image, {
    current_image(max(1, current_image() - 1))
  })
  
  observeEvent(input$next_image, {
    current_image(min(length(images()), current_image() + 1))
  })
  
  output$image_display <- renderPlotly({
    req(images())
    img <- images()[[current_image()]]
    plot_ly(z = readPNG(img), type = "heatmap", colorscale = "Gray")
  })
}

shinyApp(ui, server)
