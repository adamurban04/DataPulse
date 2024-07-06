source("global.R") # Global Dependencies (packages)
source("data_loading.R") # Loading Datasets
source("helper_functions.R") # Helper Functions for handling DICOM files
source("ui.R") # User Interface
source("server.R") # Server function

shinyApp(ui = ui, server = server)