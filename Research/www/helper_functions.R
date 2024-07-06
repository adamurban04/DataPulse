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