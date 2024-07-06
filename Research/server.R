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