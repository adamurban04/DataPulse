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
        div(class = "space-before-panel"),
        
        h3("Summary in R"),
        div(class = "summary-text-box", verbatimTextOutput("data_summary1")),
        div(class = "space-before-panel"),
        
        
        h3("Descriptive Statistics using table1 Package"),
        dashboardSidebar(
          selectInput('dataset_t1_choice', 'Select Dataset', choices = names(datasets_clinical)),
          uiOutput("variable_select") # Dynamic variable selection based on chosen dataset
        ),
        dashboardBody(
          box(
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(class = "container-box",
                div(class = "table1-output", tableOutput("T1"))
            )
          )
        )
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
        plotOutput("corrPlot", width = "1024", height = "1024px")
      ),
      
      
      
    )),
    
    # UI Tab 4: IMAGES
    tabPanel("IMAGES", fluidRow(
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
    
    # UI Tab 5: REPORT
    tabPanel("REPORT", mainPanel(includeMarkdown("www/Report.md")))
    
  )
)