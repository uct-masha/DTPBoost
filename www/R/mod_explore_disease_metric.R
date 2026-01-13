# Disease Metric Module
# This module creates a box/card for exploring disease and metric data with three sections:
# 1. Left: Plotly visualization of all known time series
# 2. Middle: Text description of data series and general information
# 3. Right: Custom data upload feature with template download

# Module UI Function
diseaseMetricUI <- function(id, disease, metric) {
  ns <- NS(id)
  
  box(
    title = paste(disease, "-", metric),
    width = 12,

    fluidRow(
      # Left Section: Plot
      column(
        width = 5,
        plotlyOutput(ns("time_series_plot"), height = "400px")
      ),
      
      # Middle Section: Description
      column(
        width = 4,
        h4("About the data", style = "margin-top: 0;"),
        uiOutput(ns("general_info"))
      ),
      
      # Right Section: Upload Feature
      column(
        width = 3,
        h4("Custom Data Upload", style = "margin-top: 0;"),
        wellPanel(
          style = "background-color: #f9f9f9;",
          
          h5(icon("info-circle"), "Instructions"),
          p("Upload your custom data as an Excel file with two columns:",
            style = "font-size: 12px;"),
          tags$ul(
            tags$li(strong("Year:"), "The year of observation"),
            tags$li(strong("Value:"), "The metric value"),
            style = "font-size: 12px;"
          ),
          
          hr(),
          
          # Template download
          downloadButton(
            ns("download_template"),
            "Download Template",
            class = "btn-info btn-sm btn-block",
            icon = icon("download")
          ),
          
          br(), br(),
          
          # File upload
          fileInput(
            ns("custom_data_file"),
            "Upload Custom Data",
            accept = c(".xlsx", ".xls"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          
          # Upload status
          uiOutput(ns("upload_status"))
        )
      )
    )
  )
}

# Module Server Function
diseaseMetricServer <- function(id, disease, metric, data_list = NULL, info = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store all data (original + custom)
    all_data <- reactiveVal(data_list)
    custom_data <- reactiveVal(NULL)
    
    # Render the time series plot
    output$time_series_plot <- renderPlotly({
      data <- all_data()
      # TODO: Merge all data series into a single dataframe with data source column
      # Make a ggplot2 object and convert to plotly with ggplotly()
      if (is.null(data) || length(data) == 0) {
        p <- ggplot() +
          theme_minimal() +
          labs(title = "No data available", x = "Year", y = metric)
        return(ggplotly(p))
      }
      
      plot_data <- do.call(bind_rows, lapply(names(data), function(name) {
          df <- data[[name]]
          df$DataSource <- name
          return(df)
      }))
      p <- ggplot(plot_data, aes(x = Year, y = Value, color = DataSource)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = paste(disease, "-", metric, "Over Time"),
           x = "Year",
           y = metric,
           color = "Data Source")
        ggplotly(p)
    })
    
    # Render general information
    output$general_info <- renderUI({
      gen_info <- if (is.reactive(info)) info() else info
      
      if (!is.null(gen_info)) {
        tags$div(
          style = "padding: 10px; background-color: #ffffff; border-radius: 4px;",
          tags$p(gen_info, style = "margin: 0; font-size: 13px;")
        )
      } else {
        tags$div(
          style = "padding: 10px; background-color: #ffffff; border-radius: 4px;",
          tags$p("No common data for this metric. Please upload custom data.", 
                 style = "margin: 0; font-size: 13px; color: gray;")
        )
      }
    })
    
    # Download template handler
    output$download_template <- downloadHandler(
      filename = function() {
        paste0("template_", disease, "_", metric, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Create a template dataframe
        template_df <- data.frame(
          Year = 2014:2024,
          Value = rep(NA_real_, 11)
        )
        
        # Write to Excel
        writexl::write_xlsx(template_df, file)
      }
    )
    
    # Handle custom data upload
    observeEvent(input$custom_data_file, {
      req(input$custom_data_file)
      
      tryCatch({
        # Read the uploaded file
        uploaded_df <- readxl::read_excel(input$custom_data_file$datapath)
        
        # Validate columns
        if (!all(c("Year", "Value") %in% names(uploaded_df))) {
          showNotification(
            "Error: Excel file must contain 'Year' and 'Value' columns",
            type = "error",
            duration = 5
          )
          return(NULL)
        }
        
        # Validate data types
        uploaded_df$Year <- as.integer(uploaded_df$Year)
        uploaded_df$Value <- as.numeric(uploaded_df$Value)
        
        # Remove rows with missing values
        uploaded_df <- uploaded_df[complete.cases(uploaded_df), ]
        
        if (nrow(uploaded_df) == 0) {
          showNotification(
            "Error: No valid data found in uploaded file",
            type = "error",
            duration = 5
          )
          return(NULL)
        }
        
        # Store custom data
        custom_data(uploaded_df)
        
        # Update all_data to include custom data
        current_data <- all_data()
        if (is.null(current_data)) {
          current_data <- list()
        }
        current_data[["Custom Data"]] <- uploaded_df
        all_data(current_data)
        
        showNotification(
          paste("Successfully uploaded", nrow(uploaded_df), "data points"),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Render upload status
    output$upload_status <- renderUI({
      if (!is.null(custom_data())) {
        tags$div(
          style = "padding: 8px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; margin-top: 10px;",
          tags$small(
            icon("check-circle", style = "color: #28a745;"),
            paste("Custom data loaded:", nrow(custom_data()), "points"),
            style = "color: #155724;"
          )
        )
      }
    })
    
    # Return reactive values for parent module to access if needed
    return(
      list(
        all_data = all_data,
        custom_data = custom_data
      )
    )
  })
}