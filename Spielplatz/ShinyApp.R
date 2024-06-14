# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)  # For progress bar

# Define UI
ui <- fluidPage(
  titlePanel("Leaflet Map with Function Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("function_select", "Select Function:",
                  choices = list("geolink_electaccess" = "geolink_electaccess")),
      uiOutput("parameters_ui"),
      checkboxInput("use_survey", "Use Survey Data", value = FALSE),
      uiOutput("survey_ui"),
      actionButton("run_button", "Run Function")
    ),
    mainPanel(
      # Progress bar
      progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE),
      verbatimTextOutput("function_comments"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Define UI for function parameters
  output$parameters_ui <- renderUI({
    if (input$function_select == "geolink_electaccess") {
      tagList(
        dateInput("start_date", "Start Date:", value = Sys.Date() - 30),
        dateInput("end_date", "End Date:", value = Sys.Date()),
        textInput("shp_fn", "Shapefile Location:", value = "", placeholder = "e.g., '/path/to/your/shapefile.shp'"),
        numericInput("grid_size", "Grid Size:", value = 1000)
      )
    }
  })

  # Define UI for survey options
  output$survey_ui <- renderUI({
    if (input$use_survey) {
      tagList(
        dateInput("survey_dt", "Survey Date:", value = Sys.Date()),
        textInput("survey_fn", "Survey Filename:", value = ""),
        numericInput("survey_lat", "Survey Latitude:", value = NA),
        numericInput("survey_lon", "Survey Longitude:", value = NA),
        numericInput("buffer_size", "Buffer Size:", value = NA),
        textInput("extract_fun", "Extract Function:", value = "mean"),
        numericInput("survey_crs", "Survey CRS:", value = 4326)
      )
    }
  })

  # Placeholder function (should contain actual implementation)
  geolink_electaccess <- function(
    start_date = NULL,
    end_date = NULL,
    shp_fn = NULL,
    grid_size = 1000,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326) {

    # Update progress
    incProgress(10, message = "Loading shapefile...")

    # Ensure the shapefile path is correctly formatted
    shp_fn <- gsub("^['\"]|['\"]$", "", shp_fn)

    # Load the shapefile using the provided path (shp_fn)
    if (!is.null(shp_fn) && file.exists(shp_fn)) {
      shp_data <- st_read(shp_fn)
    } else {
      stop("Shapefile not found or path is incorrect")
    }

    # Debug: Print the first few rows of the shapefile data
    print(head(shp_data))

    # Update progress
    incProgress(40, message = "Processing data...")

    # Process the shapefile data (example processing)
    processed_data <- shp_data

    # Simulate processing time
    Sys.sleep(1)
    incProgress(50, message = "Finalizing data...")

    return(processed_data)
  }

  # Reactive function to generate the map based on selected function
  generate_map <- reactive({
    req(input$run_button) # Ensure the button is clicked

    # Run the function with a progress bar
    withProgress(message = "Running function...", value = 0, {
      isolate({
        result <- geolink_electaccess(
          start_date = input$start_date,
          end_date = input$end_date,
          shp_fn = input$shp_fn,
          grid_size = input$grid_size,
          survey_dt = input$survey_dt,
          survey_fn = input$survey_fn,
          survey_lat = input$survey_lat,
          survey_lon = input$survey_lon,
          buffer_size = input$buffer_size,
          extract_fun = input$extract_fun,
          survey_crs = input$survey_crs
        )
        return(result)
      })
    })
  })

  output$map <- renderLeaflet({
    data <- generate_map()

    # Convert sf object to data frame
    data_df <- as.data.frame(data)

    # Debug: Check the class of data_df
    print(class(data_df))

    # Debug: Check if the data contains the expected columns
    if (any(grepl("^lightscore_", names(data_df)))) {
      print("Columns with prefix 'lightscore_' found in the data")
    } else {
      print("No columns with prefix 'lightscore_' found in the data")
    }

    # Extract columns starting with 'lightscore'
    lightscore_columns <- data_df %>% dplyr::select(starts_with("lightscore_"))

    # Debug: Print the first few rows of the lightscore columns
    print(head(lightscore_columns))

    # Create popup content for each feature
    popups <- apply(lightscore_columns, 1, function(row) {
      paste(names(row), ": ", row, collapse = "<br>")
    })

    # Debug: Print a few popups to check formatting
    print(head(popups))

    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        color = "blue",
        weight = 1,
        popup = popups
      )
  })

  # Display function comments
  output$function_comments <- renderText({
    generate_map()
    capture.output(print("Function executed successfully"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
