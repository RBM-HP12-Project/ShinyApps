library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(readr)
library(zoo)
library(ggplot2)
library(dplyr)

# Sample data for 5 locations
locations <- data.frame(
  Name = c("Location A", "Location B", "Location C", "Location D", "Location E"),
  Latitude = c(3.692083, 3.402780, 3.544720, 3.485560, 2.992780),
  Longitude = c(101.3419, 101.4431, 101.6722, 101.5392, 101.7869)
)

# Function to generate synthetic streamflow data
generate_streamflow_data <- function(n = 24) {
  streamflow <- round(rgamma(n, shape = 2, scale = 100))
  dates <- seq(as.Date("1996-01-01"), by = "month", length.out = n)
  data.frame(Date = dates, Streamflow = streamflow)
}

# Generate data frames for each location
data_list <- list(
  "Location A" = generate_streamflow_data(360),  # 30 years of data
  "Location B" = generate_streamflow_data(360),
  "Location C" = generate_streamflow_data(360),
  "Location D" = generate_streamflow_data(360),
  "Location E" = generate_streamflow_data(360)
)

# Function to calculate and plot SDI
calculate_sdi <- function(df, aggregation, ref_date, year_range) {
  df <- df %>%
    mutate(Streamflow_3month = zoo::rollapply(Streamflow, width = 3, FUN = sum, fill = NA, align = "right"),
           Streamflow_6month = zoo::rollapply(Streamflow, width = 6, FUN = sum, fill = NA, align = "right")) %>%
    na.omit()
  
  # Choose the correct aggregation
  if (aggregation == "3-month") {
    mean_streamflow <- mean(df$Streamflow_3month)
    sd_streamflow <- sd(df$Streamflow_3month)
    df$SDI <- (df$Streamflow_3month - mean_streamflow) / sd_streamflow
  } else {
    mean_streamflow <- mean(df$Streamflow_6month)
    sd_streamflow <- sd(df$Streamflow_6month)
    df$SDI <- (df$Streamflow_6month - mean_streamflow) / sd_streamflow
  }
  
  # Filter data based on selected years
  df <- df %>%
    filter(format(Date, "%Y") >= year_range[1] & format(Date, "%Y") <= year_range[2])
  
  # Define x-axis limits
  xlim_values <- as.Date(c(paste0(year_range[1], "-01-01"), paste0(year_range[2], "-12-31")))
  
  # Plot SDI
  ggplot(df, aes(x = Date, y = SDI)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = as.Date(ref_date), linetype = "dashed", color = "green", size = 1) +
    labs(title = paste(aggregation, "Streamflow Drought Index Time Series"), x = "Date", y = "SDI") +
    xlim(xlim_values) +  # Apply user-selected xlim
    theme_minimal()
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "MYdrought Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map View", tabName = "map_tab", icon = icon("map")),
      menuItem("Upload Daily River Flow CSV file", tabName = "upload_tab", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Change the dashboard body background */
      .content-wrapper, .right-side {
        background-color: #FFE5B4 !important; /* Light Orange */
      }

      /* Change sidebar background */
      .skin-blue .main-sidebar, .skin-blue .left-side {
        background-color: #CD853F !important; /* Gold */
      }
      
      /* Change the top header background color */
      .skin-blue .main-header .logo {
        background-color: #CD853F !important; /* Gold */
      }

      /* Change the top header bar color */
      .skin-blue .main-header .navbar {
        background-color: #CD853F !important; /* Gold */
      }
      
      /* Change the sidebar menu background */
      .skin-blue .main-sidebar {
        background-color: #CD853F !important; /* Gold */
      }

      /* Change sidebar active item highlight */
      .skin-blue .sidebar-menu > li.active > a {
        background-color: #CD853F !important; /* Darker Gold */
      }

      /* Change box headers */
      .box.box-primary > .box-header {
        background-color: #CD853F !important; /* Gold */
        color: white !important; /* White text */
      }

      /* Change box background */
      .box {
        background-color: #FFF3E0 !important; /* Very Light Orange */
        border-radius: 10px;
        border-color: #CD853F !important; /* Darker Gold Border */
      }

      /* Change box title text color */
      .box-header {
        color: white !important; /* White text */
      }
    "))
    ),
    tabItems(
      tabItem(tabName = "map_tab",
              fluidRow(
                box(title = "Map", status = "primary", solidHeader = TRUE, width = 6,
                    leafletOutput("map", height = 400)
                ),
                box(title = "Streamflow Drought Index Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("sdi_plot", height = 400)
                )
              ),
              fluidRow(
                box(title = "Flow Value", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("table")
                )
              ),
              fluidRow(
                box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("aggregation", "Select Aggregation:", choices = c("3-month", "6-month")),
                    dateInput("ref_date", "Select Reference Date:", value = "2023-10-01"),
                    sliderInput("year_range", "Select Year Range:",
                                min = 1995, max = 2025, value = c(2023, 2025), step = 1, sep = ""),
                    actionButton("process", "Calculate the SDI")
                )
              )
      ),
      tabItem(tabName = "upload_tab",
              fluidRow(
                box(title = "Upload CSV", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file", "Upload CSV File"),
                    actionButton("process_upload", "Process Uploaded Data")
                ),
                box(title = "Uploaded Data SDI Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("upload_sdi_plot")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_data <- reactiveVal(NULL)
  
  # Render Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = locations,
        ~Longitude, ~Latitude,
        popup = ~Name,
        layerId = ~Name
      )
  })
  
  # Select Data on Map Click
  observeEvent(input$map_marker_click, {
    selected <- input$map_marker_click$id
    selected_data(data_list[[selected]])
  })
  
  # Display Table
  output$table <- renderDT({
    req(selected_data())
    datatable(selected_data())
  })
  
  # Process Data on Button Click
  observeEvent(input$process, {
    req(selected_data())
    output$sdi_plot <- renderPlot({
      calculate_sdi(selected_data(), input$aggregation, input$ref_date, input$year_range)
    })
  })
  
  uploaded_data <- reactiveVal(NULL)
  
  # Upload CSV
  observeEvent(input$file, {
    req(input$file)
    uploaded_data(read_csv(input$file$datapath))
  })
  
  # Process Uploaded Data
  observeEvent(input$process_upload, {
    req(uploaded_data())
    output$upload_sdi_plot <- renderPlot({
      calculate_sdi(uploaded_data(), input$aggregation, input$ref_date, input$year_range)
    })
  })
}

shinyApp(ui, server)
