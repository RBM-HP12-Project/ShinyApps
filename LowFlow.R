library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)

# Sample data for locations with their corresponding regions
locations <- data.frame(
  Name = c("Location A", "Location B", "Location C", "Location D", "Location E"),
  Latitude = c(3.692083, 3.402780, 3.544720, 3.485560, 2.992780),
  Longitude = c(101.3419, 101.4431, 101.6722, 101.5392, 101.7869),
  Region = c("Region 2", "Region 3", "Region 4", "Region 5", "Region 2")
)

# Function to generate synthetic data
generate_data <- function(n = length(1995:2025)) {
  data.frame(
    Year = 2025,
    Rainfall = round(runif(1, min = 800, max = 2500)),
    Catchment_Value = round(runif(1, min = 50, max = 300))
  )
}

# Generate location-based data
data_list <- list(
  "Location A" = generate_data(),
  "Location B" = generate_data(),
  "Location C" = generate_data(),
  "Location D" = generate_data(),
  "Location E" = generate_data()
)

# Growth Factors for each region and flow duration
growth_factors <- list(
  "Region 2" = data.frame(Duration = c(1, 4, 7, 30), 
                          GF_2 = c(0.97, 0.97, 0.98, 1.00),
                          GF_5 = c(0.60, 0.61, 0.62, 0.66), 
                          GF_10 = c(0.43, 0.44, 0.46, 0.50),
                          GF_20 = c(0.29, 0.31, 0.33, 0.37), 
                          GF_50 = c(0.14, 0.18, 0.20, 0.21)),
  
  "Region 3" = data.frame(Duration = c(1, 4, 7, 30), 
                          GF_2 = c(0.96, 0.97, 0.98, 0.99),
                          GF_5 = c(0.52, 0.53, 0.56, 0.59), 
                          GF_10 = c(0.32, 0.34, 0.37, 0.42),
                          GF_20 = c(0.16, 0.20, 0.23, 0.29), 
                          GF_50 = c(0.05, 0.06, 0.09, 0.16)),
  
  "Region 4" = data.frame(Duration = c(1, 4, 7, 30), 
                          GF_2 = c(0.96, 0.98, 0.99, 1.01),
                          GF_5 = c(0.45, 0.49, 0.51, 0.56), 
                          GF_10 = c(0.25, 0.27, 0.28, 0.32),
                          GF_20 = c(0.14, 0.15, 0.15, 0.17), 
                          GF_50 = c(0.07, 0.08, 0.08, 0.09)),
  
  "Region 5" = data.frame(Duration = c(1, 4, 7, 30), 
                          GF_2 = c(0.91, 0.92, 0.92, 0.93),
                          GF_5 = c(0.49, 0.51, 0.52, 0.54), 
                          GF_10 = c(0.32, 0.33, 0.34, 0.36),
                          GF_20 = c(0.20, 0.21, 0.22, 0.24), 
                          GF_50 = c(0.08, 0.08, 0.10, 0.11))
)

# Function to compute Q7 value
compute_Q7 <- function(A, R, GF) {
  Q7 <- (2.423 * 10^(-11)) * (A^0.984) * (R^2.568)
  return(Q7 * GF)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Low Flow Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map View", tabName = "map_tab", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map_tab",
              fluidRow(
                box(title = "Map", status = "primary", solidHeader = TRUE, width = 6,
                    leafletOutput("map", height = 400)
                ),
                box(title = "Low Flow Statistics", status = "primary", solidHeader = TRUE, width = 6,
                    DTOutput("data_table")
                )
              ),
              fluidRow(
                box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
                    # sliderInput("year", "Select Year:", min = 1995, max = 2025, value = 2025, step = 1, sep = ""),
                    selectInput("low_flow", "Select Low Flow Duration (days):", choices = c(1, 4, 7, 30))
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
  
  # Update Data on Map Click
  observeEvent(input$map_marker_click, {
    selected <- input$map_marker_click$id
    if (!is.null(selected)) {
      location <- locations[locations$Name == selected, ]
      region <- location$Region
      duration <- as.numeric(input$low_flow)
      # year <- as.numeric(input$year)
      year <- 2025
      
      GF_table <- growth_factors[[region]]
      GF_values <- GF_table[GF_table$Duration == duration, ]
      
      # Fetch randomly generated A and R from data_list
      data_selected <- data_list[[selected]]
      A <- data_selected$Catchment_Value[data_selected$Year == year]
      R <- data_selected$Rainfall[data_selected$Year == year]
      
      Q7_2 <- compute_Q7(A, R, GF_values$GF_2)
      Q7_5 <- compute_Q7(A, R, GF_values$GF_5)
      Q7_10 <- compute_Q7(A, R, GF_values$GF_10)
      Q7_20 <- compute_Q7(A, R, GF_values$GF_20)
      Q7_50 <- compute_Q7(A, R, GF_values$GF_50)
      
      results <- data.frame(
        Return_Period = c(2, 5, 10, 20, 50),
        Low_Flow = c(round(Q7_2,digits=2), round(Q7_5,digits=2),round(Q7_10,digits=2),round(Q7_20,digits=2),round(Q7_50,digits=2))
      )
      selected_data(results)
    }
  })
  
  # Display Selected Data
  output$data_table <- renderDT({
    req(selected_data())
    datatable(selected_data())
  })
}

shinyApp(ui, server)
