library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)

# Define basin folders and random coordinates
basin_path <- "D:\\Project_LowFlow_JPS\\Drought_analysis\\Download_SDI_from_GD"
basin_names <- list.dirs(basin_path, full.names = FALSE, recursive = FALSE)
set.seed(42)
basin_coords <- data.frame(
  Basin = basin_names,
  Latitude = runif(length(basin_names), min = 2.5, max = 4.0),
  Longitude = runif(length(basin_names), min = 101.0, max = 102.0),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "MYdrought Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("SDI", tabName = "dashboard", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Change the dashboard body background */
        .content-wrapper, .right-side {
          background-color: #FFE5B4 !important; /* Light Orange */
        }

        /* Sidebar and header background */
        .skin-blue .main-sidebar, .skin-blue .left-side,
        .skin-blue .main-header .logo,
        .skin-blue .main-header .navbar {
          background-color: #CD853F !important; /* Gold */
        }

        /* Active sidebar menu item */
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #A0522D !important; /* Darker Gold */
        }

        /* Box header */
        .box.box-primary > .box-header {
          background-color: #CD853F !important;
          color: white !important;
        }

        /* Box content */
        .box {
          background-color: #FFF3E0 !important;
          border-radius: 10px;
          border-color: #CD853F !important;
        }

        .box-header {
          color: white !important;
        }
      "))
    ),
    fluidRow(
      box("Select Basin", status = "primary", width = 4, solidHeader = TRUE,
          leafletOutput("basin_map", height = 300)
      ),
      box("Controls", status = "primary", width = 8, solidHeader = TRUE,
          uiOutput("station_ui"),
          selectInput("aggregation", "Select Aggregation:",
                      choices = c("3-month" = "SDI_3", "6-month" = "SDI_6")),
          dateInput("ref_date", "Reference Date:"),
          sliderInput("year_range", "Year Range:",
                      min = 1960, max = year(Sys.Date()),
                      value = c(2000, year(Sys.Date())), sep = ""),
          actionButton("process", "Plot SDI")
      )
    ),
    fluidRow(
      box("Streamflow Drought Index Time Series", width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("sdi_plot", height = 500)
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$basin_map <- renderLeaflet({
    leaflet(basin_coords) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       layerId = ~Basin, label = ~Basin,
                       radius = 6, color = "blue")
  })
  
  selected_basin <- reactiveVal(NULL)
  observeEvent(input$basin_map_marker_click, {
    selected_basin(input$basin_map_marker_click$id)
    showNotification("Selected basin: " %>% paste0(selected_basin()), type = "message")
  })
  
  basin_files <- reactive({
    req(selected_basin())
    list.files(file.path(basin_path, selected_basin(),"SDI_results"),
               pattern = "\\.csv$", full.names = TRUE)
  })
  
  output$station_ui <- renderUI({
    req(basin_files())
    sts <- tools::file_path_sans_ext(basename(basin_files()))
    selectInput("stations", "Select Stations:",
                choices = sts, selected = sts, multiple = TRUE)
  })
  
  combined_data <- reactive({
    req(input$stations)
    sel_files <- basin_files()[tools::file_path_sans_ext(basename(basin_files())) %in% input$stations]
    
    df_list <- lapply(sel_files, function(f) {
      dat <- read_csv(f) %>%
        mutate(
          Date    = ymd(Date),
          Station = tools::file_path_sans_ext(basename(f))
        )
      dat_imp <- read.csv(f)
      dat$SDI <- dat_imp$SDI
      dat
    })
    
    bind_rows(df_list)
  })
  
  observeEvent(input$process, {
    output$sdi_plot <- renderPlot({
      df <- combined_data()
      
      df <- df %>%
        filter(
          Aggregation == input$aggregation,
          year(Date) >= input$year_range[1],
          year(Date) <= input$year_range[2]
        )
      
      req(nrow(df) > 0, "No data left after filtering – check your CSV headers/values.")
      
      sts <- unique(df$Station)
      pal <- if (length(sts) <= 9) brewer.pal(length(sts), "Set1") else
        colorRampPalette(brewer.pal(9, "Set1"))(length(sts))
      cols <- setNames(pal, sts)
      
      ggplot(df, aes(x = Date, y = SDI, color = Station, group = Station)) +
        geom_line(size = 1.2, na.rm = TRUE) +
        scale_color_manual(values = cols) +
        geom_vline(xintercept = as.Date(input$ref_date), linetype = "dashed") +
        labs(
          title = paste(selected_basin(), input$aggregation, "SDI Series"),
          x = "Date", y = "SDI"
        ) +
        theme_minimal(base_size = 14)
    })
  })
}

shinyApp(ui, server)
