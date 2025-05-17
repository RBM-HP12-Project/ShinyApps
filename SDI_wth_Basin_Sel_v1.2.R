library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(DBI)
library(RSQLite)

# ---- UI ----
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
          background-color: #FFE5B4 !important;
        }
        .skin-blue .main-sidebar, .skin-blue .left-side,
        .skin-blue .main-header .logo,
        .skin-blue .main-header .navbar {
          background-color: #CD853F !important;
        }
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #A0522D !important;
        }
        .box.box-primary > .box-header {
          background-color: #CD853F !important;
          color: white !important;
        }
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

# ---- SERVER ----
server <- function(input, output, session) {
  # 1) Connect to SQLite once
  conn <- dbConnect(SQLite(), "streamflowdb_basin.sqlite")
  onSessionEnded(function() dbDisconnect(conn))
  
  # 2) Basin list & coords (you had this randomized in your original)
  basin_path  <- "D:/Project_LowFlow_JPS/Drought_analysis/Download_SDI_from_GD"
  basin_names <- list.dirs(basin_path, full.names = FALSE, recursive = FALSE)
  set.seed(42)
  basin_coords <- data.frame(
    Basin    = basin_names,
    Latitude = runif(length(basin_names), min = 2.5,  max = 4.0),
    Longitude= runif(length(basin_names), min = 101.0, max = 102.0),
    stringsAsFactors = FALSE
  )
  
  # 3) Render basin map & track selection
  output$basin_map <- renderLeaflet({
    leaflet(basin_coords) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       layerId = ~Basin, label = ~Basin,
                       radius = 6, color = "blue")
  })
  selected_basin <- reactiveVal(NULL)
  observeEvent(input$basin_map_marker_click, {
    selected_basin(input$basin_map_marker_click$id)
    showNotification(paste0("Selected basin: ", selected_basin()), type = "message")
  })
  
  # 4) Fetch available stations for the selected basin from DB
  basin_stations <- reactive({
    req(selected_basin())
    sql <- sprintf(
      "SELECT STN_ID FROM basin_st_tab WHERE TRIM(RB_NAME) = '%s'",
      selected_basin()
    )
    df <- dbGetQuery(conn, sql)
    df$STN_ID
  })
  
  # 5) Station multiselect UI
  output$station_ui <- renderUI({
    req(basin_stations())
    selectInput("stations", "Select Stations:",
                choices = basin_stations(), selected = basin_stations(),
                multiple = TRUE)
  })
  
  # 6) Reactive that runs the SQL query
  combined_data <- reactive({
    req(selected_basin(), input$aggregation, input$year_range, input$stations)
    
    # Build station IN-clause
    sts_sql <- paste0("('", paste(input$stations, collapse="','"), "')")
    
    # Date range from slider
    sd <- paste0(input$year_range[1], "-01-01")
    ed <- paste0(input$year_range[2], "-12-01")
    
    # Compose and run query
    query <- paste0("
      SELECT
        s.Date        AS Date,
        s.STN_ID      AS Station,
        s.SDI         AS SDI,
        s.AGGREGATION AS Aggregation
      FROM stn_sf_tab s
      JOIN basin_st_tab b ON s.STN_ID = b.STN_ID
      WHERE
        TRIM(b.RB_NAME) = '", selected_basin(), "' AND
        s.AGGREGATION   = '", input$aggregation, "' AND
        s.STN_ID IN ", sts_sql, " AND
        s.Date BETWEEN '", sd, "' AND '", ed, "'
      ORDER BY s.Date
    ")
    df <- dbGetQuery(conn, query)
    df$Date <- ymd(df$Date)
    df
  })
  
  # 7) Plot on button click
  observeEvent(input$process, {
    output$sdi_plot <- renderPlot({
      df <- combined_data()
      req(nrow(df) > 0, "No data left after filtering – check your DB entries.")
      
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
