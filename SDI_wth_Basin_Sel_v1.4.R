# app.R

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(DBI)
library(RSQLite)
library(readr)
library(zoo)

# ── SDI CALCULATION FUNCTION (for uploaded data) ──────────────────────────────
calculate_sdi <- function(df, aggregation, ref_date, year_range) {
  df <- df %>%
    arrange(Date) %>%
    mutate(
      Streamflow_3month = zoo::rollapply(Streamflow, width = 3, FUN = sum, fill = NA, align = "right"),
      Streamflow_6month = zoo::rollapply(Streamflow, width = 6, FUN = sum, fill = NA, align = "right")
    ) %>%
    na.omit()
  
  if (aggregation == "3-month") {
    df <- df %>%
      mutate(SDI = (Streamflow_3month - mean(Streamflow_3month)) / sd(Streamflow_3month))
  } else {
    df <- df %>%
      mutate(SDI = (Streamflow_6month - mean(Streamflow_6month)) / sd(Streamflow_6month))
  }
  
  df <- df %>%
    filter(year(Date) >= year_range[1], year(Date) <= year_range[2])
  
  ggplot(df, aes(x = Date, y = SDI)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = as.Date(ref_date), linetype = "dashed", color = "green", size = 1) +
    labs(
      title = paste(aggregation, "Streamflow Drought Index"),
      x = "Date", y = "SDI"
    ) +
    theme_minimal()
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "MYdrought Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("SDI (Map)",     tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Upload CSV",    tabName = "upload_tab", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #FFE5B4 !important; }
        .skin-blue .main-sidebar, .skin-blue .left-side,
        .skin-blue .main-header .logo, .skin-blue .main-header .navbar {
          background-color: #CD853F !important;
        }
        .skin-blue .sidebar-menu > li.active > a { background-color: #A0522D !important; }
        .box.box-primary > .box-header {
          background-color: #CD853F !important;
          color: white !important;
        }
        .box {
          background-color: #FFF3E0 !important;
          border-radius: 10px;
          border-color: #CD853F !important;
        }
        .box-header { color: white !important; }
      "))
    ),
    tabItems(
      # ── Dashboard Tab ──────────────────────────────────────────────────────
      tabItem(tabName = "dashboard",
              fluidRow(
                box("Select Basin", status = "primary", width = 4, solidHeader = TRUE,
                    leafletOutput("basin_map", height = 300)
                ),
                box("Controls", status = "primary", width = 8, solidHeader = TRUE,
                    uiOutput("station_ui"),
                    selectInput("aggregation",  "Select Aggregation:",
                                choices = c("3-month" = "SDI_3", "6-month" = "SDI_6")),
                    dateInput("ref_date",       "Reference Date:"),
                    sliderInput("year_range",   "Year Range:",
                                min = 1960, max = year(Sys.Date()),
                                value = c(2000, year(Sys.Date())), sep = ""),
                    actionButton("process",     "Plot SDI")
                )
              ),
              fluidRow(
                box("Streamflow Drought Index Time Series", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("sdi_plot", height = 500)
                )
              )
      ),
      
      # ── Upload CSV Tab ─────────────────────────────────────────────────────
      tabItem(tabName = "upload_tab",
              fluidRow(
                box("Upload Daily River Flow CSV", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file", "Choose CSV:",
                              accept = c(".csv")),
                    selectInput("upload_aggregation", "Aggregation:",
                                choices = c("3-month", "6-month")),
                    dateInput("upload_ref_date", "Reference Date:",
                              value = Sys.Date()),
                    sliderInput("upload_year_range", "Year Range:",
                                min = 1960, max = year(Sys.Date()),
                                value = c(2000, year(Sys.Date())), sep = ""),
                    actionButton("process_upload", "Calculate SDI")
                ),
                box("Uploaded Data SDI Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("upload_sdi_plot", height = 400)
                )
              )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # 1) Database connection
  conn <- dbConnect(SQLite(), "streamflowdb_basin.sqlite")
  onSessionEnded(function() dbDisconnect(conn))
  
  # 2) Load basin coords from DB
  basin_coords <- dbGetQuery(conn, "
    SELECT RB_NAME AS Basin, lat AS Latitude, lon AS Longitude
    FROM basin_meta_db
  ")
  
  # 3) Render leaflet basin map
  output$basin_map <- renderLeaflet({
    leaflet(basin_coords) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       layerId = ~Basin, label = ~Basin,
                       radius = 6, color = "blue")
  })
  selected_basin <- reactiveVal(NULL)
  observeEvent(input$basin_map_marker_click, {
    selected_basin(input$basin_map_marker_click$id)
    showNotification(paste("Selected basin:", selected_basin()), type = "message")
  })
  
  # 4) Fetch stations for selected basin
  basin_stations <- reactive({
    req(selected_basin())
    sql <- sprintf(
      "SELECT STN_ID FROM basin_st_tab WHERE TRIM(RB_NAME) = '%s'",
      selected_basin()
    )
    dbGetQuery(conn, sql)$STN_ID
  })
  
  # 5) Station selector UI
  output$station_ui <- renderUI({
    req(basin_stations())
    selectInput("stations", "Select Stations:",
                choices = basin_stations(),
                selected = basin_stations(),
                multiple = TRUE)
  })
  
  # 6) Reactive to pull SDI from DB
  combined_data <- reactive({
    req(selected_basin(), input$aggregation, input$year_range, input$stations)
    sts_sql <- paste0("('", paste(input$stations, collapse = "','"), "')")
    sd <- paste0(input$year_range[1], "-01-01")
    ed <- paste0(input$year_range[2], "-12-01")
    
    query <- paste0("
      SELECT s.Date AS Date, s.STN_ID AS Station, s.SDI
      FROM stn_sf_tab s
      JOIN basin_st_tab b ON s.STN_ID = b.STN_ID
      WHERE TRIM(b.RB_NAME)    = '", selected_basin(), "' AND
            s.AGGREGATION      = '", input$aggregation, "' AND
            s.STN_ID IN ", sts_sql, " AND
            s.Date BETWEEN '", sd, "' AND '", ed, "'
      ORDER BY s.Date
    ")
    df <- dbGetQuery(conn, query)
    df$Date <- ymd(df$Date)
    df
  })
  
  # 7) Plot DB‐backed SDI
  observeEvent(input$process, {
    output$sdi_plot <- renderPlot({
      df <- combined_data()
      req(nrow(df) > 0, "No data found — check your DB filters.")
      sts <- unique(df$Station)
      pal <- if (length(sts) <= 9) brewer.pal(length(sts), "Set1") else
        colorRampPalette(brewer.pal(9, "Set1"))(length(sts))
      cols <- setNames(pal, sts)
      
      ggplot(df, aes(Date, SDI, color = Station, group = Station)) +
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
  
  # ── UPLOAD TAB LOGIC ────────────────────────────────────────────────────────
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath,
                   col_types = cols(
                     Date       = col_date(),
                     Streamflow = col_double()
                   ))
    uploaded_data(df)
  })
  
  observeEvent(input$process_upload, {
    req(uploaded_data())
    output$upload_sdi_plot <- renderPlot({
      calculate_sdi(
        df         = uploaded_data(),
        aggregation = input$upload_aggregation,
        ref_date    = input$upload_ref_date,
        year_range  = input$upload_year_range
      )
    })
  })
}

shinyApp(ui, server)
