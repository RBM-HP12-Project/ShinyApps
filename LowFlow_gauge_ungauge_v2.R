library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(readr)
library(sf)    

# ---------------------------- Shared Data ----------------------------
# Tab 1: Low Flow Analysis
locations <- data.frame(
  Name = c("Location A", "Location B", "Location C", "Location D", "Location E"),
  Latitude = c(3.692083, 3.402780, 3.544720, 3.485560, 2.992780),
  Longitude = c(101.3419, 101.4431, 101.6722, 101.5392, 101.7869),
  Region = c("Region 2", "Region 3", "Region 4", "Region 5", "Region 2"),
  Catchment_Value = round(runif(5, min = 1000, max = 3000))
)

growth_factors <- list(
  "Region 2" = data.frame(Duration = c(1, 4, 7, 30), GF_2 = c(0.97, 0.97, 0.98, 1.00), GF_5 = c(0.60, 0.61, 0.62, 0.66), GF_10 = c(0.43, 0.44, 0.46, 0.50), GF_20 = c(0.29, 0.31, 0.33, 0.37), GF_50 = c(0.14, 0.18, 0.20, 0.21)),
  "Region 3" = data.frame(Duration = c(1, 4, 7, 30), GF_2 = c(0.96, 0.97, 0.98, 0.99), GF_5 = c(0.52, 0.53, 0.56, 0.59), GF_10 = c(0.32, 0.34, 0.37, 0.42), GF_20 = c(0.16, 0.20, 0.23, 0.29), GF_50 = c(0.05, 0.06, 0.09, 0.16)),
  "Region 4" = data.frame(Duration = c(1, 4, 7, 30), GF_2 = c(0.96, 0.98, 0.99, 1.01), GF_5 = c(0.45, 0.49, 0.51, 0.56), GF_10 = c(0.25, 0.27, 0.28, 0.32), GF_20 = c(0.14, 0.15, 0.15, 0.17), GF_50 = c(0.07, 0.08, 0.08, 0.09)),
  "Region 5" = data.frame(Duration = c(1, 4, 7, 30), GF_2 = c(0.91, 0.92, 0.92, 0.93), GF_5 = c(0.49, 0.51, 0.52, 0.54), GF_10 = c(0.32, 0.33, 0.34, 0.36), GF_20 = c(0.20, 0.21, 0.22, 0.24), GF_50 = c(0.08, 0.08, 0.10, 0.11))
)

compute_Q7 <- function(A, R, GF) {
  Q7 <- (2.423 * 10^(-11)) * (A^0.984) * (R^2.568)
  return(Q7 * GF)
}

# Tab 2: Return Period Lookup
rp_data <- read_csv("all_combined_rp_results.csv")
basins <- unique(rp_data$Basin)

station_locations <- read.csv("Q_prelimscreen_AI.csv")
return_periods <- c(1.11, 2, 5, 10, 20, 25, 50, 100)
amin_types <- c("amin_1Q", "amin_4Q", "amin_7Q", "amin_30Q", "amin_60Q", "amin_90Q")

# ---------------------------- UI ----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "MyLowFlow"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ungauged Low Flow",        tabName = "map_tab",    icon = icon("tint")),
      menuItem("Gauged In-Built Low Flow",   tabName = "rp_tab",     icon = icon("chart-line")),
      menuItem("Return Period Calculator",       tabName = "rp_calc",    icon = icon("calculator"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(12,
             tags$div(
               class = "bg-primary",
               style = "padding: 20px; color: white; text-align: center; border-bottom: 5px solid #367fa9;",
               fluidRow(
                 column(2, tags$img(src = "logo-petra2.png", height = "180px")),
                 column(8,
                        tags$h3("Hydrological Procedure No. 12 Low Flow and Drought", style = "margin-bottom:0; font-weight:bold;"),
                        tags$h3("Estimation Tool", style = "margin-top:0; font-weight:bold;"),
                        tags$h4("Department of Irrigation and Drainage", style = "margin-top:10px;"),
                        tags$h4("Ministry of Energy Transition and Water Transformation (PETRA)")
                 ),
                 column(2, tags$img(src = "jps_logo.jpeg", height = "180px"))
               )
             )
      )
    ),
    tabItems(
      # Tab 1: Low Flow Map
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
                    numericInput("rainfall", "Enter Rainfall (mm):", value = 1500, min = 0),
                    numericInput("catchment", "Enter Catchment Area (km^2):", value = NA, min = 0),
                    selectInput("low_flow", "Select Low Flow Duration (days):", choices = c(1, 4, 7, 30))
                )
              )
      ),
      
      # Tab 2: Return Period Map
      tabItem(tabName = "rp_tab",
              fluidRow(
                box(title = "Basin Map", status = "primary", solidHeader = TRUE, width = 6,
                    leafletOutput("basin_map", height = 400)
                ),
                box(title = "Filtered Station Values", status = "primary", solidHeader = TRUE, width = 6,
                    DTOutput("rp_table")
                )
              ),
              fluidRow(
                box(title = "Selections", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("amin_type", "Select low flow duration:", choices = amin_types),
                    selectInput("return_period", "Select Return Period:", choices = return_periods)
                )
              )
      ),
      
      # Tab 3: Return Period Calculator
      tabItem(tabName = "rp_calc",
              fluidRow(
                box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 4,
                    fileInput("station_csv", "Upload Daily Streamflow CSV"),
                    numericInput("duration", "Select Duration:", value = 1, min = 1),
                    actionButton("process1", "Compute annual minima"),
                    actionButton("process2", "Compute return period low flow")
                ),
                box(title = "Annual Minima Table", status = "primary", solidHeader = TRUE, width = 8,
                    DTOutput("minQTable"),
                    downloadButton("downloadMinQ", "Download Annual Minima Table"),
                )
              ),
              fluidRow(
                box(title = "Best Fit Distribution", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("bestDist")
                )
              ),
              fluidRow(
                box(title = "Return Period Table", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("returnPeriods")
                ),
                downloadButton("downloadReturnPeriod", "Download Return Period Table"),
              )
      )
    )
  )
)

# ---------------------------- Server ----------------------------
server <- function(input, output, session) {
  # # --- Tab 1: Low Flow ---
  # selected_location <- reactiveVal(NULL)
  # 
  # # Load and transform the shapefile
  # basins_shp <- read_sf("Malaysia_Basin_shp/MalaysiaBasin_V2.shp", quiet = TRUE)
  # basins_wgs84 <- st_transform(basins_shp, 4326)
  # 
  # # Render the interactive map with polygons only
  # output$map <- renderLeaflet({
  #   leaflet(basins_wgs84) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       fillColor   = "#FF7800AA",
  #       color       = "#444444",
  #       weight      = 1,
  #       opacity     = 1,
  #       fillOpacity = 0.5,
  #       layerId     = ~RB_NAME,   # Required for shape click
  #       label       = ~RB_NAME
  #     )
  # })
  # 
  # # When a basin polygon is clicked
  # observeEvent(input$map_shape_click, {
  #   clicked_basin <- input$map_shape_click$id
  #   selected_location(clicked_basin)
  #   
  #   basin_row <- basins_wgs84[basins_wgs84$RB_NAME == clicked_basin, ]
  #   last_val <- tail(na.omit(basin_row$Shape_Area), 1)
  #   # Extract AREA_M2 and convert to km^2
  #   catchment_area_km2 <- as.numeric(last_val) / 1e6
  #   updateNumericInput(session, "catchment", value = round(catchment_area_km2, 2))
  # })
  # 
  # # Automatically compute Q values whenever any input changes
  # selected_data <- reactive({
  #   req(input$catchment, input$rainfall, input$region_select, input$low_flow, selected_location())
  #   
  #   region <- input$region_select
  #   duration <- as.numeric(input$low_flow)
  #   GF_table <- growth_factors[[region]]
  #   GF_values <- GF_table[GF_table$Duration == duration, ]
  #   
  #   A <- input$catchment
  #   R <- input$rainfall
  #   
  #   Q7_2 <- compute_Q7(A, R, GF_values$GF_2)
  #   Q7_5 <- compute_Q7(A, R, GF_values$GF_5)
  #   Q7_10 <- compute_Q7(A, R, GF_values$GF_10)
  #   Q7_20 <- compute_Q7(A, R, GF_values$GF_20)
  #   Q7_50 <- compute_Q7(A, R, GF_values$GF_50)
  #   
  #   data.frame(
  #     Return_Period = c(2, 5, 10, 20, 50),
  #     Low_Flow = c(round(Q7_2, 2), round(Q7_5, 2), round(Q7_10, 2), round(Q7_20, 2), round(Q7_50, 2))
  #   )
  # })
  # 
  # # Display the auto-updating table
  # output$data_table <- renderDT({
  #   req(selected_data())
  #   datatable(selected_data())
  # })
  
  # --- Tab 1: Low Flow ---
  selected_data <- reactiveVal(NULL)
  selected_location <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = locations,
        ~Longitude, ~Latitude,
        popup = ~paste("Region:", Region, "<br>Name:", Name),
        layerId = ~Name
      )
  })
  
  observeEvent(input$map_marker_click, {
    selected <- input$map_marker_click$id
    selected_location(selected)
    
    location_row <- locations[locations$Name == selected, ]
    updateNumericInput(session, "catchment", value = location_row$Catchment_Value)
    
    region <- location_row$Region
    duration <- as.numeric(input$low_flow)
    
    GF_table <- growth_factors[[region]]
    GF_values <- GF_table[GF_table$Duration == duration, ]
    
    A <- input$catchment
    R <- input$rainfall
    
    Q7_2 <- compute_Q7(A, R, GF_values$GF_2)
    Q7_5 <- compute_Q7(A, R, GF_values$GF_5)
    Q7_10 <- compute_Q7(A, R, GF_values$GF_10)
    Q7_20 <- compute_Q7(A, R, GF_values$GF_20)
    Q7_50 <- compute_Q7(A, R, GF_values$GF_50)
    
    results <- data.frame(
      Return_Period = c(2, 5, 10, 20, 50),
      Low_Flow = c(round(Q7_2, 2), round(Q7_5, 2), round(Q7_10, 2), round(Q7_20, 2), round(Q7_50, 2))
    )
    selected_data(results)
  })
  
  output$data_table <- renderDT({
    req(selected_data())
    datatable(selected_data())
  })
  
  
  # --- Tab 2: Return Period ---
  selected_basin <- reactiveVal(NULL)
  
  # Replace this path with the folder containing your shapefile (.shp + .dbf etc)
  basins_shp <- read_sf("Malaysia_Basin_shp/MalaysiaBasin_V2.shp", quiet = TRUE)
  
  basins_wgs84 <- st_transform(basins_shp, 4326)
  
  output$basin_map <- renderLeaflet({
    leaflet(basins_wgs84) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = "#FF7800AA",   # semi-transparent orange
        color       = "#444444",     # border color
        weight      = 1,             # border thickness
        opacity     = 1,             # border opacity
        fillOpacity = 0.5,           # interior opacity
        label       = ~RB_NAME,       # hover labels (adjust if your field is named differently)
        layerId     = ~RB_NAME   # <-- Add this
      ) %>%
      # add the points on top
      addCircleMarkers(
        data    = station_locations,
        lng     = ~Longitude, lat = ~Latitude,
        layerId = ~StationID, color = "blue",
        radius  = 5, label = ~StationID
      )
  })
  
  observeEvent(input$basin_map_shape_click, {
    selected <- input$basin_map_shape_click$id
    selected_basin(selected)
  })
  
  output$rp_table <- renderDT({
    req(selected_basin(), input$return_period, input$amin_type)
    
    basin_data <- rp_data %>% 
      filter(Basin == selected_basin())
    
    colname <- paste0("RP_", input$return_period, "_yr")
    
    if (!(colname %in% colnames(basin_data))) {
      return(data.frame(Station = "N/A", Message = paste("Column", colname, "not found in dataset")))
    }
    
    result <- basin_data %>%
      filter(Amin_Type == input$amin_type) %>%
      select(Station, !!sym(colname)) %>%
      setNames(c("Station", paste(input$return_period, "Year ARI"))) 
    
    datatable(result, options = list(pageLength = 10))
  })
  
  #------- Tab 3 ----------
  minQData <- reactiveVal()
  rpResults <- reactiveVal()
  bestDist <- reactiveVal()
  
  observeEvent(input$process1, {
    req(input$station_csv)
    df <- read.csv(input$station_csv$datapath)
    j_data <- read.csv("D:/Project_LowFlow_JPS/Drought_analysis/amin_analysis/single_station_analysis/j.csv", stringsAsFactors = FALSE)
    colnames(j_data)[1] <- "STN_ID"
    
    d <- input$duration
    station_name <- as.character(df[2, 3])
    j_row <- j_data[j_data$STN_ID == station_name, ]
    if (nrow(j_row) == 0) return()
    
    j_start_month <- as.numeric(j_row$j[1])
    df$Date <- as.Date(df$Date)
    df$Flow <- df[[2]]
    df$Year <- as.numeric(format(df$Date, "%Y"))
    df$Month <- as.numeric(format(df$Date, "%m"))
    df$HydroYear <- ifelse(df$Month >= j_start_month, df$Year + 1, df$Year)
    df_clean <- df[!is.na(df$Flow), ]
    
    pettitt_result_all <- trend::pettitt.test(df_clean$Flow)
    change_index <- pettitt_result_all$estimate[1]
    change_date <- df_clean$Date[change_index]
    flow_before <- df_clean$Flow[1:change_index]
    flow_after <- df_clean$Flow[(change_index + 1):nrow(df_clean)]
    mean_diff <- mean(flow_after) - mean(flow_before)
    
    df_clean$Flow_detrended <- df_clean$Flow
    df_clean$Flow_detrended[(change_index + 1):nrow(df_clean)] <- df_clean$Flow_detrended[(change_index + 1):nrow(df_clean)] - mean_diff
    
    df_clean <- df_clean %>%
      arrange(Date) %>%
      mutate(
        Flow_roll_raw = zoo::rollmean(Flow, k = d, align = "right", fill = NA),
        Flow_roll_detrended = zoo::rollmean(Flow_detrended, k = d, align = "right", fill = NA)
      )
    
    minQ <- df_clean %>%
      filter(!is.na(Flow_roll_raw) & !is.na(Flow_roll_detrended)) %>%
      group_by(HydroYear) %>%
      summarise(
        Year = unique(HydroYear),
        MinFlow_Raw = min(Flow_roll_raw),
        MinFlow_Detrended = min(Flow_roll_detrended),
        .groups = "drop"
      )
    
    minQ$Duration <- d
    minQ$Station <- rep(station_name, nrow(minQ))
    minQData(minQ)
  })
  
  output$minQTable <- renderDT({
    req(minQData())
    # Select specific columns to display
    display_df <- minQData()[, c("HydroYear", "MinFlow_Raw")]
    datatable(display_df, options = list(pageLength = 5))
  })
  
  output$downloadMinQ <- downloadHandler(
    filename = function() { paste0("minQ_results_d", input$duration, ".csv") },
    content = function(file) {
      write.csv(minQData(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$process2, {
    amin <- minQData()
    req(amin)
    
    if (!"Station" %in% colnames(amin)) {
      showNotification("Missing 'Station' column. Please rerun Checkpoint 1.", type = "error")
      return()
    }
    
    # Define the return periods for which to calculate values
    return_periods <- c(1.11, 2, 5, 10, 20, 25, 50, 100) # Using 1.11 as per provided base code
    
    # Calculate non-exceedance probabilities for low flow based on distribution type
    # For distributions fitted to POSITIVE minima (e.g., Gamma, LN, Pearson Type 3, GLO): P = 1/T
    non_exceedance_probs_positive_fits <- 1 / return_periods
    # For distributions fitted to NEGATED minima (EVT types used for maxima): P = 1 - 1/T
    # This is because if X is low flow, Y = -X is a high value (maxima).
    # P(X <= x_low) = P(Y >= -x_low) = 1/T
    # So, P(Y <= -x_low) = 1 - 1/T
    non_exceedance_probs_negated_fits <- 1 - (1 / return_periods)
    
    # Create names for the return period columns in the final table
    return_period_col_names <- paste0("RP_", return_periods, "_yr")
    
    # Define the types of 'amin' data to process (added as per instruction)
    amin_type <- paste0("amin_", amin$Duration, "Q")
    
    
    cat("  Processing station:", amin$Station, "\n")
    
    # --- Data Preparation ---
    # Data for extreme value distributions (negated minima, can be negative)
    data_negated_minima <- -amin$MinFlow_Raw
    data_negated_minima <- data_negated_minima[!is.na(data_negated_minima) & is.finite(data_negated_minima)]
    
    # Data for distributions requiring positive values (original minima)
    data_positive_minima <- amin$MinFlow_Raw
    data_positive_minima <- data_positive_minima[!is.na(data_positive_minima) & is.finite(data_positive_minima) & data_positive_minima > 0]
    
    # List to store ONLY K-S statistics for the current station
    ks_values_station <- list()
    
    # Define a minimum number of data points for reliable fitting
    MIN_DATA_POINTS_FEVD <- 5
    MIN_DATA_POINTS_FITDISTRPLUS <- 5
    MIN_DATA_POINTS_LN3_P3 <- 4 # Applies to LN3, Pearson Type 3, and GLO (L-moments)
    
    # --- Fit Extreme Value Distributions (using data_negated_minima) ---
    if (!is.numeric(data_negated_minima) || length(data_negated_minima) < MIN_DATA_POINTS_FEVD || (var(data_negated_minima) == 0 && length(data_negated_minima) > 1)) {
      ks_values_station$GEV <- NA
      ks_values_station$Gumbel <- NA
      # Removed GP and Exponential as not in implied base code
    } else {
      # GEV
      fit_gev <- tryCatch({ extRemes::fevd(data_negated_minima, type = "GEV") }, error = function(e) { NULL })
      if (!is.null(fit_gev)) {
        ks_values_station$GEV <- tryCatch({ as.numeric(ks.test(x = data_negated_minima, y = "pgev", loc = fit_gev$results$par["location"], scale = fit_gev$results$par["scale"], shape = fit_gev$results$par["shape"])$statistic) }, error = function(e) { NA })
      } else { ks_values_station$GEV <- NA }
      
      # Gumbel
      fit_gumbel <- tryCatch({ extRemes::fevd(data_negated_minima, type = "Gumbel") }, error = function(e) { NULL })
      if (!is.null(fit_gumbel)) {
        ks_values_station$Gumbel <- tryCatch({ as.numeric(ks.test(x = data_negated_minima, y = "pgev", loc = fit_gumbel$results$par["location"], scale = fit_gumbel$results$par["scale"], shape = 0)$statistic) }, error = function(e) { NA })
      } else { ks_values_station$Gumbel <- NA }
    }
    
    # --- Fit General Distributions (using data_positive_minima) ---
    if (!is.numeric(data_positive_minima) || length(data_positive_minima) < MIN_DATA_POINTS_FITDISTRPLUS || (var(data_positive_minima) == 0 && length(data_positive_minima) > 1)) {
      ks_values_station$Gamma <- NA
      ks_values_station$`LN-2` <- NA
      ks_values_station$`LN-3` <- NA
      ks_values_station$`Pearson Type 3` <- NA
      ks_values_station$GLO <- NA # Re-added GLO
    } else {
      # Gamma
      fit_gamma <- tryCatch({ fitdistrplus::fitdist(data_positive_minima, "gamma") }, error = function(e) { NULL })
      if (!is.null(fit_gamma)) {
        ks_values_station$Gamma <- tryCatch({ as.numeric(ks.test(x = data_positive_minima, y = "pgamma", shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"])$statistic) }, error = function(e) { NA })
      } else { ks_values_station$Gamma <- NA }
      
      # Log-Normal (LN-2)
      fit_ln2 <- tryCatch({ fitdistrplus::fitdist(data_positive_minima, "lnorm") }, error = function(e) { NULL })
      if (!is.null(fit_ln2)) {
        ks_values_station$`LN-2` <- tryCatch({ as.numeric(ks.test(x = data_positive_minima, y = "plnorm", meanlog = fit_ln2$estimate["meanlog"], sdlog = fit_ln2$estimate["sdlog"])$statistic) }, error = function(e) { NA })
      } else { ks_values_station$`LN-2` <- NA }
      
      # Log-Normal (LN-3) - Using lmomco
      if (length(data_positive_minima) >= MIN_DATA_POINTS_LN3_P3) {
        fit_ln3_params <- tryCatch({ lmom_data <- lmomco::lmom.ub(data_positive_minima); lmomco::parln3(lmom_data) }, error = function(e) { NULL })
        if (!is.null(fit_ln3_params) && !is.null(fit_ln3_params$para)) {
          pln3_custom <- function(q, par_obj) { lmomco::cdfln3(q, par = par_obj) } # 'par =' kept for helper function
          ks_values_station$`LN-3` <- tryCatch({ as.numeric(ks.test(x = data_positive_minima, y = pln3_custom, par_obj = fit_ln3_params)$statistic) }, error = function(e) { NA })
        } else { ks_values_station$`LN-3` <- NA }
      } else { ks_values_station$`LN-3` <- NA }
      
      # Pearson Type 3
      if (length(data_positive_minima) >= MIN_DATA_POINTS_LN3_P3) {
        fit_p3_params <- tryCatch({ lmom_data <- lmomco::lmom.ub(data_positive_minima); lmomco::parpe3(lmom_data) }, error = function(e) { NULL })
        if (!is.null(fit_p3_params) && !is.null(fit_p3_params$para)) {
          ppearson3_custom <- function(q, par_obj) { lmomco::cdfpe3(q, par = par_obj) } # 'par =' kept for helper function
          ks_values_station$`Pearson Type 3` <- tryCatch({ as.numeric(ks.test(x = data_positive_minima, y = ppearson3_custom, par_obj = fit_p3_params)$statistic) }, error = function(e) { NA })
        } else { ks_values_station$`Pearson Type 3` <- NA }
      } else { ks_values_station$`Pearson Type 3` <- NA }
      
      # Generalized Logistic (GLO) - Re-added
      if (length(data_positive_minima) >= MIN_DATA_POINTS_LN3_P3) {
        fit_glo_params <- tryCatch({ lmom_data <- lmomco::lmom.ub(data_positive_minima); lmomco::parglo(lmom_data) }, error = function(e) { NULL })
        if (!is.null(fit_glo_params) && !is.null(fit_glo_params$para)) {
          pglo_custom <- function(q, par_obj) { lmomco::cdfglo(q, par = par_obj) } # 'par =' kept for helper function
          ks_values_station$GLO <- tryCatch({ as.numeric(ks.test(x = data_positive_minima, y = pglo_custom, par_obj = fit_glo_params)$statistic) }, error = function(e) { NA })
        } else { ks_values_station$GLO <- NA }
      } else { ks_values_station$GLO <- NA }
    }
    
    # --- Determine Best Fit and Refit for Plotting and RP Calculation ---
    ks_values_numeric <- unlist(ks_values_station) # Convert to a vector for min()
    min_ks_value <- min(ks_values_numeric, na.rm = TRUE)
    
    best_fit_object_for_plot <- NULL
    best_dist_name <- NA_character_
    
    # Initialize return period values and parameters for the current station
    rp_values_current_station <- setNames(rep(NA_real_, length(return_periods)), return_period_col_names)
    rp_params_current_station <- list(
      Basin = "Uploaded_Basin",
      Amin_Type = amin_type, # Added for multi-Q output
      Station = amin$Station,
      Best_Fit_Distribution = NA_character_,
      Loc_Param = NA_real_, Scale_Param = NA_real_, Shape_Param = NA_real_, Rate_Param = NA_real_, Threshold_Param = NA_real_, MeanLog_Param = NA_real_, SdLog_Param = NA_real_, Zeta_Param = NA_real_, Alpha_Param = NA_real_, Kappa_Param = NA_real_
    )
    
    
    if (is.finite(min_ks_value)) {
      best_dist_name <- names(which.min(ks_values_numeric)) #This where we get best fit method
      rp_params_current_station$Best_Fit_Distribution <- best_dist_name
      
      # Refit the best distribution to get the fit object for plotting and RP calculation
      best_fit_object_for_plot <- tryCatch({
        if (best_dist_name == "GEV") {
          extRemes::fevd(data_negated_minima, type = "GEV")
        } else if (best_dist_name == "Gumbel") {
          extRemes::fevd(data_negated_minima, type = "Gumbel")
        } else if (best_dist_name == "Gamma") {
          fitdistrplus::fitdist(data_positive_minima, "gamma")
        } else if (best_dist_name == "LN-2") {
          fitdistrplus::fitdist(data_positive_minima, "lnorm")
        } else if (best_dist_name == "LN-3") { # Using lmomco for LN-3
          lmom_data_refit <- lmomco::lmom.ub(data_positive_minima)
          lmomco::parln3(lmom_data_refit)
        } else if (best_dist_name == "Pearson Type 3") {
          lmom_data_refit <- lmomco::lmom.ub(data_positive_minima)
          lmomco::parpe3(lmom_data_refit)
        } else if (best_dist_name == "GLO") { # Re-added GLO refit
          lmom_data_refit <- lmomco::lmom.ub(data_positive_minima)
          lmomco::parglo(lmom_data_refit)
        } else { NULL } # Removed GP, Exponential, Pareto
      }, error = function(e) { NULL })
      
      # Extract parameters for RP table and calculate return period values
      if (!is.null(best_fit_object_for_plot)) {
        calculated_rp_values <- tryCatch({
          current_non_exceedance_probs <- NULL
          # GEV and Gumbel are from negated fits
          if (best_dist_name %in% c("GEV", "Gumbel")) {
            current_non_exceedance_probs <- non_exceedance_probs_negated_fits
          } else { # Gamma, LN-2, LN-3, Pearson Type 3, GLO
            current_non_exceedance_probs <- non_exceedance_probs_positive_fits
          }
          
          q_rp <- NULL
          switch(best_dist_name,
                 "GEV" = {
                   rp_params_current_station$Loc_Param <- best_fit_object_for_plot$results$par["location"]
                   rp_params_current_station$Scale_Param <- best_fit_object_for_plot$results$par["scale"]
                   rp_params_current_station$Shape_Param <- best_fit_object_for_plot$results$par["shape"]
                   q_rp <- qgev(current_non_exceedance_probs, loc = rp_params_current_station$Loc_Param, scale = rp_params_current_station$Scale_Param, shape = rp_params_current_station$Shape_Param)
                   q_rp <- -q_rp # Negate back to original flow scale
                 },
                 "Gumbel" = {
                   rp_params_current_station$Loc_Param <- best_fit_object_for_plot$results$par["location"]
                   rp_params_current_station$Scale_Param <- best_fit_object_for_plot$results$par["scale"]
                   rp_params_current_station$Shape_Param <- 0 # Fixed for Gumbel
                   q_rp <- qgev(current_non_exceedance_probs, loc = rp_params_current_station$Loc_Param, scale = rp_params_current_station$Scale_Param, shape = 0)
                   q_rp <- -q_rp # Negate back to original flow scale
                 },
                 "Gamma" = {
                   rp_params_current_station$Shape_Param <- best_fit_object_for_plot$estimate["shape"]
                   rp_params_current_station$Rate_Param <- best_fit_object_for_plot$estimate["rate"]
                   q_rp <- qgamma(current_non_exceedance_probs, shape = rp_params_current_station$Shape_Param, rate = rp_params_current_station$Rate_Param)
                 },
                 "LN-2" = {
                   rp_params_current_station$MeanLog_Param <- best_fit_object_for_plot$estimate["meanlog"]
                   rp_params_current_station$SdLog_Param <- best_fit_object_for_plot$estimate["sdlog"]
                   q_rp <- qlnorm(current_non_exceedance_probs, meanlog = rp_params_current_station$MeanLog_Param, sdlog = rp_params_current_station$SdLog_Param)
                 },
                 "LN-3" = {
                   rp_params_current_station$Threshold_Param <- best_fit_object_for_plot$para["zeta"]
                   rp_params_current_station$MeanLog_Param <- best_fit_object_for_plot$para["mulog"]
                   rp_params_current_station$SdLog_Param <- best_fit_object_for_plot$para["sdlog"]
                   q_rp <- lmomco::qualn3(current_non_exceedance_probs, best_fit_object_for_plot) # Removed 'par ='
                 },
                 "Pearson Type 3" = {
                   rp_params_current_station$Zeta_Param <- best_fit_object_for_plot$para["zeta"]
                   rp_params_current_station$Alpha_Param <- best_fit_object_for_plot$para["alpha"]
                   rp_params_current_station$Kappa_Param <- best_fit_object_for_plot$para["kappa"]
                   q_rp <- lmomco::quape3(current_non_exceedance_probs, best_fit_object_for_plot) # Removed 'par ='
                 },
                 "GLO" = { # Re-added GLO RP calculation
                   rp_params_current_station$Zeta_Param <- best_fit_object_for_plot$para["zeta"]
                   rp_params_current_station$Alpha_Param <- best_fit_object_for_plot$para["alpha"]
                   rp_params_current_station$Kappa_Param <- best_fit_object_for_plot$para["kappa"]
                   q_rp <- lmomco::quaglo(current_non_exceedance_probs, best_fit_object_for_plot) # Removed 'par ='
                 }
          )
          if (!all(is.finite(q_rp))) { rep(NA_real_, length(return_periods)) } else { q_rp } # Corrected length argument
        }, error = function(e) { rep(NA_real_, length(return_periods)) }) # Corrected length argument
        # Round RP values to 0 decimal places
        rp_values_current_station <- setNames(round(calculated_rp_values, 0), return_period_col_names)
        rp_values_current_station <- lapply(rp_values_current_station, function(x) ifelse(x < 0, 0, x))
      }
    }
    
    
    bestDist(best_dist_name)
    # Convert list to numeric vector
    rp_values_numeric <- as.numeric(rp_values_current_station[return_period_col_names])
    
    # Create final table with Return Period and Flow
    rp_value_df <- data.frame(
      Return_Period = return_periods,
      Low_Flow = round(rp_values_numeric, 2)
    )
    rpResults(rp_value_df)
  })
  
  output$returnPeriods <- renderDT({
    req(rpResults())
    datatable(rpResults())
  })
  
  output$downloadReturnPeriod <- downloadHandler(
    filename = function() { paste0("ReturnPeriod_results_d", input$duration, ".csv") },
    content = function(file) {
      write.csv(rpResults(), file, row.names = FALSE)
    }
  )
  
  output$bestDist <- renderPrint({
    req(bestDist())
    paste("Best Fit Distribution:", bestDist())
  })
}

shinyApp(ui, server)
