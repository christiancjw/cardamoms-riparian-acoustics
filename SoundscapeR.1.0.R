library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyjs)

## Setup --------------------------------------------------------------------------------------
### Data Read in ------------------
global_singledevice         <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
continuous_singledevice     <- read.csv("clean_data/datasets/indices_datasets/continuous_data.csv")
multi25                     <- read.csv("clean_data/datasets/indices_datasets/multi25_data.csv")
multi25_1in5                <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5_data.csv")
global_data                 <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")
sum25_songmeters            <- read.csv("clean_data/datasets/indices_datasets/songmeters25_data.csv")

global_singledevice_RL      <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
continuous_singledevice_RL  <- read.csv("clean_data/datasets/indices_datasets/continuousRL_data.csv")
multi25_RL                  <- read.csv("clean_data/datasets/indices_datasets/multi25RL_data.csv")
multi25_1in5_RL             <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5RL_data.csv")
global_data_RL              <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")
sum25_songmeters_RL         <- read.csv("clean_data/datasets/indices_datasets/songmeters25RL_data.csv")

### Store datasets in a named list ------------------
datasets <- list(
  "Global Single Device"            = global_singledevice,
  "Continuous"                      = continuous_singledevice,
  "Multi-device 2025"               = multi25,
  "Multi-Device 2025 (1 in 5)"      = multi25_1in5,
  "Global"                          = global_data,
  "Songmeters 2025"                 = sum25_songmeters,
  
  "[RL] Global Single Device"       = global_singledevice_RL,
  "[RL] Continuous"                 = continuous_singledevice_RL,
  "[RL] Multi-device 2025"          = multi25_RL,
  "[RL] Multi-Device 2025 (1 in 5)" = multi25_1in5_RL,
  "[RL] Global"                     = global_data_RL,
  "[RL] Songmeters 2025"            = sum25_songmeters_RL
)

### Functions  --------------------------------------------------------------------------------------------------------

#### Function to add leading zeros to time values for seeking audio files  ------------------
format_time_for_seeking <- function(time_value) {
  sprintf("%06d", as.numeric(time_value))  # Ensure time is always 6 digits long
}

# Ensures that time column is numeric for all datasets
# Apply mutate(Time = as.numeric(Time)) to each dataset
global_singledevice        <- global_singledevice        %>% mutate(Time = as.numeric(Time))
continuous_singledevice    <- continuous_singledevice    %>% mutate(Time = as.numeric(Time))
multi25                    <- multi25                    %>% mutate(Time = as.numeric(Time))
multi25_1in5               <- multi25_1in5               %>% mutate(Time = as.numeric(Time))
global_data                <- global_data                %>% mutate(Time = as.numeric(Time))
sum25_songmeters           <- sum25_songmeters           %>% mutate(Time = as.numeric(Time))

global_singledevice_RL     <- global_singledevice_RL     %>% mutate(Time = as.numeric(Time))
continuous_singledevice_RL <- continuous_singledevice_RL %>% mutate(Time = as.numeric(Time))
multi25_RL                 <- multi25_RL                 %>% mutate(Time = as.numeric(Time))
multi25_1in5_RL            <- multi25_1in5_RL            %>% mutate(Time = as.numeric(Time))
global_data_RL             <- global_data_RL             %>% mutate(Time = as.numeric(Time))
sum25_songmeters_RL        <- sum25_songmeters_RL        %>% mutate(Time = as.numeric(Time))


#### Define available dataframes   -----------------------------
dataframes <- c(
  "Global Single Device",
  "Continuous",
  "Multi-device 2025",
  "Multi-Device 2025 (1 in 5)",
  "Global",
  "Songmeters 2025",
  
  "[RL] Global Single Device",
  "[RL] Continuous",
  "[RL] Multi-device 2025",
  "[RL] Multi-Device 2025 (1 in 5)",
  "[RL] Global",
  "[RL] Songmeters 2025"
)

#### Define available time-ranges  -----------------------------
time_ranges <- list(
  "Full Day" = c("000000", "235959"),
  "Dawn"     = c("050000", "090000"),
  "Midday"   = c("103000", "143000"),
  "Dusk"     = c("153000", "193000"),
  "Midnight" = c("220000", "020000")
)

#### Define recording periods (deployments)  -----------------------------
recording_periods <- list(
  "All Periods" = c(0, 99999999),   # effectively no filter
  "Nov 2023" = c(20231116, 20231203),
  "Jan 2024" = c(20231216, 20240208),
  "Apr 2024" = c(20240401, 20240501),
  "Jun 2024" = c(20240607, 20240707),
  "Jun 2025" = c(20250605, 20250716)
)

#### Define available acoustic indices  -----------------------------
acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond", 
                      "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")

sampling_sites <- c("TaCheyHill", "TaChey", "Arai", "Oda", 
                    "KnaongBatSa", "TaSay", "Kronomh", "DamFive", 
                    "TangRang", "Kravanh Bridge", "PursatTown")

#### Definte Site Ordering:   -----------------------
site_order <- c("TaCheyHill", "TaChey", "Arai", "Oda", 
                "KnaongBatSa", "TaSay", "Kronomh", 
                "DamFive", "TangRang", "Kravanh Bridge", "PursatTown")

  
#### Define a fixed color palette for each site  -----------------------
site_colors <- c(
  "TaCheyHill" = "#103004",
  "TaChey" = "#103F96",
  "Arai" = "#3DA9C7", 
  "Oda" = "#53D4FF",
  "KnaongBatSa" = "#1d601d",
  "TaSay" = "#7DBC62", 
  "Kronomh" = "#74EAA3", 
  "DamFive" = "#b2a539",  
  "TangRang" = "#e8d642",
  "Kravanh Bridge" = "#b5473a",  
  "PursatTown" = "#f87060"   
)

site_colors2 <- c(
  "TaCheyHill" = "#2b4c0e",
  "TaChey" = "#26870f",
  "Arai" = "#30c111", 
  "Oda" = "#afbc1e",
  "KnaongBatSa" = "#007376",
  "TaSay" = "#3bb7b9", 
  "Kronomh" = "#2cb897", 
  "DamFive" = "#b2a539",  
  "TangRang" = "#d1e027",
  "Kravanh Bridge" = "#cf6f31",  
  "PursatTown" = "#bb431d"   
)

#### Month Colours  -----------------------

  # Define full year of months
    month_levels <- month.name  # "January", ..., "December"
  # Custom palette for monsoon cycle:
    # Jan = greenish-blue, Apr = orange, Jun = blue, Jul = blue (distinct), Dec = greenish-blue
    month_anchors <- c(
      "January"   = "#5cd66b",   # greenish-blue
      "April"     = "#f46d43",   # orange
      "June"      = "#48a4d3",   # dark blue
      "July"      = "#629dff",   # lighter blue
      "December"  = "#62ffe3"    # greenish-blue (different from Jan)
    )
  # Interpolate smoothly across all 12 months
  month_colors <- colorRampPalette(month_anchors)(12)
  names(month_colors) <- month_levels
  
  
#### Period Colours  -----------------------
  
  
  # Custom palette for monsoon cycle:
  # Jan = greenish-blue, Apr = orange, Jun = blue, Jul = blue (distinct), Dec = greenish-blue
period_anchors <- c(
    "Nov 2023" = "blue",
    "Jan 2024" = "purple",
    "Apr 2024" = "cyan",
    "Jun 2024" = "green",
    "Jun 2025" = "pink"
  )
  
  
#### QBR Colors: continuous gradient red → green -----------------------
  qbr_order <- c("Natural (95–100)", "Good (75–90)", "Fair (55–70)", "Poor (30–50)", "Bad (<25)")
  
  strahler_order <- c("1st Order", "2nd Order", "3rd Order", "4th Order", "5th Order")
  
  # QBR colours
  qbr_colors <- c(
    "Natural (95–100)" = "blue",
    "Good (75–90)"     = "green",
    "Fair (55–70)"     = "gold",
    "Poor (30–50)"     = "orange",
    "Bad (<25)"        = "red"
  )
  
  # Strahler colours
  strahler_colors <- c(
    "1st Order" = "purple",
    "2nd Order"   = "blue",
    "3rd Order"  = "green",
    "4th Order" = "gold",
    "5th Order" = "orange"
  )


# -----------------------------------
#### UI - Layout and Interactive Elements  ----------------------------
  
# Sets up the page  
ui <- fluidPage(
  useShinyjs(), # Enables Javascript functionality
  tags$head(
    # Including external JavaScript Libraries for Wavesurfer Audio Visualisation - referencing files in www/js folder
    tags$script(src = "js/wavesurfer.min.js"),
    tags$script(src = "js/spectrogram.min.js"),
    # Custom CSS for Styling Waveform and Spectrogram display
    tags$style(HTML("#waveform { 
                    width: 100% !important; 
                    height: 100px !important; 
                    margin-top: 10px; 
                    border: 1px solid #ccc; 
                    } 
                    #spectrogram { 
                    width: 100% !important; 
                    height: 150px !important; 
                    margin-top: 10px; 
                    border: 1px solid #ccc; 
                    } 
                    #now_playing { 
                    margin-top: 20px; 
                    padding: 10px; 
                    background-color: #f9f9f9; 
                    border: 1px solid #ccc; 
                    font-size: 14px; 
                    width: 100%; 
                    }")),
    
    # Add this new CSS for hiding buttons
    tags$style(HTML("
    .hidden { display: none; }
  "))
  ),
  
  # Main PCA plot - spans both the sidebar and main panel
  fluidRow(
    column(12, 
           div(style = "position: absolute; top: 10px; left: 10px; 
            background: rgba(255,255,255,0.85); /* slightly transparent */
            padding: 10px; border-radius: 8px; 
            box-shadow: 0 2px 6px rgba(0,0,0,0.2); 
            z-index: 10; width: 250px;",
               
               # Thin Yves Klein Blue toggle bar
               actionButton("toggle_controls", label = NULL,
                            style = "
                   width: 100%; 
                   height: 8px; 
                   padding: 0; 
                   margin-bottom: 5px;
                   background-color: #002FA7; 
                   border: none;
                   border-radius: 4px;
                   cursor: pointer;
                 "),
               
  # Collapsible Primary Control Panel that is floating
               div(id = "controls_panel",
                   
                   # Select Dataframe
                   div("Select Dataframe:", style = "font-size: 12px; margin-bottom: 2px;"),
                   selectInput("selected_dataframe", label = NULL, choices = dataframes, selected = dataframes[1]),
                   
                   # Recording Period
                   div("Recording Period:", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
                   selectInput("selected_period", label = NULL, choices = names(recording_periods), selected = recording_periods),
                   
                   # Select Time
                   div("Select Time:", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
                   selectInput("selected_time", label = NULL, choices = names(time_ranges), selected = "Full Day"),
                   
                   # Select Site(s)
                   div("Select Site(s):", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
                   selectInput("selected_sites", label = NULL, choices = sampling_sites, 
                               selected = sampling_sites, multiple = TRUE),
                   
                   # Acoustic Indices
                   div("Acoustic Indices:", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
                   selectInput("selected_indices", label = NULL, choices = acoustic_indices, multiple = TRUE),
                   
                   # Colour By
                   div("Colour By:", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
                   selectInput("color_by", label = NULL, choices = c("Site", "Month", "QBR_Class", "Strahler_Class"), selected = "Site"),
                   
                   # Add functionality buttons
                   actionButton("compute", "Compute", class = "btn-primary")
                  )
                ),
      # Plot Here
      plotlyOutput("main_plot", height = "600px")
      )
    ),
  
  # PCA Popup (only appears when >3 indices selected)
  div(style = "position: absolute; top: 10px; left: 280px; 
           background: rgba(255,255,255,0.85);
           padding: 10px; border-radius: 8px; 
           box-shadow: 0 2px 6px rgba(0,0,0,0.2); 
           z-index: 10; width: 250px;",
      
      # Thin toggle bar
      conditionalPanel(
        condition = "input.selected_indices != null && input.selected_indices.length > 3",
        actionButton("toggle_pca_controls", label = NULL,
                     style = "
                     width: 100%; height: 8px; padding: 0; margin-bottom: 5px;
                     background-color: #002FA7; border: none;
                     border-radius: 4px; cursor: pointer;"),
        
        div(id = "pca_controls_panel",
            div("Plot Type:", style = "font-size: 12px; margin-bottom: 2px;"),
            selectInput("plot_type", label = NULL, 
                        choices = c("Scatter 3D", "Scatter 2D", "Diel Line 2D", "Diel Line 3D", "Boxplot"), 
                        selected = "Scatter 3D"),
            
            div("PCA Axes:", style = "font-size: 12px; margin-bottom: 2px; margin-top: 5px;"),
            
            # 3D Scatter plot (PC vs PC vs PC)
            conditionalPanel(
              condition = "input.plot_type == 'Scatter 3D'",
              selectInput("pca_x", "X-axis", choices = paste0("PC", 1:10), selected = "PC1"),
              selectInput("pca_y", "Y-axis", choices = paste0("PC", 1:10), selected = "PC2"),
              selectInput("pca_z", "Z-axis", choices = paste0("PC", 1:10), selected = "PC3")
            ),
            
            # --- add conditionalPanel for Scatter 2D ---
            conditionalPanel(
              condition = "input.plot_type == 'Scatter 2D'",
              selectInput("pca_x", "X-axis", choices = paste0("PC", 1:10), selected = "PC1"),
              selectInput("pca_y", "Y-axis", choices = paste0("PC", 1:10), selected = "PC2")
            ),
            
            # Diel Line 2D (time vs PC)
            conditionalPanel(
              condition = "input.plot_type == 'Diel Line 2D'",
              selectInput("pca_y", "Y-axis (PC)", choices = paste0("PC", 1:10), selected = "PC1")
            ),
            
            # Diel Line 3D (time vs PC1 vs PC2)
            conditionalPanel(
              condition = "input.plot_type == 'Diel Line 3D'",
              selectInput("pca_y", "Y-axis (PC)", choices = paste0("PC", 1:10), selected = "PC1"),
              selectInput("pca_z", "Z-axis (PC)", choices = paste0("PC", 1:10), selected = "PC2")
            ),
            
            # Boxplot (single PC vs Group)
            conditionalPanel(
              condition = "input.plot_type == 'Boxplot'",
              selectInput("pca_y", "PC for Boxplot", choices = paste0("PC", 1:10), selected = "PC1")
            ),
            
            actionButton("compute", label = NULL,
                         style = "
                   width: 100%; 
                   height: 8px; 
                   padding: 0; 
                   margin-bottom: 5px;
                   background-color: #002FA7; 
                   border: none;
                   border-radius: 4px;
                   cursor: pointer;
                 ")
        )
      )
  ),       

  
#### Bot Row - UI Controls and Audio Visualization. Seems like 12 is max width. Structured around that ----------------
  fluidRow(
    column(4, 
           div(id = "now_playing",
               span(id = "now_playing_text", "Now Playing: "),
               
               # Buttons stacked top-right, initially hidden
               div(id = "buttons_container", class = "hidden",
                   style = "position: absolute; top: 10px; right: 10px; display: flex; flex-direction: column;",
                   actionButton("play_pause", label = NULL, icon = icon("play"), class = "btn-primary btn-sm", style = "margin-bottom: 5px;"),
                   actionButton("open_file", label = NULL, icon = icon("folder-open"), class = "btn-secondary btn-sm")
               ),
               style = "position: relative; padding: 10px; background-color: #f9f9f9; 
               border: 1px solid #ccc; font-size: 14px; width: 100%; border-radius: 8px;"
           ), 
            div(style = "height: 200px; overflow-y: auto; padding: 10px; border-radius: 8px;", verbatimTextOutput("pca_summary")),
           # PCA Results Panel
           div(
             id = "pca_results_panel",
             style = "margin-top: 15px;",
             h4("PCA Results"),
             verbatimTextOutput("pca_results"),
             downloadButton("download_pca", "Export PCA Results")
           ),
    ),
    column(8, 
           div(id = "waveform"), # Placeholder for waveform display
           div(id = "spectrogram") # Placeholder for spectrogram display
    )
  )
)

# -----------------------------------
# SERVER ------------------------------
current_audio <- reactiveVal(NULL)

server <- function(input, output, session) {
  
  selected_data <- reactive(datasets[[input$selected_dataframe]])
  
  # Single Index Data
  filtered_data <- reactive({
    df <- selected_data()
    tr <- as.numeric(time_ranges[[input$selected_time]])
    if (tr[1] > tr[2]) subset(df, Time >= tr[1] | Time <= tr[2])
    else subset(df, Time >= tr[1] & Time <= tr[2])
  })
  
  
  # PCA Data
  full_pca_data <- reactive({
    df <- datasets[[input$selected_dataframe]]  # full dataset
    inds <- input$selected_indices
    # Filter only by selected sites
    if (!is.null(input$selected_sites) && length(input$selected_sites) > 0) {
      df <- df %>% filter(Site %in% input$selected_sites)
    }
    # Only compute PCA if more than 3 indices
    if (length(inds) <= 3) return(NULL)
    # Compute PCA
    pca <- prcomp(df %>% select(all_of(inds)), center = TRUE, scale. = TRUE)
    scores <- as.data.frame(pca$x)
    # attach metadata ensuring no duplicate names
    scores <- bind_cols(df, scores[, !(names(scores) %in% names(df)), drop = FALSE])
    list(scores = scores, pca = pca)
  })
  
  # --- Reactive: subset data for plotting (filter by time/period) ---
  plotting_data <- reactive({
    full_scores <- full_pca_data()$scores  # full PCA scores with metadata
    pca_obj <- full_pca_data()$pca        # full PCA object
    inds <- input$selected_indices
    df <- full_scores
    
    # --- Time filter ---
    tr <- as.numeric(time_ranges[[input$selected_time]])
    if (tr[1] > tr[2]) {
      df <- subset(df, Time >= tr[1] | Time <= tr[2])
    } else {
      df <- subset(df, Time >= tr[1] & Time <= tr[2])
    }
    
    # --- Recording period filter ---
    if (!is.null(input$selected_period)) {
      pr <- recording_periods[[input$selected_period]]
      df <- df %>% filter(Date >= pr[1] & Date <= pr[2])
    }
    
    # --- Project filtered data onto full PCA ---
    df_proj <- as.data.frame(predict(pca_obj, newdata = df[inds]))
    # attach metadata ensuring no duplicate names
    df_proj <- bind_cols(df, df_proj[, !(names(df_proj) %in% names(df)), drop = FALSE])  
    
    df_proj
  })
  
  

# --------------------------
# Plotting
# --------------------------  
  
  # Compute plot
  plot_results <- eventReactive(input$compute, {
    
    inds <- input$selected_indices
    n_inds <- length(inds)
    colvar <- input$color_by
    
    # --- Choose data based on number of indices ---
    if (n_inds <= 3) {
      data <- filtered_data()
    } else {
      data <- plotting_data()
      if (is.null(data)) return(NULL)  # safety
    }
    
    # --- Prepare color vector ---
    if (colvar == "Site") {
      color_vec <- factor(data$Site, levels = site_order)
      pal <- site_colors
    } else if (colvar == "Month") {
      data$Month <- factor(month.name[as.numeric(substr(data$Date, 5, 6))],
                           levels = month_levels)
      color_vec <- data$Month
      pal <- month_colors
    } else if (colvar == "QBR_Class") {
      color_vec <- factor(data$QBR_Class, levels = qbr_order)
      pal <- qbr_colors
    } else if (colvar == "Strahler_Class") {
      color_vec <- factor(data$Strahler_Class, levels = strahler_order)
      pal <- strahler_colors
    } else {
      color_vec <- "black"
      pal <- NULL
    }
    
    # --- Format Time for hover / audio keys ---
    data$Time_fmt <- sprintf("%06d", as.numeric(data$Time))
    
    # --- Plot branches ---
    if (n_inds == 1) {
      # Boxplot
      p <- plot_ly(data, x = ~color_vec, y = data[[inds[1]]],
                   type = "box",
                   color = color_vec,
                   colors = pal)
      
    } else if (n_inds == 2) {
      # 2D scatter
      p <- plot_ly(data, x = data[[inds[1]]], y = data[[inds[2]]],
                   type = "scatter", mode = "markers", marker = list(size = 2),
                   color = color_vec, colors = pal,
                   text = ~paste("Site:", Site,
                                 "<br>Date:", Date,
                                 "<br>Device:", Device,
                                 "<br>Time:", Time_fmt),
                   key = ~paste0("http://localhost:8000/", Site, "/", Device, "/", Date, "/", FileName, ".wav")) %>%
        layout(xaxis = list(title = inds[1]), yaxis = list(title = inds[2]))
      
    } else if (n_inds == 3) {
      # 3D scatter
      p <- plot_ly(data, x = data[[inds[1]]], y = data[[inds[2]]], z = data[[inds[3]]],
                   type = "scatter3d", mode = "markers", marker = list(size = 2),
                   color = color_vec, colors = pal,
                   text = ~paste("Site:", Site,
                                 "<br>Date:", Date,
                                 "<br>Device:", Device,
                                 "<br>Time:", Time_fmt),
                   key = ~paste0("http://localhost:8000/", Site, "/", Device, "/", Date, "/", FileName, ".wav")) %>%
        layout(scene = list(
          xaxis = list(title = inds[1]),
          yaxis = list(title = inds[2]),
          zaxis = list(title = inds[3])
        ))
      
    } else if (n_inds > 3) {
      # --- PCA branch ---
      scores <- data
      pca_obj <- full_pca_data()$pca
      var_exp <- round(100 * (pca_obj$sdev^2 / sum(pca_obj$sdev^2)), 1)
      
      # Sets: default selection of PCs - Fall back to sensible defaults if dropdowns not ready yet
      pcx <- if (!is.null(input$pca_x)) input$pca_x else "PC1"
      pcy <- if (!is.null(input$pca_y)) input$pca_y else "PC2"
      pcz <- if (!is.null(input$pca_z)) input$pca_z else "PC3"
      # Selected PCs
      xlab <- paste0(pcx, " (", var_exp[as.numeric(sub("PC", "", pcx))], "%)")
      ylab <- paste0(pcy, " (", var_exp[as.numeric(sub("PC", "", pcy))], "%)")
      zlab <- paste0(pcz, " (", var_exp[as.numeric(sub("PC", "", pcz))], "%)")
      
      if (input$plot_type == "Scatter 3D") {
        # 3D scatter
        p <- plot_ly(scores, x = scores[[pcx]], y = scores[[pcy]], z = scores[[pcz]],
                     type = "scatter3d", mode = "markers", marker = list(size = 2),
                     color = color_vec, colors = pal,
                     text = ~paste("Site:", Site, "<br>Date:", Date, "<br>Device:", Device, 
                                   "<br>Time:", sprintf("%06d", as.numeric(Time))),
                     key = ~paste0("http://localhost:8000/", Site, "/", Device, "/", Date, "/", FileName, ".wav")) %>%
          layout(scene = list(
            xaxis = list(title = xlab),
            yaxis = list(title = ylab),
            zaxis = list(title = zlab)
          ))
        
      } else if (input$plot_type == "Scatter 2D") {
        # 2D scatter
        p <- plot_ly(scores, x = scores[[pcx]], y = scores[[pcy]],
                     type = "scatter", mode = "markers", marker = list(size = 2),
                     color = color_vec, colors = pal,
                     text = ~paste("Site:", Site, "<br>Date:", Date, "<br>Device:", Device, 
                                   "<br>Time:", sprintf("%06d", as.numeric(Time))),
                     key = ~paste0("http://localhost:8000/", Site, "/", Device, "/", Date, "/", FileName, ".wav")) %>%
          layout(
            xaxis = list(title = xlab),
            yaxis = list(title = ylab)
          )
        
        
        # ---- 2D Diel Line ----
      } else if (input$plot_type == "Diel Line 2D") {
        
        # Create Time_fmt only if it doesn't exist
        if (!"Time_fmt" %in% colnames(scores)) {
          scores <- scores %>%
            mutate(Time_fmt = sprintf("%06d", as.numeric(Time)))
        }
        
        # Convert to POSIX and compute 30-min bins
        scores <- scores %>%
          mutate(
            Time_posix = as.POSIXct(Time_fmt, format = "%H%M%S", tz = "UTC"),
            Minutes = hour(Time_posix) * 60 + minute(Time_posix),
            Time_bin = floor(Minutes / 30) * 30, # Change this for different averaging (30mins here)
            Time_label = sprintf("%02d:%02d", Time_bin %/% 60, Time_bin %% 60)
          )
        
        # Average per site / bin
        avg_scores <- scores %>%
          group_by(Time_label, !!sym(colvar)) %>%
          summarise(mean_val = mean(.data[[pcy]], na.rm = TRUE), .groups = "drop")
        
        # Plot
        p <- plot_ly(avg_scores, x = ~Time_label, y = ~mean_val,
                     type = "scatter", mode = "lines+markers",
                     line = list(shape = "spline"), marker = list(size = 2),
                     color = avg_scores[[colvar]], colors = pal) %>%
          layout(
            xaxis = list(title = "Time of Day"),
            yaxis = list(title = ylab)
          )
        
        # ---- 3D Diel Line ----
      } else if (input$plot_type == "Diel Line 3D") {
        
        # Create Time_fmt only if it doesn't exist
        if (!"Time_fmt" %in% colnames(scores)) {
          scores <- scores %>%
            mutate(Time_fmt = sprintf("%06d", as.numeric(Time)))
        }
        
        # Convert to POSIX and format as HH:MM
        scores <- scores %>%
          mutate(
            Time_posix = as.POSIXct(Time_fmt, format = "%H%M%S", tz = "UTC"),
            Time_hm = format(Time_posix, "%H:%M")
          )
        
        # Average per site / bin
        avg_scores <- scores %>%
          group_by(Time_hm, !!sym(colvar)) %>%
          summarise(
            mean_y = mean(.data[[pcy]], na.rm = TRUE),
            mean_z = mean(.data[[pcz]], na.rm = TRUE),
            .groups = "drop"
          )
        
        # 3D Plot
        p <- plot_ly(avg_scores, x = ~Time_hm, y = ~mean_y, z = ~mean_z,
                     type = "scatter3d", mode = "lines+markers", marker = list(size = 2),
                     color = avg_scores[[colvar]], colors = pal) %>%
          layout(scene = list(
            xaxis = list(title = "Time of Day"),
            yaxis = list(title = ylab),
            zaxis = list(title = zlab)
          ))
        
      } else if (input$plot_type == "Boxplot") {
        # boxplot
        pc_sel <- if (!is.null(input$pca_y)) input$pca_y else "PC1"
        ylab <- paste0(pc_sel, " (", var_exp[as.numeric(sub("PC", "", pc_sel))], "%)")
        
        p <- plot_ly(scores, x = ~color_vec, y = scores[[pc_sel]],
                     type = "box",
                     color = color_vec, colors = pal,
                     text = ~paste("Site:", Site,
                                   "<br>Date:", Date,
                                   "<br>Device:", Device,
                                   "<br>Time:", sprintf("%06d", as.numeric(Time)))) %>%
          layout(yaxis = list(title = ylab),
                 xaxis = list(title = input$color_by))
      }
      # --- PCA LOGIC END ---
    }
    
    p %>% event_register("plotly_click")
    
    p <- p %>%
      layout(
        legend = list(
          x = 1,
          y = 1,
          xanchor = "right",
          yanchor = "top",
          bgcolor = 'rgba(255,255,255,0.85)',  # slightly opaque
          borderwidth = 0,                     # no border
          font = list(size = 10),
          traceorder = "normal",
          itemsizing = "constant"              # ensures legend markers reflect trace marker
        )
      )
  })
  
  
# Reset PCA axis selectors when plot type changes
  observeEvent(input$plot_type, {
    if (input$plot_type == "Scatter 3D") {
      updateSelectInput(session, "pca_x", selected = "PC1")
      updateSelectInput(session, "pca_y", selected = "PC2")
      updateSelectInput(session, "pca_z", selected = "PC3")
    } else if (input$plot_type == "Scatter 2D") {
      updateSelectInput(session, "pca_x", selected = "PC1")
      updateSelectInput(session, "pca_y", selected = "PC2")
    } else if (input$plot_type == "Diel Line 2D") {
      # force y axis to PC1 when only one axis matters
      updateSelectInput(session, "pca_y", selected = "PC1")
    } else if (input$plot_type == "Diel Line 3D") {
      updateSelectInput(session, "pca_y", selected = "PC1")
      updateSelectInput(session, "pca_z", selected = "PC2")
    }
  })  
  
  # Allows hiding of export panel
  observeEvent(input$run_pca, {
    shinyjs::show("pca_results_panel")
  })
  
# Allows minimisation of control panel
  observeEvent(input$toggle_controls, {
    shinyjs::toggle(id = "controls_panel", anim = TRUE)
  })

# Allows minimisation of PCA Panel  
  observeEvent(input$toggle_pca_controls, {
    shinyjs::toggle(id = "pca_controls_panel", anim = TRUE)
  })
  
# Allows file seeking for opening the folder
     observeEvent(input$open_file, {
       
    # The URL currently being used by WaveSurfer
       url <- current_audio()
       if (is.null(url)) return()
       
    # Translate back to the local path -------------------------
    # Strip off the localhost URL base
    rel_path <- sub("^http://localhost:8000/", "", url)
    
    # Define the root directory where your audio files live - NEEDS CHANGES IF CHANGE DEVICE
    base_dir <- "/Volumes/SSD Type II/Acoustics/CCMP Riparian Audio"
    
    # Construct full path
    file_path <- file.path(base_dir, rel_path)
    
    # Open & highlight depending on OS -------------------------
    if (.Platform$OS.type == "windows") {
      system2("explorer", paste0('/select,"', normalizePath(file_path, winslash = "\\"), '"'))
      
    } else if (Sys.info()[["sysname"]] == "Darwin") {
      system2("open", c("-R", shQuote(file_path)))
      
    } else {
      system2("xdg-open", dirname(file_path))
    }
  })
  
# PLot
output$main_plot <- renderPlotly(plot_results())
  
  # PCA summary
output$pca_summary <- renderPrint({
  inds <- input$selected_indices
  if (length(inds) > 3) {
    # extract dataframe
    df_for_pca <- full_pca_data()$scores
    
    # compute PCA
    pca <- prcomp(df_for_pca %>% select(all_of(inds)), center = TRUE, scale. = TRUE)
    
    cat("PCA Summary:\n")
    print(summary(pca)$importance)  # all PCs
    cat("\nPCA Loadings (all PCs):\n")
    print(round(pca$rotation, 3))   # all loadings
  } else {
    cat("PCA Summary available only when >3 indices selected.")
  }
})

### Server: Downloader
output$download_pca <- downloadHandler(
  filename = function() {
    paste0("PCA_export_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # --- Filtered data ---
    data <- full_pca_data()$scores
    inds <- input$selected_indices
    
    if (length(inds) <= 3) {
      showNotification("PCA export only available when >3 indices selected", type = "error")
      return(NULL)
    }
    
    # --- Compute PCA ---
    pca <- prcomp(data %>% select(all_of(inds)), center = TRUE, scale. = TRUE)
    scores <- as.data.frame(pca$x)
    
    # --- Bind metadata ---
    pca_export <- cbind(
      data %>% select(file_id, Site, Device, Date, Time, 
                      Strahler, QBR_Score, QBR_Class, 
                      Strahler_Class, Deployment_Season, 
                      Branch, Year, Month, FileName),
      scores
    )
    
    # --- Write CSV ---
    write.csv(pca_export, file, row.names = FALSE)
  }
)
  
  
# Click → Audio Functionality -------------------
observe({
  click <- event_data("plotly_click")
  if (!is.null(click)) {
    
    data_clicked <- NULL
    url <- NULL
    
    # --- Scatter / 3D / PCA plots ---
    if (!is.null(click$key)) {
      url <- click$key
      current_audio(url)  # store currently selected audio
      
      parts <- strsplit(url, "/")[[1]]
      site <- parts[4]; device <- parts[5]; date <- parts[6]
      time <- substr(gsub(".wav", "", basename(parts[7])), 10, 15)
      info_html <- paste0("<div><strong>Site:</strong> ", site, 
                          " | <strong>Device:</strong> ", device, "</div>",
                          "<div><strong>Date:</strong> ", date,
                          " | <strong>Time:</strong> ", time, "</div>")
      session$sendCustomMessage("update_now_playing", list(info = info_html))
      updateAudio(session, url)
      
    } else {
      # --- Boxplot or Diel Line plots ---
      n_inds <- length(input$selected_indices)
      
      if (n_inds <= 3) {
        # Boxplot: use filtered_data
        index_name <- input$selected_indices[1]
        site_clicked <- click$x
        y_clicked <- click$y
        site_data <- filtered_data() %>% filter(Site == site_clicked)
        closest_row <- site_data[which.min(abs(site_data[[index_name]] - y_clicked)), ]
        data_clicked <- closest_row
        
      } else {
        # Diel Line 2D/3D: use plotting_data
        scores <- plotting_data()
        if (!is.null(scores)) {
          if (input$plot_type %in% c("Diel Line 2D", "Diel Line 3D")) {
            # Convert Time to POSIX
            scores <- scores %>%
              mutate(Time_fmt = sprintf("%06d", as.numeric(Time)),
                     Time_posix = as.POSIXct(Time_fmt, format = "%H%M%S", tz = "UTC"))
            
            # Find closest row based on click coordinates
            if (input$plot_type == "Diel Line 2D") {
              y_clicked <- click$y
              closest_row <- scores[which.min(abs(scores[[input$pca_y]] - y_clicked)), ]
            } else if (input$plot_type == "Diel Line 3D") {
              y_clicked <- click$y
              z_clicked <- click$z
              closest_row <- scores[which.min((scores[[input$pca_y]] - y_clicked)^2 + 
                                                (scores[[input$pca_z]] - z_clicked)^2), ]
            }
            data_clicked <- closest_row
          }
        }
      }
      
      # --- Construct audio URL ---
      if (!is.null(data_clicked)) {
        url <- paste0("http://localhost:8000/", data_clicked$Site, "/", data_clicked$Device, "/", data_clicked$Date, "/",
                      data_clicked$Date, "_", sprintf("%06d", as.numeric(data_clicked$Time)), ".wav")
        current_audio(url)
        
        info_html <- paste0("<div><strong>Site:</strong> ", data_clicked$Site, 
                            " | <strong>Device:</strong> ", data_clicked$Device, "</div>",
                            "<div><strong>Date:</strong> ", data_clicked$Date,
                            " | <strong>Time:</strong> ", data_clicked$Time, "</div>")
        session$sendCustomMessage("update_now_playing", list(info = info_html))
        updateAudio(session, url)
      }
    }
  }
})


  
# Wavesurfer Integration -------------------------------
  shinyjs::runjs("
    $(document).ready(function() {
    
    var audioCtx = new (window.AudioContext || window.webkitAudioContext)({ sampleRate: 48000 });
        
      var wavesurfer = WaveSurfer.create({
        container: '#waveform',
        waveColor: 'violet',
        progressColor: 'purple',
        cursorColor: 'black',
        height: 100,
        
        plugins: [WaveSurfer.Spectrogram.create({
          container: '#spectrogram', 
          fftSamples: 512, 
          labels: true,
          frequencyMax: 4000
          })]
      });
      
      
      var isPlaying = false;
      
      $('#play_pause').click(function() {
        isPlaying ? wavesurfer.pause() : wavesurfer.play(); 
        isPlaying = !isPlaying; 
      });
      
      Shiny.addCustomMessageHandler('update_audio', function(msg) {
        wavesurfer.load(msg.src);
        
        wavesurfer.on('ready', function() { wavesurfer.play(); isPlaying = true; });
      });
      
      
        Shiny.addCustomMessageHandler('update_now_playing', function(msg) {
          $('#now_playing_text').html('Currently Playing: ' + msg.info);
          $('#buttons_container').removeClass('hidden'); // reveal buttons when audio is selected
  });

    });
  ")
  
  updateAudio <- function(session, src) session$sendCustomMessage("update_audio", list(src = src))
}

# This runs it ---------------------
shinyApp(ui, server)

# runApp("waveapptest.R")
