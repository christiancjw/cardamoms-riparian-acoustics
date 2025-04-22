library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)

# Read Dataset
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")
jan24_fullday_data <- read.csv("clean_data/Datasets/jan24_fullday_data.csv")
apr24_fullday_data <- read.csv("clean_data/Datasets/apr24_fullday_data.csv")
jun24_fullday_data <- read.csv("clean_data/Datasets/jun24_fullday_data.csv")

# Store datasets in a named list
datasets <- list(
  "November 2023" = nov23_fullday_data,
  "January 2024" = jan24_fullday_data,
  "April 2024" = apr24_fullday_data,
  "June 2024" = jun24_fullday_data
)

# Function to add leading zeros to time values for seeking audio files
format_time_for_seeking <- function(time_value) {
  sprintf("%06d", as.numeric(time_value))  # Ensure time is always 6 digits long
}


# Ensures that time column is numeric for all datasets
nov23_fullday_data <- nov23_fullday_data %>%
  mutate(Time = as.numeric(Time))
jan24_fullday_data <- jan24_fullday_data %>%
  mutate(Time = as.numeric(Time))
apr24_fullday_data <- apr24_fullday_data %>%
  mutate(Time = as.numeric(Time))
jun24_fullday_data <- jun24_fullday_data %>%
  mutate(Time = as.numeric(Time))

# Define available month datasets
months <- c("November 2023", "January 2024", "April 2024", "June 2024")

# Define available time-ranges, and acoustic indices
time_ranges <- list(
  "Full Day" = c("000000", "235959"),
  "Dawn" = c("050000", "090000"),
  "Midday" = c("103000", "143000"),
  "Dusk" = c("153000", "193000"),
  "Midnight" = c("220000", "020000")
)

# Define available acoustic indices
acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond", 
                      "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")

# Definte Site Ordering:
site_order <- c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "Kronomh", "DamFive", "TangRang", "Kravanh Bridge", "PursatTown")


# Define a fixed color palette for each site
site_colors <- c(
  "TaChey" = "#562b87",
  "Arai" = "#2c90a2", 
  "Stung Oda" = "#39b9d1",
  "KnaongBatSa" = "#1d601d",
  "TaSay" = "#2ca02c", 
  "Kronomh" = "#39b9d1", 
  "DamFive" = "#b2a539",  
  "TangRang" = "#e8d642",
  "Kravanh Bridge" = "#b5473a",  
  "PursatTown" = "#f87060"   
)


# UI - Layout and Interactive Elements 
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
                    font-size: 14px; w
                    idth: 100%; 
                    }"))
  ),
  # Main PCA plot - spans both the sidebar and main panel
  fluidRow(
    column(12, plotlyOutput("pca_plot", height = "700px"))
  ),
  # UI Controls and Audio Visualization. Seems like 12 is max width. Structured around that.
  fluidRow(
    column(3, 
           selectInput("selected_month", "Choose a Month:", choices = months),
           selectInput("selected_time", "Choose a Time Range:", choices = names(time_ranges)),
           selectInput("selected_indices", "Choose Acoustic Indices:", choices = acoustic_indices, multiple = TRUE)
    ),
    column(3, 
           actionButton("run_pca", "Run PCA", class = "btn-primary"),
           actionButton("play_pause", "Play / Pause", class = "btn-primary"),
           div(id = "now_playing", "Now Playing: ") # Placeholder for "Now Playing" 
    ),
    column(6, 
           div(id = "waveform"), # Placeholder for waveform display
           div(id = "spectrogram") # Placeholder for spectrogram display
    )
  )
)

# Server - Handles data processing, PCA computation, and audio playback
server <- function(input, output, session) {
  # Select dataset based on user input
  selected_data <- reactive({
    datasets[[input$selected_month]]
  })
  # Filter dataset based on selected time range
  filtered_data <- reactive({
    data <- selected_data()
    time_range <- as.numeric(time_ranges[[input$selected_time]])  # Convert time range to numeric
    
    if (time_range[1] > time_range[2]) {  # Handles wraparound (e.g., Midnight)
      subset(data, Time >= time_range[1] | Time <= time_range[2])  # Handle midnight wrap-around
    } else {
      subset(data, Time >= time_range[1] & Time <= time_range[2])
    }
  })
  # Perform PCA when user clicks "Run PCA"
  pca_results <- eventReactive(input$run_pca, {
    data <- filtered_data()
    pca_data <- data %>% select(all_of(input$selected_indices))
    data_scaled <- scale(pca_data) # Standardize data before PCA
    prcomp(data_scaled, center = TRUE, scale. = TRUE)
  })
  # Render 3D PCA plot
  output$pca_plot <- renderPlotly({
    pca_data <- pca_results()
    scores <- as.data.frame(pca_data$x)
    site_colormap <- site_colors[filtered_data()$Site] # Assign colors based on site names
    plot_ly(scores, x = ~PC1, y = ~PC2, z = ~PC3,
            color = ~factor(filtered_data()$Site, levels = site_order),  # Orders legend
            colors = site_colors, # Ensures same colors per site
            text = ~paste(
              "Site:", filtered_data()$Site, 
              "<br>Time:", filtered_data()$Time, 
              "<br>Date:", filtered_data()$Date, 
              "<br>Device:", filtered_data()$Device), 
            key = ~paste("http://localhost:8000/", 
                         filtered_data()$Site, "/", 
                         filtered_data()$Device, "/", 
                         filtered_data()$Date, "/", 
                         filtered_data()$Date, "_", 
                         sapply(filtered_data()$Time, format_time_for_seeking), ".wav", sep = ""), # Time formatted to HHMMSS for seeking
            type = 'scatter3d', mode = 'markers', marker = list(size = 2, color = site_colormap)) 
  })
  
  # Detect click on PCA plot and update audio
  observe({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      audio_url <- click_data$key
      
      # Extract the path components from the URL (excluding the domain part)
      path_parts <- strsplit(click_data$key, "/")[[1]]
      
      # Check and print the components to see how it's structured
      print(path_parts)
      
      # Extract Site, Device, Date, Time based on the structure of the URL
      site <- path_parts[4]  # Site is the fourth part of the URL path
      device <- path_parts[5]  # Device is the fifth part
      date <- path_parts[6]  # Date is the sixth part
      time <- gsub(".wav", "", basename(path_parts[7]))  # Time is part of the filename (remove .wav)
      
      # Extract only the time (HHMMSS) from the full timestamp (YYYYMMDD_HHMMSS)
      time <- substr(time, 10, 15)  # Extract the last 6 characters (HHMMSS)
      
      # Format the information to send to the "Now Playing" box
      # Create individual rows for each piece of information (using <div> tags)
      info_html <- paste(
        "<div><strong>Site:</strong> ", site, "</div>",
        "<div><strong>Device:</strong> ", device, "</div>",
        "<div><strong>Date:</strong> ", date, "</div>",
        "<div><strong>Time:</strong> ", time, "</div>",
        sep = "\n"
      )
      
      # Send formatted string to update the Now Playing box
      session$sendCustomMessage("update_now_playing", list(info = info_html))
      
      # Update audio player with the correct source
      updateAudio(session, audio_url)
    }
  })
  
  
  # JavaScript-based audio player using WaveSurfer.js
  shinyjs::runjs("
  $(document).ready(function() {
    var wavesurfer = WaveSurfer.create({ 
    container: '#waveform', 
    waveColor: 'violet', 
    progressColor: 'purple', 
    cursorColor: 'black', 
    height: 100, 
    plugins: [
      WaveSurfer.Spectrogram.create({ 
        container: '#spectrogram', 
        fftSamples: 512, 
        labels: true 
        })
      ] 
    });
    
    var isPlaying = false;
    
    $('#play_pause').click(function() {
    isPlaying ? wavesurfer.pause() : wavesurfer.play(); 
    isPlaying = !isPlaying; });
    
    Shiny.addCustomMessageHandler(
    'update_audio', function(message) {
        wavesurfer.load(message.src); 
        wavesurfer.on('ready', function () { wavesurfer.play(); 
        isPlaying = true; }); }
      );
      
    Shiny.addCustomMessageHandler(
      'update_now_playing', 
        function(message) {
        $('#now_playing').html('Currently Playing: ' + message.info); 
     });
  });")
  
  updateAudio <- function(session, src) {
    session$sendCustomMessage("update_audio", list(src = src))
  }
}

shinyApp(ui, server)



# runApp("waveapptest.R")
