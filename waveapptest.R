library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)

# Read Dataset
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")
jan24_fullday_data <- read.csv("clean_data/Datasets/jan24_fullday_data.csv")
apr24_fullday_data <- read.csv("clean_data/Datasets/apr24_fullday_data.csv")

# Store datasets in a named list
datasets <- list(
  "November 2023" = nov23_fullday_data,
  "January 2024" = nov23_fullday_data,
  "April 2024" = nov23_fullday_data
)

# Define available months, time-ranges, and acoustic indices
time_ranges <- list(
  "Full Day" = c("000000", "235959"),
  "Dawn" = c("050000", "090000"),
  "Midday" = c("103000", "143000"),
  "Dusk" = c("153000", "193000"),
  "Midnight" = c("220000", "020000")
)

months <- c("November 2023", "January 2024", "April 2024")

acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond", 
                      "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src = "js/wavesurfer.min.js"),
    tags$script(src = "js/spectrogram.min.js"),
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
  
  titlePanel("Interactive PCA with Audio"),
  fluidRow(
    column(12, plotlyOutput("pca_plot", height = "700px"))
  ),
  fluidRow(
    column(6, 
           selectInput("selected_month", "Choose a Month:", choices = months),
           selectInput("selected_time", "Choose a Time Range:", choices = names(time_ranges)),
           selectInput("selected_indices", "Choose Acoustic Indices:", choices = acoustic_indices, multiple = TRUE),
           actionButton("run_pca", "Run PCA", class = "btn-primary"),
           actionButton("play_pause", "Play / Pause", class = "btn-primary"),
           div(id = "now_playing", "Now Playing: ")
    ),
    column(6, 
           div(id = "waveform"),
           div(id = "spectrogram")
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_data <- reactive({
    datasets[[input$selected_month]]
  })
  
  filtered_data <- reactive({
    data <- selected_data()
    time_range <- time_ranges[[input$selected_time]]
    subset(data, Time >= time_range[1] & Time <= time_range[2])
  })
  
  pca_results <- eventReactive(input$run_pca, {
    data <- filtered_data()
    pca_data <- data %>% select(all_of(input$selected_indices))
    data_scaled <- scale(pca_data)
    prcomp(data_scaled, center = TRUE, scale. = TRUE)
  })
  
  output$pca_plot <- renderPlotly({
    pca_data <- pca_results()
    scores <- as.data.frame(pca_data$x)
    plot_ly(scores, x = ~PC1, y = ~PC2, z = ~PC3,
            color = ~filtered_data()$Site, 
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
                         filtered_data()$Time, ".wav", sep = ""), 
            type = 'scatter3d', mode = 'markers', marker = list(size = 2))
  })
  
  observe({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      audio_url <- click_data$key
      session$sendCustomMessage("update_audio", list(src = audio_url))
    }
  })
  
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
