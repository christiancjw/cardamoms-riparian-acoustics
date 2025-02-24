library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)
library(pracma)
library(tidyr)
library(ggfortify)


# Function to format time from seconds to HHMMSS
format_time <- function(time) {
  sprintf("%06d", as.integer(time))
}

# Read in Datasets
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")
jan24_fullday_data <- read.csv("clean_data/Datasets/jan24_fullday_data.csv")
apr24_fullday_data <- read.csv("clean_data/Datasets/apr24_fullday_data.csv")

# Load full-day datasets
full_day_datasets <- list(
  "November 2023" = nov23_fullday_data,
  "January 2024" = jan24_fullday_data,
  "April 2024" = apr24_fullday_data
)

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src = "js/wavesurfer.min.js"),
    tags$script(src = "js/spectrogram.min.js"),
    tags$style(HTML("#waveform { width: 100% !important; height: 100px !important; margin-top: 10px; border: 1px solid #ccc; } #spectrogram { width: 100% !important; height: 150px !important; margin-top: 10px; border: 1px solid #ccc; } #now_playing { margin-top: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ccc; font-size: 14px; width: 100%; }"))
  ),
  titlePanel("Interactive PCA with Audio"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_dataset", "Choose a Dataset:", choices = names(full_day_datasets)),
      selectInput("selected_site", "Select Site:", choices = NULL, multiple = TRUE),
      selectInput("selected_time_period", "Select Time Period:", choices = c("Dawn" = "0500-0900", "Midday" = "1030-1430", "Dusk" = "1530-1930", "Midnight" = "2200-0200"))
    ),
    mainPanel(
      plotlyOutput("pca_plot", height = "700px"),
      actionButton("play_pause", "Play / Pause", class = "btn-primary"),
      div(id = "now_playing", "Now Playing: "),
      div(id = "waveform"),
      div(id = "spectrogram")
    )
  )
)

# Server
server <- function(input, output, session) {
  dataset <- reactive({
    full_day_datasets[[input$selected_dataset]]
  })
  
  observeEvent(dataset(), {
    updateSelectInput(session, "selected_site", choices = unique(dataset()$Site))
  })
  
  filtered_data <- reactive({
    dataset() %>%
      separate(Time, into = c("hour", "minute"), sep = 2, convert = TRUE) %>%
      mutate(time_numeric = hour * 100 + minute) %>%
      filter(time_numeric >= as.numeric(substr(input$selected_time_period, 1, 4)) & 
               time_numeric <= as.numeric(substr(input$selected_time_period, 6, 9)))
  })
  
  pca_result <- reactive({
    req(nrow(filtered_data()) > 0)
    pca_data <- filtered_data() %>% select(AcousticComplexity, TemporalEntropy, Ndsi, EventsPerSecond, LowFreqCover, MidFreqCover, HighFreqCover, ClusterCount, ThreeGramCount)
    prcomp(pca_data, center = TRUE, scale. = TRUE)
  })
  
  output$pca_plot <- renderPlotly({
    pca_scores <- as.data.frame(pca_result()$x)
    pca_scores$Site <- filtered_data()$Site
    pca_scores$Time <- filtered_data()$Time
    pca_scores$Date <- filtered_data()$Date
    pca_scores$Device <- filtered_data()$Device
    
    pca_plot <- plot_ly(pca_scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Site,
                        text = ~paste("Site:", Site, "<br>Time:", Time, "<br>Date:", Date, "<br>Device:", Device),
                        key = ~paste("http://localhost:8000/", Site, "/", Device, "/", Date, "/", Date, "_", format_time(Time), ".wav", sep = ""),
                        type = 'scatter3d', mode = 'markers', marker = list(size = 2))
    
    # Register 'plotly_click' event
    pca_plot <- pca_plot %>% event_register('plotly_click') 
    pca_plot
  })
  
  observe({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      audio_url <- click_data$key
      session$sendCustomMessage("update_now_playing", list(info = paste("Currently Playing:", audio_url)))
      updateAudio(session, audio_url)
    }
  })
  
  shinyjs::runjs("$(document).ready(function() { 
                 var wavesurfer = WaveSurfer.create({ 
                 container: '#waveform', waveColor: 'violet', 
                 progressColor: 'purple', cursorColor: 'black', 
                 height: 100, plugins: [ 
                 WaveSurfer.Spectrogram.create({ container: '#spectrogram', fftSamples: 512, labels: true }) ] }); 
                 var isPlaying = false; function togglePlayPause() { if (isPlaying) { wavesurfer.pause(); } 
                 else { wavesurfer.play(); } isPlaying = !isPlaying; } $('#play_pause').click(function() 
                 { togglePlayPause(); }); Shiny.addCustomMessageHandler('update_audio', function(message) 
                 { wavesurfer.load(message.src); wavesurfer.on('ready', function () { wavesurfer.play(); isPlaying = true; }); }); 
                 Shiny.addCustomMessageHandler('update_now_playing', function(message) { $('#now_playing').html(message.info); }); });")
  
  updateAudio <- function(session, src) {
    session$sendCustomMessage(type = 'update_audio', message = list(src = src))
  }
}

shinyApp(ui, server)

# runApp("wavetest.R")
