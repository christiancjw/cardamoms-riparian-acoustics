library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)



# Store datasets in a named list
pca_datasets <- list("November 2023 Full Soundscapes" = nov23_pca_scores, "November 2023 Dawn Chorus" = nov23_dawn_pca_scores,
                     "November 2023 Midday Soundscapes" = nov23_pca_scores, "November 2023 Dusk Chorus" = nov23_dusk_pca_scores,
                     "November 2023 Midnight Soundscapes" = nov23_midn_pca_scores, 
                     "January 2024 Full Soundscapes" = jan24_pca_scores, "January 2024 Dawn Chorus" = jan24_dawn_pca_scores, 
                     "January 2024 Midday Soundscapes" = jan24_midd_pca_scores, "January 2024 Dusk Chorus" = jan24_dusk_pca_scores,
                     "January 2024 Midnight Soundscapes" = jan24_midn_pca_scores, 
                     "April 2024 Full Soundscapes" = apr24_pca_scores, "April 2024 Dawn Chorus" = apr24_dawn_pca_scores, 
                     "April 2024 Midday Soundscapes" = apr24_midd_pca_scores, "April 2024 Dusk Chorus" = apr24_dusk_pca_scores,
                     "April 2024 Midnight Soundscapes" = apr24_midn_pca_scores, 
                     "")

# Function to format time from seconds to HHMMSS
format_time <- function(time) {
  sprintf("%06d", as.integer(time))
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # Correctly reference the files in the 'www/js' folder
    tags$script(src = "js/wavesurfer.min.js"),
    tags$script(src = "js/spectrogram.min.js"),
    tags$style(HTML("
      #waveform {
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
      }
    "))
  ),
  titlePanel("Interactive PCA with Audio"),
  fluidRow(
    # Plotly plot spans both the sidebar and main panel
    column(12, plotlyOutput("pca_plot", height = "700px"))
  ),
  fluidRow(
    # This row is split into two columns:
    # Column 1 for controls (select input, play/pause, now playing)
    column(6, 
           selectInput("selected_pca", "Choose a PCA dataset:", choices = names(pca_datasets), selected = "November PCA"),
           actionButton("play_pause", "Play / Pause", class = "btn-primary"),
           div(id = "now_playing", "Now Playing: ")  # Now Playing text
    ),
    
    # Column 2 for the WaveSurfer interface (waveform and spectrogram)
    column(6, 
           div(id = "waveform"),   # Waveform container
           div(id = "spectrogram")  # Spectrogram container
    )
  )
)

# Server
server <- function(input, output, session) {
  print(getwd())
  
  # Reactive dataset based on user selection
  selected_data <- reactive({
    pca_datasets[[input$selected_pca]]
  })
  
  # Render dynamic Plotly PCA plot
  output$pca_plot <- renderPlotly({
    plot_ly(selected_data(),
            x = ~PC1, y = ~PC2, z = ~PC3,
            color = ~Site,
            text = ~paste("Site:", Site,
                          "<br>Time:", Time,
                          "<br>Date:", Date,
                          "<br>Device:", Device),
            key = ~paste("http://localhost:8000/", Site, "/", Device, "/",
                         Date, "/", Date, "_", format_time(Time), ".wav", sep = ""),
            type = 'scatter3d',
            mode = 'markers',
            marker = list(size = 2))
  })
  
  # Play correct audio when clicking a point
  observe({
    click_data <- event_data("plotly_click")
    print(click_data)  # Inspecting the structure of click_data
    
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
  
  
  
  
  # JavaScript for WaveSurfer v7 with Spectrogram plugin
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

    function togglePlayPause() {
      if (isPlaying) {
        wavesurfer.pause();
      } else {
        wavesurfer.play();
      }
      isPlaying = !isPlaying;
    }

    // Play/Pause button
    $('#play_pause').click(function() {
      togglePlayPause();
    });

    // Update Now Playing Info
    Shiny.addCustomMessageHandler('update_audio', function(message) {
      console.log('Loading audio:', message.src);
      wavesurfer.load(message.src);
      wavesurfer.on('ready', function () {
        wavesurfer.play();
        isPlaying = true;
        console.log('Audio is playing');
      });
    });

    // Update Now Playing Box
    Shiny.addCustomMessageHandler('update_now_playing', function(message) {
      $('#now_playing').html('Currently Playing: ' + message.info);
    });
  });
  ")
  
  # Function to update the audio player source
  updateAudio <- function(session, src) {
    session$sendCustomMessage(type = 'update_audio', message = list(src = src))
  }
}

# Run the app
shinyApp(ui, server)

# Running 
# runApp("waveapp.R")

