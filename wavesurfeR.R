library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  fileInput("audio_file", "Choose a WAV file", accept = ".wav"),
  actionButton("play_pause", "Play / Pause"),
  
  tags$div(id = "waveform"),
  tags$div(id = "spectrogram"),
  tags$div(id = "now_playing", tags$span(id = "now_playing_text", "No file selected")),
  
  tags$head(
    tags$script(src = "js/wavesurfer.min.js"),
    tags$script(src = "js/spectrogram.min.js"),
    tags$style(HTML("
      #waveform { width: 100% !important; height: 100px !important; margin-top: 10px; border: 1px solid #ccc; }
      #spectrogram { width: 100% !important; height: 150px !important; margin-top: 10px; border: 1px solid #ccc; }
      #now_playing { margin-top: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ccc; font-size: 14px; width: 100%; }
      .hidden { display: none; }
    ")),
    
    # JS directly in head, avoids shinyjs session issues
    tags$script(HTML("
$(document).ready(function() {
  var wavesurfer = WaveSurfer.create({
    container: '#waveform',
    waveColor: 'violet',
    progressColor: 'purple',
    cursorColor: 'black',
    height: 100,
    sampleRate: 44100,
    
    plugins: [
      WaveSurfer.Spectrogram.create({
        container: '#spectrogram',
        fftSamples: 2048,
        labels: true,
        frequencyMax: 22050  // temporary; will fix on ready
      })
    ]
  });

  var spectrogramPlugin = wavesurfer.getActivePlugins()[0]; // get reference to spectrogram
  var isPlaying = false;

  $(document).on('click', '#play_pause', function() {
    isPlaying ? wavesurfer.pause() : wavesurfer.play(); 
    isPlaying = !isPlaying;
  });

  Shiny.addCustomMessageHandler('update_audio', function(msg) {
    if (wavesurfer.isPlaying()) { wavesurfer.stop(); isPlaying = false; }

    var blob = new Blob([new Uint8Array(msg.data)], { type: 'audio/wav' });
    var url = URL.createObjectURL(blob);
    wavesurfer.load(url);

    wavesurfer.on('ready', function() { 
      // Update frequencyMax based on actual audio sample rate
      spectrogramPlugin.params.frequencyMax = wavesurfer.backend.ac.sampleRate / 2;
      spectrogramPlugin.drawBuffer();  // force redraw

      wavesurfer.play(); 
      isPlaying = true; 
    });
  });

  Shiny.addCustomMessageHandler('update_now_playing', function(msg) {
    $('#now_playing_text').html('Currently Playing: ' + msg.info);
  });
});
"))
  )
)

server <- function(input, output, session) {
  observeEvent(input$audio_file, {
    req(input$audio_file)
    
    # Read the file as raw bytes
    file_data <- readBin(input$audio_file$datapath, "raw", n = file.info(input$audio_file$datapath)$size)
    
    # Send to JS
    session$sendCustomMessage("update_audio", list(data = as.integer(file_data)))
    session$sendCustomMessage("update_now_playing", list(info = input$audio_file$name))
  })
}

shinyApp(ui, server)
