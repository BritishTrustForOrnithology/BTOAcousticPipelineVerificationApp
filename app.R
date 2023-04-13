  #' A Shiny App to facilitate verification of audio classification
#' Simon Gillings BTO
#' April 2023

#load functions
source('functions.R')

#imports
require(audio)
require(signal)
require(scales)
require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
require(ini)


#read and parse the ini settings
settings <- parse_settings(read.ini('settings.ini'))
settings$max_clip_duration <- 10

#make empty history dataset
history_start <- data.frame('file' = character(), 
                      'origin' = character(),
                      'destination' = character(), 
                      stringsAsFactors = FALSE)

#get the drive letters
volumes <- getVolumes()()

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force=TRUE),
  
  tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
  
  
  # Application title
  titlePanel(windowTitle = "BTO Acoustic Pipeline Tools - clip verification",
  title = div(img(src="APlogo50px.png"), "BTO Acoustic Pipeline Tools - clip verification", style="font-size:50px; color: #31566d;")),

  tabsetPanel(
    tabPanel(
      title = "Introduction",
      div(
        style="width:800px;",
        tags$h1('Purpose'),
        tags$p('This app is designed to facilitate manual verification of short (<10s) audio 
               clips stored locally on a computer or media disc. Each clip can be moved 
               to a subfolder based on button presses (e.g. True or False identity).'),
        tags$br(),
        tags$h1('How to use'),
        tags$p('The Main tab contains options to select a folder containing audio clips. The app 
                will automatically show a spectrogram for the first clip in the folder. The user 
                can select a specific clip using the dropdown menu, or use navigation buttons to
                step through the clips.'),
        tags$p('Each spectrogram is accompanied by a sound player widget so the user can listen to
                the sound clip. Blue buttons allow the user to verify the clip: by default the choices 
                are: True, False or Uncertain (but see below). Upon clicking one of these options the 
                audio clip will be moved to a subfolder of that name. In this way a folder 
                of audio provisionally identified as species X can be quickly and easily 
                moved into True and False subfolders.'),
        tags$p('The Settings tab allows various features of the app to be customised. In 
               particular the user can change the set of validation buttons to suit their 
               needs. For example, a user may wish to have buttons for individual species or
               call types.'),
        tags$p('The Spectrogram Settings button opens a panel of settings to customise the spectrogram image.
               Users can change colour themes, adjust the window length and overlap to better \'focus\' the image, 
               and adjust the frequency axis range.'),
        tags$p('Advanced Users: to preserve settings at startup, carefully modify the settings.ini file.'),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$p('App by Simon Gillings, BTO')
      )
    ),
    
    tabPanel("Main",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(width = 2,
                            shinyDirButton(id = 'path_audio', 
                                           label = ' Select audio folder',
                                           icon = icon('folder-open', verify_fa=FALSE),
                                           title = 'Select folder containing original audio files',
                                           class = "btn-primary"
                                           ),
                            div(style="color: #A42A04; font-weight: bold; padding-top: 10px; padding-bottom: 20px; width:250px; word-wrap: break-word;",
                              textOutput("path_audio")
                            ),
                            actionButton(inputId = 'btn_scan', 
                                         label = div('Start audit', icon('play', verify_fa=FALSE)),
                                         class = "btn-primary"),
                            tags$br(),
                            tags$br(),
                            div(style="color: #337ab7;",
                            selectInput(inputId = 'file_picker', 
                                        label = 'Pick a file (or use green navigation buttons)', 
                                        choices = NULL)
                            )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 
                 div(textOutput(outputId = 'file_count'), style="color: #A42A04; font-weight: bold;"),
                 div(textOutput(outputId = 'file_current'), style="color: #A42A04; font-weight: bold;"),
                 div(
                   div(
                     style="float: left;;",
                     plotOutput("spec", width = "800px", height="600px")
                   ),
                   actionButton(inputId = 'spec_settings', 
                                label = div(icon('cog'), 'Spectrogram Settings'), 
                                class = "btn-primary"),
                   tags$div(
                     id='settings_panel',
                     style="float: left; width: 320px;",
                     selectInput(inputId = 'speccolour', 
                                 label = 'Spectrogram theme', 
                                 choices = c('Greyscale','Viridis','Red', "Blue", "Green", "Warm"), 
                                 selected = settings$spec_theme),
                     sliderInput(inputId = 'window_size',
                                 label = 'Window size', 
                                 min = 128, max = 2048,
                                 value = 1024),
                     sliderInput(inputId = 'overlap',
                                 label = 'Window overlap (%)',
                                 min = 5, max = 95, step = 5, value = 75),
                     sliderInput(inputId = 'y_range',
                                 label = 'Frequency axis range (kHz)', 
                                 min = 0, max = 11,
                                 value = c(settings$spec_ymin, max=settings$spec_ymax),
                                 step = 1),
                   ),
                   div(
                     id='controls',
                     style="float: left; padding-top:20px; width: 320px;",
                     div(
                       style="text-align:center;",
                       actionButton(inputId = "btn_first", 
                                    label = div(icon('caret-left', verify_fa=FALSE), "First"), 
                                    style="margin-top:10px; margin-bottom:10px; background-color: green; color: white;"),
                       actionButton(inputId = "btn_previous", 
                                    label = div(icon('angle-left', verify_fa=FALSE), "Previous"), 
                                    style="margin-top:10px; margin-bottom:10px; background-color: green; color: white;"),
                       actionButton(inputId = 'btn_next', 
                                    label = div("Next", icon('angle-right', verify_fa=FALSE)),
                                    style="margin-top:10px; margin-bottom:10px; background-color: green; color: white;"),
                       actionButton(inputId = 'btn_last', 
                                    label = div("Last", icon('caret-right', verify_fa=FALSE)),
                                    style="margin-top:10px; margin-bottom:10px; background-color: green; color: white;")
                     ),
                     div(
                       style="text-align:center;",
                       uiOutput(outputId = 'player')
                     ),
                     div(id='checks',
                       tags$h4("Select an option to move the clip", style="padding-top: 20px; color: #337ab7;"),
                       tags$p("[Edit/add options in Settings panel]", style="font-size:12px; color: #337ab7;"),
                       style="text-align:center;",
                       uiOutput("validation_buttons")
                     ),
                     div(id='undo',
                         style="text-align:center;",
                         actionButton(inputId = 'btn_undo', icon=icon('rotate-left', verify_fa=FALSE),
                                      label = "Undo file move", 
                                      style="margin-top:10px; margin-bottom:10px; background-color: red; color: white;")
                     )
                   )
                  ) 
                 
                 
               )
             )
    ),
    
  tabPanel(
    title = "Settings", 
      textInput(inputId = 'validation_buttons_list', 
                label = 'Enter the list of validation buttons you require, as an unquoted comma-separated list:', 
                value = settings$vfchoices, width = "800px"),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$p('Advanced Users: to preserve settings at startup, carefully modify the settings.ini file')
    ),
  
  tabPanel("Exit", 
           tags$br(),
           tags$br(),
           tags$br(),
           actionButton("exit2", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);")
  )

    
  )  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  hide('settings_panel')
  hide('file_picker')
  hide('btn_scan')
  hide('spec')
  hide('controls')
  hide('checks')
  hide('file_count')
  hide('file_current')
  hide('spec_settings')
  hide('btn_undo')
  
  global <- reactiveValues(
    path_audio = 'Path not set',
    files_audio = NULL,
    n_files = 0,
    file_counter = NULL,
    file_current = NULL,
    history = history_start
    )
  
  
  shinyDirChoose(input, 'path_audio', roots = volumes, session = session, filetypes = c(''))
  
  
  
  #' OBSERVERS ------------------------------------------------------------------------ 
  
  #show the history button?
  observe({
     if(nrow(global$history) > 0) show('btn_undo')
     if(nrow(global$history) == 0) hide('btn_undo')
  })
  

  #observer for the audio folder selection
  observeEvent(eventExpr = {input$path_audio}, 
               handlerExpr = {
                 #clear any existing settings
                 global$files_audio <- NULL
                 global$file_counter <- NULL
                 global$file_current <- NULL
                 #and hide mainpanel stuff until file picked
                 hide('spec')
                 hide('controls')
                 hide('checks')
                 hide('file_count')
                 hide('file_current')
                 hide('file_picker')
                 global$path_audio <- parseDirPath(volumes, input$path_audio)
                 show('btn_scan')
                 } )
  
  
  #observer to show/hide the settings for the spectrogram
  observeEvent(eventExpr = input$spec_settings, toggle('settings_panel'))
  

  #observer for the main start button
  # check dir exists and that audio there
  # update global reactive to include list of files, number of files
  # update and show the file picker dropdown
  # show the main panel image and controls
  observeEvent(eventExpr = input$btn_scan, 
               handlerExpr = {
                 files <- list.files(global$path_audio, pattern = "*.wav", full.names = FALSE)
                 
                 if(length(files)==0) {
                   #message('No such path for audio')
                   shinyalert(title = "Error",
                              text = "No audio files at this location",
                              type = "error",
                              callbackR = message('Callback: No such path for audio')
                              )
                   }
                 if(length(files) > 0) {
                   addResourcePath("audio", global$path_audio) 
                   global$files_audio <- files
                   global$n_files <- length(files)
                   updateSelectInput(session, input = "file_picker",
                                     choices = files, selected=files[1])
                   global$file_counter <- 1
                   nav_button_toggles(global$n_files, global$file_counter)
                   global$file_current <- global$files_audio[global$file_counter]
                   
                   #make/overwrite history dataset
                   global$history <- history_start
                   
                   
                   show('file_picker')
                   show('spec')
                   show('controls')
                   show('checks')
                   show('file_count')
                   show('file_current')
                   show('spec_settings')
                 }
               })
  
  #observer for the file picker dropdown
  observeEvent(eventExpr = input$file_picker, {
    global$file_current <- input$file_picker
    global$file_counter <- which(global$files_audio == input$file_picker)
  })
  
  #observers for the file navigation buttons
  observeEvent(input$btn_first, {
    global$file_counter <- 1
    global$file_current <- global$files_audio[global$file_counter]
    nav_button_toggles(global$n_files, global$file_counter)
  })
  observeEvent(input$btn_previous, {
    global$file_counter <- max(1, global$file_counter - 1)
    global$file_current <- global$files_audio[global$file_counter]
    nav_button_toggles(global$n_files, global$file_counter)
  })
  observeEvent(input$btn_next, {
    global$file_counter <- min(global$file_counter + 1, global$n_files)
    global$file_current <- global$files_audio[global$file_counter]
    nav_button_toggles(global$n_files, global$file_counter)
  })
  observeEvent(input$btn_last, {
    global$file_counter <- global$n_files
    global$file_current <- global$files_audio[global$file_counter]
    nav_button_toggles(global$n_files, global$file_counter)
  })
  
  
  #observer for the undo button
  observeEvent(input$btn_undo, {
    #get the last entry of history
    #last_item <- history[nrow(history),]
    last_item <- global$history[nrow(global$history),]
    #call the audio move function to move the clip back
    moved_back <- audio_move(file = last_item$file, 
                             path_from = last_item$destination, 
                             path_to = last_item$origin)
    #if the file was successfully moved back:
    if(moved_back==TRUE) {
      #remove the last row from history
      #history <<- history[1:nrow(history)-1,]
      global$history <- global$history[1:nrow(global$history)-1,]
      #refresh file list and number of files
      files <- list.files(global$path_audio, pattern = "*.wav", full.names = FALSE)      
      global$files_audio <- files 
      global$n_files <- length(files)
      #refresh current file
      global$file_current <- last_item$file
      #refresh file counter
      global$file_counter <- which(files==last_item$file)
      #update dropdown
      updateSelectInput(session, input = "file_picker",
                        choices = global$files_audio, selected=global$file_current)
      #updaate nav buttons
      nav_button_toggles(global$n_files, global$file_counter)
    }
    if(moved_back==FALSE) stop("Error: Failed to undo file move")
    })
  
  
  #' OUTPUTS ----------------------------------------------------------------------
  
  #show the path
  output$path_audio <- renderText({ global$path_audio })

  
  #dynamic creation of a set of validation buttons based on value list in settings panel 
  obsList <- list()  # to store observers and make sure only once is created per button
  output$validation_buttons <- renderUI({
    val_but_list <- unlist(strsplit(input$validation_buttons_list, ','))
    val_but_list <- trimws(unique(val_but_list))
    buttons <- as.list(1:length(val_but_list))
    buttons <- lapply(buttons, function(i)
    {
      btName <- paste0("validation_button_",i)
      # creates an observer only if it doesn't already exists
      if (is.null(obsList[[btName]])) {
        # make sure to use <<- to update global variable obsList
        obsList[[btName]] <<- observeEvent(input[[btName]], {
          #cat("Button ", i, "pressed\n")
          moved <- audio_move(file = global$file_current, 
                        path_from = global$path_audio, 
                        path_to = file.path(global$path_audio, val_but_list[i]))
          if(moved==TRUE) {
            #add record to history
            h1 <- data.frame('file' = global$file_current, 'origin' = global$path_audio, 'destination' = file.path(global$path_audio, val_but_list[i]), stringsAsFactors = FALSE)
            #history <<- rbind(history, h1) #<< to add to global history
            global$history <- rbind(global$history, h1) #<< to add to global history
            #print(history)
            
            #remove file from list
            global$files_audio <- global$files_audio[global$files_audio != global$file_current]
            #reduce number of files by one
            global$n_files <- global$n_files - 1
            
            #update file_current if still files to check and not on last file in list
            if(global$n_files > 0 & global$file_counter <= global$n_files) {
              global$file_current <- global$files_audio[global$file_counter]
              #update dropdown
              updateSelectInput(session, input = "file_picker",
                                choices = global$files_audio, selected=global$file_current)
            }
            #update file_current if still files to check and on last file in list
            if(global$n_files > 0 & global$file_counter > global$n_files) {
              #must reduce counter by one more to move it back a step to the last remaining file
              global$file_counter <- global$file_counter - 1
              global$file_current <- global$files_audio[global$file_counter]
              #update dropdown
              updateSelectInput(session, input = "file_picker",
                                choices = global$files_audio, selected=global$file_current)
            }
            #no files left
            if(global$n_files == 0) {
              global$file_counter <- 0
              global$file_current <- NULL
              #update dropdown
              updateSelectInput(session, input = "file_picker",
                                choices = NULL, selected=NULL)
              hide('file_count')
              hide('file_current')
              shinyalert(title = "Success",
                         text = "All files in this folder have been verified",
                         type = "success",
                         callbackR = message('Callback: No more audio in this folder')
              )
            }
            
          }

        })
      }
        actionButton(inputId=btName,
                     label=paste(val_but_list[i]), 
                     style="background-color: #337ab7; color: #fff; margin-top:5px;margin-bottom:5px;")
    }
    )
  })
  
  
  #make the spectrogram
  output$spec <- renderPlot({
    if(global$file_counter>0) {
      file1 <- file.path( global$path_audio, global$file_current )
      
      signal <- audio::load.wave(file1)
      sr <- signal$rate
      
      if(length(signal)/sr > settings$max_clip_duration) {
        shinyalert(title = 'Error: clip too long', 
                   text = paste0('This app is designed for clips of ', settings$max_clip_duration,' seconds or less. Producing longer spectrograms is very slow and best not attempted here.'),
                   type='error'
        )
      }
      if(length(signal)/sr <= settings$max_clip_duration) {
      
      
      
      #Produce the spectrogram
      spec_fast(signal = signal, 
                sr = sr,
                window_size = input$window_size, 
                overlap = input$overlap/100, 
                theme = input$speccolour, 
                ylim = input$y_range*1000)
      
      }
      
    }
  })
  
  
  #create the audio player  
  output$player <- renderUI({
    file1 <- file.path( 'audio/', global$file_current )
    tags$audio(src = file.path(file1), controls=NA,type='audio/wav')
  })
  
  
  #text for current clip
  output$file_count <- renderText(
    paste("Clip ", global$file_counter, " of ", global$n_files))
  output$file_current <- renderText(
    paste("File: ", global$file_current))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

