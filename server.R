#' SERVER for a Shiny App to facilitate verification of audio classification
#' Simon Gillings BTO
#' April 2023

#imports
require(audio)
require(signal)
require(scales)
require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
require(ini)
require(tuneR)

#read and parse the ini settings
settings <- parse_settings(read.ini('settings.ini'))
settings$max_clip_duration <- 10


#get the drive letters
volumes <- getVolumes()()


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
    label = NULL,
    history = history_start
    )
  
  
  shinyDirChoose(input, 'path_audio', roots = volumes, session = session, filetypes = c(''))
  
  
  
  #' OBSERVERS ------------------------------------------------------------------------ 
  
  #show/hide the filename
  observe({
    if(input$show_filename == TRUE) show('file_current')
    if(input$show_filename == FALSE) hide('file_current')
  })
  
  #show the history button?
  observe({
     if(nrow(global$history) > 0) show('btn_undo')
     if(nrow(global$history) == 0) hide('btn_undo')
  })

  #observer to hold the current custom label  
  observe({
    global$label <- input$custom_label
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
                 
                 #randomise file order?
                 if(input$random_order == TRUE) {
                   files <- sample(files, size = length(files))
                 }
                 
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
                   if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = files, selected=files[1])
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
                   if(input$show_filename==TRUE) show('file_current')
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
    moved_back <- audio_move(path_from = last_item$path_to,
                             path_to = last_item$path_from, 
                             file_from = last_item$file_to, 
                             file_to = last_item$file_from 
                               )
    
    #if the file was successfully moved back:
    if(moved_back==TRUE) {
      #remove the last row from history
      #history <<- history[1:nrow(history)-1,]
      global$history <- global$history[1:nrow(global$history)-1,]
      #refresh file list and number of files
      files <- list.files(global$path_audio, pattern = "*.wav", full.names = FALSE) 
      #randomise file order?
      if(input$random_order == TRUE) {
        files <- sample(files, size = length(files))
      }
      #and put the focal file back in position 1
      files <- append(files[-which(files==last_item$file_from)], 
                      files[which(files==last_item$file_from)], 0)
      global$files_audio <- files 
      global$n_files <- length(files)
      #refresh current file
      global$file_current <- last_item$file_from
      #refresh file counter
      global$file_counter <- which(files==last_item$file_from)
      #update dropdown
      if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = global$files_audio, selected=global$file_current)
      #update nav buttons
      nav_button_toggles(global$n_files, global$file_counter)
    }
    if(moved_back==FALSE) stop("Error: Failed to undo file move")
    })
  
  observeEvent(input$btn_custom, {
    if(is.null(input$label)) {
      shinyalert(title = "Error",
                 text = "There is no label in the custom label input field",
                 type = "error",
                 callbackR = message('Callback: No label in input field')
      )
    }
    if(!is.null(input$label)) {
      #get the codes for these species
      selected_species <- subset(splist, select_val %in% input$label)
      
      #construct the label string
      label <- paste0('-[',paste0("'", selected_species$code, "'", collapse = ','), '].wav')

      #clear the value from the select
      updateSelectizeInput(inputId = 'label', selected = " ")
      
      #make new file name
      file_new <- gsub(".wav", label, global$file_current)

      #move the file and rename
      renamed <- audio_move(path_from = global$path_audio,
                            path_to = file.path(global$path_audio, 'labelled'),
                            file_from = global$file_current,
                            file_to = file_new
                            )

      #if file successfully moved
      if(renamed==TRUE) {
        #add record to history
        h1 <- data.frame('path_from' = global$path_audio,
                         'path_to' = file.path(global$path_audio, 'labelled'),
                         'file_from' = global$file_current,
                         'file_to' = file_new, 
                         stringsAsFactors = FALSE)
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
          if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = global$files_audio, selected=global$file_current)
        }
        #update file_current if still files to check and on last file in list
        if(global$n_files > 0 & global$file_counter > global$n_files) {
          #must reduce counter by one more to move it back a step to the last remaining file
          global$file_counter <- global$file_counter - 1
          global$file_current <- global$files_audio[global$file_counter]
          #update dropdown
          if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = global$files_audio, selected=global$file_current)
        }
        #no files left
        if(global$n_files == 0) {
          global$file_counter <- 0
          global$file_current <- NULL
          #update dropdown
          if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = NULL, selected=NULL)
          hide('file_count')
          hide('file_current')
          shinyalert(title = "Success",
                     text = "All files in this folder have been verified",
                     type = "success",
                     callbackR = message('Callback: No more audio in this folder')
          )
        }
        
      
      }
      
      
    }
    
  })
  
  #' OUTPUTS ----------------------------------------------------------------------
  
  #show the path
  output$path_audio <- renderText({ global$path_audio })

  
  #dynamic creation of a set of validation buttons based on value list in settings panel 
  obsList <- list()  # to store observers and make sure only once is created per button
  output$validation_buttons <- renderUI({
    val_but_list <- unlist(strsplit(settings$vfchoices, ','))
    #val_but_list <- unlist(strsplit(input$validation_buttons_list, ','))
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
          moved <- audio_move(path_from = global$path_audio, 
                              path_to = file.path(global$path_audio, val_but_list[i]),
                              file_from = global$file_current, 
                              file_to = global$file_current 
                              )
          if(moved==TRUE) {
            #add record to history
            h1 <- data.frame('path_from' = global$path_audio,
                             'path_to' = file.path(global$path_audio, val_but_list[i]),
                             'file_from' = global$file_current,
                             'file_to' = global$file_current, 
                             stringsAsFactors = FALSE)
            
            
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
              if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = global$files_audio, selected=global$file_current)
            }
            #update file_current if still files to check and on last file in list
            if(global$n_files > 0 & global$file_counter > global$n_files) {
              #must reduce counter by one more to move it back a step to the last remaining file
              global$file_counter <- global$file_counter - 1
              global$file_current <- global$files_audio[global$file_counter]
              #update dropdown
              if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = global$files_audio, selected=global$file_current)
            }
            #no files left
            if(global$n_files == 0) {
              global$file_counter <- 0
              global$file_current <- NULL
              #update dropdown
              if(file_dropdown) updateSelectInput(session, input = "file_picker", choices = NULL, selected=NULL)
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
      
      #read the wav data using audio preferably, but if this fails with an odd incomplete file error, use tuneR
      wavdata <- tryCatch(
        {
          # Try to load the wave file using audio::load.wave
          audio::load.wave(file1)
        },
        error = function(e) {
          # If an error occurs, try using tuneR::readWave
          message("audio::load.wave failed, attempting tuneR::readWave...")
          tuneR::readWave(file1)
        }
      )
      
      #unpack depending on which method is used
      if(class(wavdata)=='Wave') {
        #just use left channel for now
        signal <- wavdata@left
        sr <- wavdata@samp.rate
      }
      if(class(wavdata)!='Wave') {
        signal <- wavdata
        sr <- signal$rate
        #deal with stereo file - dim is null if mono
        if(!is.null(dim(signal))) {
          signal <- apply(signal,2,mean)
        }
      }
      

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