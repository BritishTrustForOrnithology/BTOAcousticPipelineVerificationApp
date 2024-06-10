#' UI for a Shiny App to facilitate verification of audio classification
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

#read and parse the ini settings
settings <- parse_settings(read.ini('settings.ini'))
settings$max_clip_duration <- 10

#get the drive letters
volumes <- getVolumes()()

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force=TRUE),
  
  tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
  
  
  # Application title
  titlePanel(windowTitle = "BTO Acoustic Pipeline Tools - clip verification v2.0",
  title = div(img(src="APlogo50px.png"), "BTO Acoustic Pipeline Tools - clip verification v2.0", style="font-size:50px; color: #31566d;")),

  tabsetPanel(
    tabPanel(
      title = "Introduction",
      div(
        style="width:800px;",
        tags$h2('Purpose'),
        tags$p('This App is designed to facilitate manual verification of short 
                (<10s) audio clips. A typical use case would be to have a folder 
                of clips nominally containing species X, and to use the App to indicate 
                which are True or False; clips are moved into True or False subfolders accordingly. 
                The App is designed to work with a folder of clips stored on a computer, 
                network drive, memory card or external hard drive. It is not designed 
                for use on the web.'),
        tags$h2('How to use'),
        tags$p('The Main tab contains options to select a folder containing short 
                audio clips. The user can configure options to facilitate blind 
                verification (randomly sorting clips and hiding the filenames). The 
                App will show a spectrogram for the first selected clip. Navigation 
                buttons allow the user to step through the clips.'),
        tags$p('Each spectrogram is accompanied by a sound player widget so the user can listen to 
                the sound clip. Blue buttons allow the user to verify the clip: by default the choices 
                are: True, False or Uncertain (but see below). Upon clicking one of these options the 
                audio clip will be moved to a subfolder of that name. Alternatively, one or more custom labels 
                (e.g. species codes) can be given; this action will append the label (quoted, in 
                square brackets) to the file name and move the file to a \'labelled\' subfolder.'),
        tags$p('The history of actions is stored so it is possible to undo a file move\rename action.'),
        # tags$p('The Settings tab allows various features of the app to be customised. In 
        #        particular the user can change the set of validation buttons to suit their 
        #        needs. For example, a user may wish to have buttons for individual species or
        #        call types.'),
        tags$p('The Spectrogram Settings button opens a panel of settings to customise the spectrogram image.
               Users can change colour themes, adjust the window length and overlap to better \'focus\' the image, 
               and adjust the frequency axis range. Note: stereo files are mixed to mono when creating the image.'),
        tags$h3('Advanced Users'),
        tags$p('To change the verification button options and other settings, carefully modify the settings.ini 
               file prior to launching the shiny app.'),
        tags$br(),
        tags$p('App by Simon Gillings. Copyright 2024 British Trust for Ornithology'),
        tags$br(),
        tags$h4("Licence"),
        tags$p("This software is provided under the MIT License:"),
        tags$p('Permission is hereby granted, free of charge, to any person obtaining a copy of 
             this software and associated documentation files (the "Software"), to deal in the 
             Software without restriction, including without limitation the rights to use, copy, 
             modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
             and to permit persons to whom the Software is furnished to do so, subject to the 
             following conditions:'),
        tags$p('The above copyright notice and this permission notice shall be included in 
             all copies or substantial portions of the Software.'),
        tags$p('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
              IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
              FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
              AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
              LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
              OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
              SOFTWARE.'),
        
        tags$br(),
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
                            tags$p(style = 'font-weight: bold;',
                                   'Blind settings:'),
                            checkboxInput(inputId = 'random_order',
                                          label = 'Randomise clip order',
                                          value = TRUE),
                            checkboxInput(inputId = 'show_filename',
                                          label = 'Show file name',
                                          value = FALSE),
                            actionButton(
                              inputId = 'btn_scan',
                              label = div('Start audit', icon('play', verify_fa =
                                                                FALSE)),
                              class = "btn-primary"
                            ), 
                            # tags$br(),
                            # tags$br(),
                            # div(style="color: #337ab7;",
                            # selectInput(inputId = 'file_picker', 
                            #             label = 'Pick a file (or use green navigation buttons)', 
                            #             choices = NULL)
                            # )
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
                       tags$h4("Select an option to move the clip", style="padding-top: 20px; color: #337ab7; font-weight: bold;"),
                       #tags$p("[Edit/add options in Settings panel]", style="font-size:12px; color: #337ab7;"),
                       style="text-align:center;",
                       uiOutput("validation_buttons")
                     ),
                     div(
                       id = 'multispplabel',
                       style = "text-align:left;",
                       div(
                         style = "float: left; width: 250px;",
                         selectizeInput(
                           inputId = 'label',
                           label = 'Multi-species label:',
                           choices = c("", splist$select_val),
                           multiple = TRUE,
                           options = list(placeholder = "Start typing...")
                         ),
                       ),
                       actionButton(
                         inputId = 'btn_custom',
                         icon = icon('plus', verify_fa =
                                       FALSE),
                         label = "",
                         style = "background-color: #337ab7; color: #fff; margin-top:25px;margin-left:5px;",
                         width = 50
                       )
                     ), #end multispplabel div
                     # div(id='custom',
                     #     style="margin: auto; padding-top: 10px; width: 280px",
                     #     tags$div(
                     #       style="float:left; color: #337ab7;",
                     #       textInput(inputId = 'custom_label',
                     #                 label='Or add custom label(s)',
                     #                 width = 200, placeholder = 'e.g. TO,NJ'
                     #                 ),
                     #     ),
                     #     tags$div(
                     #       style="float:left; padding-top: 20px; padding-left: 10px",
                     #       actionButton(inputId = 'btn_custom',
                     #                    label = "Add",
                     #                    style="background-color: #337ab7; color: #fff; margin-top:5px;margin-bottom:5px;"
                     #                    )
                     #     )
                     # ),
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
    
  # tabPanel(
  #   title = "Settings", 
  #     textInput(inputId = 'validation_buttons_list', 
  #               label = 'Enter the list of validation buttons you require, as an unquoted comma-separated list:', 
  #               value = settings$vfchoices, width = "800px"),
  #   tags$br(),
  #   tags$br(),
  #   tags$br(),
  #   tags$br(),
  #   tags$br(),
  #   tags$p('Advanced Users: to preserve settings at startup, carefully modify the settings.ini file')
  #   ),
  
  tabPanel("Exit", 
           tags$br(),
           tags$br(),
           tags$br(),
           actionButton("exit2", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);")
  )

    
  )  
)