#'  To run the Acoustic Pipeline Tools app on your computer:
#'
#'  1. Do not run the app.R program directly. Instead use launch_shiny_app.R
#'  
#'  2. Edit the path_to_app line to give the location of the app.R program on 
#'     your computer. Note that you must use forward (/) slashes, e.g.:
#'     path_to_app <- 'C:/My Documents/app.R'
#'
#'  3. Run all command lines and the app will launch in a browser winder.

#path to where app is saved on your machine- update if necessary
path_to_app <- 'app.R'

#run the app
shiny::runApp(path_to_app, launch.browser = TRUE)
