# BTOAcousticPipelineVerificationTool

An RShiny App to help with manual checking of short audio clips. A typical use case is where you have a folder of clips that the Acoustic Pipeline has suggested are of a certain species. This app can be used to display a spectrogram of a sound clip, allow the sound to be played, and then a decision can be made whether the identity is true or false resulting in the clip being moved to a true subfolder or a false subfolder. The app can be customised for more complex workflows, such as if multiple species are present, or to perform simple clip-level labelling.

![Screenshot](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineVerificationApp/blob/main/www/screengrab01.png)

## Quick Start

1. Check Requirements
2. Follow Installation instructions
3. Do not directly run 'app.R'. Instead run 'launch_shiny_app.R', but only after completing steps 1 and 2 above


## Requirements

You will need R installed on your computer. We have tested the app on R versions 3.6.1 and 4.2. You do not need to have RStudio installed but if you are familiar with it this may be an easier way to run the code.

You will need the following R packages installed. If you are not familiar with installing R packages we have provided a configuration program (see below). You will only need to run this once. Required packages = shiny, shinyalert, shinyFiles, shinyjs, ini and tuneR.


## Installation

* Open R, either directly or through RStudio.
* Download the package of R code: click the green "Code" button above and select Download ZIP. 
* Save and unzip the download. Remember where this is as you'll need to navigate to it to run the app.
* From R or RStudio open the script file called 'code/configure_local_machine.R'. Run this script to install/update the required R packages. This only needs to be done once on each computer (though you may need to repeat it if you update R).


## Usage

* *Do not directly run 'app.R'.* Instead, with R/RStudio open, open the script file called 'launch_shiny_app.R'. Follow the instructions there, in particular noting the need to update the path_to_app line to say where you have saved the 'app.R' file. 
* Running the 'launch_shiny_app.R' will open the app in a new browser window.


## Issues

If you have any problems with the app please add them on the Issues tab at the top of this screen.

Simon Gillings
March 2023

![APlogo](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineVerificationApp_dev/blob/main/www/APlogo50px.png)



