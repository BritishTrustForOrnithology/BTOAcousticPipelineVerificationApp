#' Configure local R environment
#' This will need to be run this on each computer on which you plan to use 
#' BTOAcousticPipelineVerificationApp. You will only need to run this once on each 
#' computer (except in case where you upgrade R and then may need to run it again).
#' 
#' Simon Gillings BTO
#' March 2023


#' Install required packages if not present
#' 
#' @description Check if required CRAN packages are installed, and if not, install them
#' @param packages = list of required packages
#' 
install_packages <- function(packages) {
  k <- packages[!(packages %in% installed.packages()[, "Package"])]
  if(length(k) == 0) cat("All CRAN packages already present\n")
  if(length(k) > 0) {
    cat("Installing the following CRAN packages:", paste(unlist(k), collapse=', '),"\n")
    install.packages(k, repos = 'https://cran.rstudio.com/')
  }
}

#get CRAN packages
install_packages(packages = c("shiny", "shinyFiles", "shinyalert", "shinyjs", "audio", "signal", "scales", "ini")) 
