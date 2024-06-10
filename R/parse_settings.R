#' @description Check and parse settings pulled from ini file
#' @param x = settings list from read.ini()
parse_settings <- function(x = settings) {
  if(!x$spectrogram$type %in% c('Audible', 'Ultrasonic')) stop("Initialization error: spectrogram type must be Audible or Ultrasonic")
  if(!x$spectrogram$theme %in% c('Greyscale', 'Viridis')) stop("Initialization error: spectrogram theme must be Greyscale or Viridis")
  if(!is.numeric(as.numeric(x$spectrogram$freq_min))) stop("Initialization error: spectrogram minimum frequency must be a number")
  if(!is.numeric(as.numeric(x$spectrogram$freq_max))) stop("Initialization error: spectrogram maximum frequency must be a number")
  if( as.numeric(x$spectrogram$freq_min) >= as.numeric(x$spectrogram$freq_max) ) stop("Initialization error: spectrogram minimum frequency must be less than maximum frequency")
  if((as.numeric(x$spectrogram$freq_min) >= 1000)) stop("Initialization error: spectrogram minimum frequency should be given in kHz, e.g. 0, 1, 2...")
  if((as.numeric(x$spectrogram$freq_max) >= 1000)) stop("Initialization error: spectrogram maximum frequency should be given in kHz, e.g. 8, 9, 10...")
  if(!is.character(x$verification_choices$list)) stop("Initialization error: verification choices must be a string")
  if(grepl('/', x$verification_choices$list)) stop("Initialization error: verification choices must not include slash characters")
  if(grepl('\\', x$verification_choices$list, fixed=TRUE)) stop("Initialization error: verification choices must not include slash characters")
  
  settings <- list()
  settings$spec_type <- x$spectrogram$type
  settings$spec_theme <- x$spectrogram$theme
  settings$spec_ymin <- as.numeric(x$spectrogram$freq_min)
  settings$spec_ymax <- as.numeric(x$spectrogram$freq_max)
  settings$vfchoices <- x$verification_choices$list
  
  return(settings)
}
