#' FUNCTIONS for the BTO Acoustic Pipeline Tools verification app
#' Simon Gillings
#' April 2023


#' @description Move an audio file based on verification decision
#' @detail This function can also be used in reverse to move a clip back using clip history
#' @param path_from = path where the file currently lives
#' @param path_to = path where the file needs to move to
#' @param file_from = current file name
#' @param file_to = destination file name (can be same as file_from)
audio_move <- function(path_from, path_to, file_from, file_to=file_from) {
  if(dir.exists(path_to)==FALSE) dir.create(path_to)
  from1 <- file.path(path_from, file_from)
  to1 <- file.path(path_to, file_to)
  success <- file.rename(from1, to1)
  return(success)
}




#' @description Toggle enable/disable feature of navigation buttons
#' @param numfiles = the total number of audio files
#' @param nthfile = which file is currently active
nav_button_toggles <- function(numfiles, nthfile) {
  if(nthfile==1) disable('btn_first') else enable('btn_first')
  if(nthfile==1) disable('btn_previous') else enable('btn_previous')
  if(nthfile==numfiles) disable('btn_last') else enable('btn_last')
  if(nthfile==numfiles) disable('btn_next') else enable('btn_next')
}


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
  





#' Produce a spectrogram quickly!
#' @description Reads an audio file and produces a spectrogram based on given window 
#' length and overlap. Most of the function is written from first principles and 
#' librosa methods from Python. Some inspiration for the image plotting from 
#' Aaron Albin's Spectrogram() function.
#' @param path_to_wav = full path to the audio file
#' @param window_size = window size for the FFT
#' @param overlap = proportional overlap of successive windows
#' @param theme = Greyscale or Viridis
#' @param ylim = vector for min and max of the y-axis range
spec_fast <- function(signal, sr, window_size, overlap, theme, ylim) {
#spec_fast <- function(path_to_wav, window_size, overlap, theme, ylim) {
  #get the audio signal
  #signal <- audio::load.wave(path_to_wav)
  #sr <- signal$rate
  
  # if(length(signal)/sr > 4.5) {
  #   shinyalert(title = 'Warning - clip too long', text = 'This app is designed for clips of 10 seconds or less. Producing longer spectrograms is very slow and best not attempted here.')
  #   return()
  # }
  
  # Calculate the hop size (i.e., how much to shift the window between segments)
  hop_size <- floor(window_size * (1 - overlap))
  
  # Apply a window function to the audio signal
  window <- signal::hanning(window_size)
  signal_length <- length(signal)
  num_segments <- floor((signal_length - window_size) / hop_size)
  
  # Initialize an empty matrix to hold the magnitude spectra
  stft <- matrix(0, ncol = window_size/2, nrow = num_segments)
  
  # Loop through each segment
  for (i in 1:num_segments) {
    # Extract the current segment
    start_idx <- (i-1)*hop_size + 1
    end_idx <- start_idx + window_size - 1
    segment <- signal[start_idx:end_idx]
    
    # Apply the window function
    windowed_segment <- segment * window
    
    # Calculate the Fourier transform
    ft <- fft(windowed_segment)
    
    # Calculate the magnitude spectrum
    mag <- Mod(ft[1:(window_size/2)])
    
    # Store the magnitude spectrum in the STFT matrix
    stft[i,] <- mag
  }
  
  #convert to log power spectrogram
  stft <- 10*log(stft^2, 10)
  #threshold to -70db
  stft[which(stft < -70)] = -70

  #set the lowest two rows of spectrogram to -70 - these are often very high and take highest colour values
  stft[,c(1, 2)] <- -70
  
  
  #inspired from https://github.com/usagi5886/dsp/blob/master/Spectrogram().r
  AmplitudeRange = range(stft, finite = TRUE) # 'finite=TRUE' excludes values of positive/negative infinity
  print(AmplitudeRange)
  MinimumAmplitude = AmplitudeRange[1]
  MaximumAmplitude = AmplitudeRange[2]
  # number of color levels
  nColorLevels = 2 * abs(MinimumAmplitude) - abs(MaximumAmplitude) 
  
  if(theme == 'Greyscale') palette <- colorRampPalette(c("white", "black"))(round(nColorLevels)) 
  if(theme == 'Viridis') palette <- scales::viridis_pal()(round(nColorLevels)) 
  if(theme == 'Red') palette <- colorRampPalette(c("white", "#811331"))(round(nColorLevels)) 
  if(theme == 'Blue') palette <- colorRampPalette(c("white", "#00008B"))(round(nColorLevels)) 
  if(theme == 'Green') palette <- colorRampPalette(c("white", "#023020"))(round(nColorLevels)) 
  if(theme == 'Warm') palette <- RColorBrewer::brewer.pal(round(nColorLevels),'YlOrRd')
    

  #calculate the x and y values
  duration <- length(signal)/sr
  times <- seq(from = 0, to = duration, length.out = dim(stft)[1]+1)
  freqs <- seq(from = 0, to = (sr/2), length.out = dim(stft)[2]+1)

  #make the plot  
  image(x=times,
        y=freqs,
        z=stft, 
        col = palette, 
        useRaster=TRUE,
        ylim=ylim,
        xlab = "Time (s)",
        ylab = "Frequency (Hz)")
}

#' 
#' #' Validate custom label
#' #' @param label
#' validate_label <- function(label) {
#'   label <- toupper(label)
#'   #split
#'   label <-  'GE-F,RE-F'
#' 
#'   labs <- strsplit(label, ',')  
#'   for(l in 1:length(labs))
#' }

