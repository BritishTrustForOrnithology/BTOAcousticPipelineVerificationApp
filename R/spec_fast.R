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
  #print(AmplitudeRange)
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