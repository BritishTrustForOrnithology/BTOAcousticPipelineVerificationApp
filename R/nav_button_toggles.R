#' @description Toggle enable/disable feature of navigation buttons
#' @param numfiles = the total number of audio files
#' @param nthfile = which file is currently active
nav_button_toggles <- function(numfiles, nthfile) {
  if(nthfile==1) disable('btn_first') else enable('btn_first')
  if(nthfile==1) disable('btn_previous') else enable('btn_previous')
  if(nthfile==numfiles) disable('btn_last') else enable('btn_last')
  if(nthfile==numfiles) disable('btn_next') else enable('btn_next')
}