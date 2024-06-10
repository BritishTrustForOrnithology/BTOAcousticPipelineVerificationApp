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