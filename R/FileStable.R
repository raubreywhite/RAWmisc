#' IsFileStable
#' This determines if the file is stable (size not increasing/decreasing)
#' @param file The file of interest
#' @param delay Number of seconds between checking file size a second time
#' @export IsFileStable
IsFileStable <- function(file=tempfile(), delay=5){
  if (file.exists(file)){
    size1 <- file.info(file)$size[1]
    Sys.sleep(delay)
    size2 <- file.info(file)$size[1]
    return(size1==size2)
  }
  return(FALSE)
}

#' IsFileChanged
#' This determines if the file has been changed (checks modification times)
#' @param fileToCheck The file of interest
#' @param fileDetails File that contains the details of things being checked
#' @export IsFileChanged
IsFileChanged <- function(fileToCheck=tempfile(), fileDetails=tempfile(), delay=5){
  if(!file.exists(fileToCheck)) stop("file doesn't exist")

  lastMod <- file.info(fileToCheck)$mtime

  if(!file.exists(fileDetails)){
    saveRDS(lastMod,fileDetails)
    return(TRUE) # first time we have checked the file
  } else {
    recordedMod <- readRDS(fileDetails)
    if(recordedMod==lastMod){
      # file hasn't changed
      return(FALSE)
    } else {
      if(RAWmisc::IsFileStable(fileToCheck, delay=delay)){
        # file has changed but is unstable, cant be trusted
        return(FALSE)
      } else {
        # file has changed and is stable
        saveRDS(lastMod,fileDetails)
        return(TRUE)
      }
    }
  }
}

