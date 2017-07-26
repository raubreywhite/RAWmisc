#' FileStable
#' This determines if the file is stable (size not increasing/decreasing)
#' @param file The file of interest
#' @param delay Number of seconds between checking file size a second time
#' @export FileStable
FileStable <- function(file=tempfile(), delay=1){
  if (file.exists(file)){
    size1 <- file.info(file)$size[1]
    Sys.sleep(delay)
    size2 <- file.info(file)$size[1]
    return(size1==size2)
  }
  return(FALSE)
}
