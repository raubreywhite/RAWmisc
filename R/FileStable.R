#' Is the file stable (size not increasing/decreasing)?
#'
#' @param file The file of interest
#' @param delay Number of seconds between checking file size a second time
#' @export IsFileStable
IsFileStable <- function(file = tempfile(), delay = 5) {
  if (file.exists(file)) {
    size1 <- file.info(file)$size[1]
    Sys.sleep(delay)
    size2 <- file.info(file)$size[1]
    return(size1 == size2)
  }
  return(FALSE)
}

#' FileTimeModified
#' This determines if the file has been changed (checks modification times)
#' @param file The file of interest
#' @export FileTimeModified
FileTimeModified <- function(file = tempfile()) {
  return(file.info(file)$mtime)
}
