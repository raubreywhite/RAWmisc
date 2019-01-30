#' Create a new temp dir
#'
#' @export TempDir
TempDir <- function(file = tempfile(), delay = 5) {
  time <- as.character(Sys.time())
  time <- gsub("[\\: ]","-",time)
  rand <- as.character(runif(1))
  rand <- gsub("[\\. ]","",rand)
  dir.create(t <- file.path(tempdir(),
                            sprintf("%s-%s-%s",
                                    time,
                                    Sys.getpid(),
                                    rand)))

  return(t)
}

#' FileTimeModified
#' This determines if the file has been changed (checks modification times)
#' @param file The file of interest
#' @export FileTimeModified
FileTimeModified <- function(file = tempfile()) {
  return(file.info(file)$mtime)
}
