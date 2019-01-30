#' Create a new temp dir
#'
#' @export TempDir
TempDir <- function(file = tempfile(), delay = 5) {
  time <- as.character(Sys.time())
  time <- gsub("[\\: ]","-",time)
  rand <- as.character(stats::runif(1))
  rand <- gsub("[\\. ]","",rand)
  dir.create(t <- file.path(tempdir(),
                            sprintf("%s-%s-%s",
                                    time,
                                    Sys.getpid(),
                                    rand)))

  return(t)
}
