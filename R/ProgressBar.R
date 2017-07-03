#' Creates a progress bar
#' @param min Minimum
#' @param max Maximum
#' @export ProgressBarCreate
ProgressBarCreate <- function(min = 0, max = 1)
{
  pb <- list()
  pb$min <- min
  pb$max <- max
  pb$timeStart <- Sys.time()
  class(pb) <- "RAWmiscProgressBar"
  return(pb)
}

#' Sets progress bar's progress
#' @param pb A progress bar
#' @param value The value
#' #' @importFrom utils flush.console
#' @export ProgressBarSet
ProgressBarSet <- function(pb, value){

  if (!inherits(pb, "RAWmiscProgressBar")) stop("'pb' is not from class 'RAWmiscProgressBar")

  timeTaken <- as.numeric(difftime(Sys.time(),pb$timeStart, units = "mins"))
  propCompleted <- (value-pb$min)/(pb$max-pb$min)
  timeTotal <- timeTaken/propCompleted
  timeLeft <- timeTotal-timeTaken

  cat(sprintf("\r%s Total time: %s min. %s%% completed in %s min. %s min remaining.",
              format(Sys.time(),'%H:%M:%S'),
              Format(timeTotal,digits=1),
              Format(propCompleted*100,digits=0),
              Format(timeTaken,digits=1),
              Format(timeLeft,digits=1)
              ))
  utils::flush.console()
}
