#' Creates a progress bar
#' @param min Minimum
#' @param max Maximum
#' @param flush Should it flush or print the output?
#' @export ProgressBarCreate
ProgressBarCreate <- function(min = 0, max = 1, flush=TRUE)
{
  pb <- list()
  pb$min <- min
  pb$max <- max
  pb$timeStart <- Sys.time()
  pb$flush <- flush
  class(pb) <- "RAWmiscProgressBar"
  return(pb)
}

#' Sets progress bar's progress
#' @param pb A progress bar
#' @param value The value
#' @importFrom utils flush.console
#' @export ProgressBarSet
ProgressBarSet <- function(pb, value){

  if (!inherits(pb, "RAWmiscProgressBar")) stop("'pb' is not from class 'RAWmiscProgressBar")

  timeTaken <- as.numeric(difftime(Sys.time(),pb$timeStart, units = "mins"))
  propCompleted <- (value-pb$min)/(pb$max-pb$min)
  if(propCompleted==0){
    timeTotal <- 1000
  } else {
    timeTotal <- timeTaken/propCompleted
  }
  timeLeft <- timeTotal-timeTaken

  retval <- sprintf("\r%s Total time: %s min. %s%% completed in %s min at %s. %s min remaining.",
              format(Sys.time(),'%H:%M:%S'),
              Format(timeTotal,digits=1),
              Format(propCompleted*100,digits=0),
              Format(timeTaken,digits=1),
              format(Sys.time()+lubridate::minutes(round(timeLeft)),'%H:%M:%S'),
              Format(timeLeft,digits=1)
              )
  if(pb$flush){
    cat(retval)
    utils::flush.console()
  } else print(retval)
}
