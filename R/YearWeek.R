#' YearWeek
#' @param date The date of interest
#' @import lubridate
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",lubridate::year(date),lubridate::week(date)))
}
