#' YearWeek
#' @param date The date of interest
#' @importFrom lubridate today year week
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",lubridate::year(date),lubridate::week(date)))
}
