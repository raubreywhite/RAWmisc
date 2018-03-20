#' YearWeek
#' @param date The date of interest
#' @importFrom lubridate today
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",format.Date(date,"%G"),format.Date(date,"%V")))
}
