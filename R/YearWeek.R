#' YearWeek
#' @param date The date of interest
#' @importFrom lubridate today
#' @import data.table
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",data.table::year(date),data.table::isoweek(date)))
}
