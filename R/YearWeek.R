#' YearWeek
#' @param date The date of interest
#' @importFrom lubridate today
#' @import data.table
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",data.table::year(date),formatC(data.table::isoweek(date),width=2,flag="0")))
}
