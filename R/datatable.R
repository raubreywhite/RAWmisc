#' Allows for recoding of variables
#' @param d a
#' @param switch a vector coded like: c("old"="new")
#' @param var a
#' @param oldOnLeft a
#' @import data.table
#' @export RecodeDT
RecodeDT <- function(d,switch,var,oldOnLeft=TRUE){
  if(oldOnLeft){
    oldNames <- names(switch)
    newNames <- switch
  } else {
    oldNames <- switch
    newNames <- names(switch)
  }
  if(is.numeric(d[[var]])){
    switch <- data.table::data.table(orig=as.numeric(oldNames),replace=as.numeric(newNames))
  } else {
    switch <- data.table::data.table(orig=oldNames,replace=newNames)
  }

  txt <- sprintf("d[switch, on=.(%s=orig), %s:=replace]",var,var)
  eval(parse(text=txt))
}
