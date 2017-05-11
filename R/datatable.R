#' Allows for recoding of variables
#' @param d a
#' @param switch a vector coded like: c("old"="new")
#' @param var a
#' @import data.table
#' @export RecodeDT
RecodeDT <- function(d,switch,var){
  if(is.numeric(d[[var]])){
    switch <- data.table::data.table(orig=as.numeric(names(switch)),replace=as.numeric(switch))
  } else {
    switch <- data.table::data.table(orig=names(switch),replace=switch)
  }

  txt <- sprintf("d[switch, on=.(%s=orig), %s:=replace]",var,var)
  eval(parse(text=txt))
}
