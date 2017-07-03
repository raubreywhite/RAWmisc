#' Allows for recoding of variables
#' @param x a
#' @param digits a
#' @export Format
Format <- function(x,digits=2){
  formatC(x,digits=digits,format="f")
}
