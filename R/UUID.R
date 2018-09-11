#' UUID
#' This creates the skeleton analysis stack. The number of digits of the UUID is altered through CONFIG$UUID_DIGITS
#' @param n The number of UUIDs to create
#' @importFrom uuid UUIDgenerate
#' @importFrom stringr str_sub
#' @export UUID
UUID <- function(n=1) {
  width <- ceiling(log10(n+1))
  padding <- formatC(1:n,width=width,flag='0')
  uuids <- replicate(n,stringr::str_sub(uuid::UUIDgenerate(),1,CONFIG$UUID_DIGITS))

  retval <- sprintf("%s-%s",padding,uuids)
  return(retval)
}
