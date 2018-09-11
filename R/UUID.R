#' UUID
#' This creates the skeleton analysis stack. The number of digits of the UUID is altered through CONFIG$UUID_DIGITS
#' @param n The number of UUIDs to create
#' @importFrom uuid UUIDgenerate
#' @importFrom stringr str_sub
#' @export UUID
UUID <- function(n=1) {
  return(replicate(n,stringr::str_sub(uuid::UUIDgenerate(),1,CONFIG$UUID_DIGITS)))
}
