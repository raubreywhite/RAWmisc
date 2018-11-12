#' Is the variable here an interaction?
#'
#' @param var The results
#' @importFrom stringr str_detect str_remove
#' @export IsInteraction
IsInteraction <- function(var) {
  varx <- stringr::str_remove(var, "::")
  return(stringr::str_detect(varx, ":"))
}

#' Detects interaction
#'
#' @param var The results
#' @importFrom stringr str_detect str_remove
#' @export DetectInteraction
DetectInteraction <- function(var) {
  if (length(var) == 0) return(FALSE)
  retval <- FALSE
  for (j in var) {
    jx <- stringr::str_remove(j, "::")
    if (!is.na(jx)) if (stringr::str_detect(jx, ":")) retval <- TRUE
  }
  # detect if interaction
  return(retval)
}

#' Detect spline
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @importFrom stringr str_detect
#' @export DetectSpline
DetectSpline <- function(var) {
  if (length(var) == 0) return(FALSE)
  retval <- FALSE
  for (i in var) {
    if (!is.na(i)) if (stringr::str_detect(i, "ns\\([a-zA-Z0-9_]*,")) retval <- TRUE
  }
  # detect if spline
  return(retval)
}

#' ExtractExposureConfounders.int
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @importFrom stringr str_remove str_replace_all str_extract str_sub str_detect str_split
#' @export ExtractExposureConfounders.int
ExtractExposureConfounders.int <- function(var) {
  var <- stringr::str_remove(var, " ")
  var <- stringr::str_replace_all(var, "splines::ns\\(", "ns\\(")

  # detect interaction
  if (stringr::str_detect(var, ":*")) {
    var <- unlist(stringr::str_split(var, "[:*]"))
  }

  # detect if spline
  for (j in 1:length(var)) {
    if (RAWmisc::DetectSpline(var[j])) {
      var[j] <- stringr::str_extract(var[j], "ns\\([a-zA-Z0-9_]*,")
      var[j] <- stringr::str_sub(var[j], 4, -2)
    }
  }

  return(var)
}

#' ExtractExposureConfounders
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @importFrom stats na.omit
#' @export ExtractExposureConfounders
ExtractExposureConfounders <- function(var) {
  var <- na.omit(unique(var))
  if (length(var) == 0) return(NULL)

  res <- c()
  for (i in var) res <- c(res, ExtractExposureConfounders.int(i))
  res <- na.omit(unique(res))
  return(res)
}
