#' Detect spline
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @export DetectSpline
DetectSpline <- function(var){
  # detect if spline
  return(stringr::str_detect(var,"ns\\([a-zA-Z0-9_]*,"))
}

#' ExtractExposureConfounders.int
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @importFrom stringr str_replace_all str_extract str_sub str_detect str_split
#' @export ExtractExposureConfounders.int
ExtractExposureConfounders.int <- function(var){
  # detect if spline
  var <- stringr::str_replace_all(var," ","")
  if(RAWmisc::DetectSpline(var)){
    var <- stringr::str_extract(var,"ns\\([a-zA-Z0-9_]*,")
    var <- stringr::str_sub(var,4,-2)
    return(var)
  }

  # detect interaction
  if(stringr::str_detect(var,":*")){
    return(unlist(stringr::str_split(var, "[:*]")))
  }
}

#' ExtractExposureConfounders
#' Transforms cos and sin to amplitude peak and trough
#' @param var The results
#' @importFrom stats na.omit
#' @export ExtractExposureConfounders
ExtractExposureConfounders <- function(var){
  res <- c()
  for(i in var) res <- c(res,ExtractExposureConfounders.int(i))
  res <- na.omit(unique(res))
  return(res)
}
