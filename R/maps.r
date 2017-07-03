#' Reads in shapefile and fortifies it
#' @param file a
#' @param region a
#' @export FortifyShapeFile
FortifyShapeFile <- function(file, region){
  map <- maptools::readShapeSpatial(file)
  fortifiedMap <- broom::tidy(map,region=region)
  return(fortifiedMap)
}

