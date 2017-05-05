#' Reads in shapefile and fortifies it
#' @param file a
#' @param region a
#' @export FortifyShapeFile
FortifyShapeFile <- function(file, region){
  map <- maptools::readShapeSpatial(file)
  fortifiedMap <- broom::tidy(map,region=region)
  return(fortifiedMap)
}

#' Produces norwegian municipalities fortified file
#' @export FortifyShapeFileNorwegianMunicipalities
FortifyShapeFileNorwegianMunicipalities <- function(){
  file <- system.file("extdata","municipalities","kommuner_2013.shp",package="RAWmisc")
  map <- FortifyShapeFile(file, "ID")
  map$municip <- paste0("municip", map$id)
  return(map)
}

#' Produces norwegian counties fortified file
#' @export FortifyShapeFileNorwegianCounties
FortifyShapeFileNorwegianCounties <- function(){
  file <- system.file("extdata","counties","NOR_adm1.shp",package="RAWmisc")
  map <- FortifyShapeFile(file, "NAME_1")

  map$county <- ""
  map$county[map$id=="\xc3stfold"] <- "county01"
  map$county[map$id=="Akershus"] <- "county02"
  map$county[map$id=="Oslo"] <- "county03"
  map$county[map$id=="Hedmark"] <- "county04"
  map$county[map$id=="Oppland"] <- "county05"
  map$county[map$id=="Buskerud"] <- "county06"
  map$county[map$id=="Vestfold"] <- "county07"
  map$county[map$id=="Telemark"] <- "county08"
  map$county[map$id=="Aust-Agder"] <- "county09"
  map$county[map$id=="Vest-Agder"] <- "county10"
  map$county[map$id=="Rogaland"] <- "county11"
  map$county[map$id=="Hordaland"] <- "county12"
  map$county[map$id=="Sogn og Fjordane"] <- "county14"
  map$county[map$id=="M\xf8re og Romsdal"] <- "county15"
  map$county[map$id=="S\xf8r-Tr\xf8ndelag"] <- "county16"
  map$county[map$id=="Nord-Tr\xf8ndelag"] <- "county17"
  map$county[map$id=="Nordland"] <- "county18"
  map$county[map$id=="Troms"] <- "county19"
  map$county[map$id=="Finnmark"] <- "county20"

  return(map)
}
