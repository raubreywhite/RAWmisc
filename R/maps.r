FortifyShapeFile <- function(file, region){
  map <- readShapeSpatial(file)
  fortifiedMap <- fortify(map,region=region)  
  return(fortifiedMap)
}

FortifyShapeFileNorwegianMunicipalities <- function(){
  file <- system.file("extdata","municipalities","kommuner_2013.shp",package="RAWmisc")
  map <- FortifyShapeFile(file, "ID")
  map$municip <- paste0("municip", map$id)
  map <
  return(map)
}

FortifyShapeFileNorwegianCounties <- function(){
  #data(countyToMunicip)
  file <- system.file("extdata","counties","NOR_adm1.shp",package="RAWmisc")
  map <- FortifyShapeFile(file, "NAME_1")
  
  map$county <- ""
  map$county[map$id=="Ãstfold"] <- "county01"
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
  map$county[map$id=="Møre og Romsdal"] <- "county15"
  map$county[map$id=="Sør-Trøndelag"] <- "county16"
  map$county[map$id=="Nord-Trøndelag"] <- "county17"
  map$county[map$id=="Nordland"] <- "county18"
  map$county[map$id=="Troms"] <- "county19"
  map$county[map$id=="Finnmark"] <- "county20"
  
  return(map)
}
