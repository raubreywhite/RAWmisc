ContribDir <- function(localCRAN="H:/CRAN"){
  return(file.path(localCRAN, "src", "contrib"))
}

RVersion <- function(){
  return(paste(unlist(getRversion())[1:2], collapse = "."))
}

BinPaths <- function(localCRAN="H:/CRAN"){
  rVersion <- RVersion()
  binPaths <- list(
    win.binary = file.path("bin/windows/contrib", rVersion),
    mac.binary = file.path("bin/macosx/contrib", rVersion),
    mac.binary.mavericks = file.path("bin/macosx/mavericks/contrib", rVersion),
    mac.binary.leopard = file.path("bin/macosx/leopard/contrib", rVersion)
  )
  
  binPaths <- lapply(binPaths, function(x) file.path(localCRAN, x))
  return(binPaths)
}



AddToLocalCRAN <- function(location="", localCRAN="H:/CRAN/"){
  contribDir <- ContribDir(localCRAN = localCRAN)
  binPaths <- BinPaths(localCRAN = localCRAN)
  rVersion <- RVersion()
  
  fileName <- str_split(location,"/") %>%
    unlist
  fileName <- fileName[length(fileName)]
  file.copy(location, file.path(contribDir,fileName), overwrite=TRUE)
  
  tools::write_PACKAGES(contribDir, type="source")
  lapply(binPaths, function(path) {
    tools::write_PACKAGES(path)
  })
}

RemoteCRANToLocalCRAN <- function(package="", localCRAN="H:/CRAN/"){
  pkgList <- pkgDep(package, repos="http://cran.revolutionanalytics.com", type="source", suggests = TRUE)
  
  for(i in pkgList){
    location <- download.packages(i, destdir=tempdir(), repos="http://cran.revolutionanalytics.com", type="source")[2]
    AddToLocalCRAN(location = location, localCRAN = localCRAN)
  }
}

BuildToLocalCRAN <- function(package="", localCRAN="H:/CRAN/"){
  if(package=="") stop("No package specified")
  
  location <- devtools::build(package)
  AddToLocalCRAN(location = location, localCRAN = localCRAN)
}


#' Creates own personal local CRAN
#' Taken from https://rstudio.github.io/packrat/custom-repos.html
CreateLocalCRAN <- function(
  localCRAN = "H:/CRAN",
  libs = c(
    "data.table",
    "tidyr",
    "magrittr",
    "grid",
    "stringr",
    "R6",
    "ggplot2",
    "dplyr",
    "gridExtra",
    "knitr",
    "rmarkdown",
    "brew",
    "scales",
    "Hmisc",
    "Gmisc",
    "htmlTable",
    "ggvis",
    "Formula",
    "roxygen2",
    "packrat",
    "rms",
    "survey"
  ),
  overwrite=FALSE){
  
  if(overwrite){
    try(unlink(localCRAN, recursive=TRUE),TRUE)
  } else {
    if(file.exists(localCRAN)) stop("CRAN directory already exists")
  }
  dir.create(localCRAN)
  contribDir <- ContribDir(localCRAN = localCRAN)
  dir.create(contribDir, recursive = TRUE)
  
  rVersion <- RVersion()
  
  binPaths <- BinPaths(localCRAN = localCRAN)
  lapply(binPaths, function(path) {
    dir.create(path, recursive = TRUE)
  })
  
  RemoteCRANToLocalCRAN(package=libs, localCRAN=localCRAN)
  
}










