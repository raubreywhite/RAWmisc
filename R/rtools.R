
AddRtools <- function(path="H:/Apps/RBuildTools/3.3/"){
  if(path!=""){
    path <- gsub("/","\\\\",path)
    path <- paste0(path,"\\")
    for(i in 1:10) path <- gsub("\\\\\\\\","\\\\",path)
    currentPath <- Sys.getenv("PATH")
    newPath <- paste(currentPath,paste0(path,"\\bin"),paste0(path,"\\gcc-4.6.3\\bin"),sep=";")
    Sys.setenv(PATH=newPath)
    
    if(!devtools::find_rtools()) stop("CANT FIND RTOOLS")
  }
}

