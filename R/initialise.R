
msg <- function(p,s,m){
  if(!s %in% c("RUN","ERROR","WARN","FINISHED")) stop("Error in message function")
  if(length(grep("linux",sessionInfo()$platform))>0) system(paste0('/log/log.sh "',lubridate::now(tzone="CET"),'@',p,'@',s,'@',m,'"'))
}

Initialise <- function(){
  if(Sys.getenv("RPROJ")!=""){
    assign("RPROJ", list(PROJHOME = Sys.getenv("RPROJ")), envir=globalenv())
    setwd(RPROJ$PROJHOME)
  }
  if(Sys.getenv("RAPP")!=""){
    assign("RPROJ", list(PROJHOME = RPROJ$PROJHOME, APPHOME = Sys.getenv("RAPP")), envir=globalenv())
  }

  assign("isLinux", length(grep("linux",sessionInfo()$platform))>0)
  assign("isRStudio", Sys.getenv("RSTUDIO") == "1")

  if(!exists("RPROJ")){
    assign("RPROJ", list(PROJHOME = file.path("/src/")), envir=globalenv())
  } 

  if(is.null(RPROJ$APPHOME)){
    assign("RPROJ", list(PROJHOME = RPROJ$PROJHOME, APPHOME = file.path("../../app/sykdomspuls/")), envir=globalenv())
  }

  setwd(RPROJ$PROJHOME)

  fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
  sapply(fileSources,source,.GlobalEnv)
 }
 
InitialiseProject <- function(PROJHOME=NULL,PROJRAW=NULL,PROJCLEAN=NULL,PROJBAKED=NULL,PROJFINAL=NULL,PROJSHARED=NULL){
  if(Sys.getenv("PROJHOME")!=""){
    PROJHOME = Sys.getenv("PROJHOME")
  }
  if(Sys.getenv("PROJRAW")!=""){
    PROJRAW = Sys.getenv("PROJRAW")
  }
  if(Sys.getenv("PROJCLEAN")!=""){
    PROJCLEAN = Sys.getenv("PROJCLEAN")
  }
  if(Sys.getenv("PROJBAKED")!=""){
    PROJBAKED = Sys.getenv("PROJBAKED")
  }
  if(Sys.getenv("PROJFINAL")!=""){
    PROJFINAL = Sys.getenv("PROJFINAL")
  }
  if(Sys.getenv("PROJSHARED")!=""){
    PROJSHARED = Sys.getenv("PROJSHARED")
  }

  assign("RPROJ", list(
    PROJHOME = PROJHOME, 
    PROJRAW = PROJRAW,
    PROJCLEAN = PROJCLEAN,
    PROJBAKED = PROJBAKED,
    PROJFINAL = PROJFINAL,
    PROJSHARED = PROJSHARED
  ), envir=globalenv())
  
  for(i in names(RPROJ)){
    if(!is.null(RPROJ[[i]])) if(!dir.exists(RPROJ[[i]])) dir.create(RPROJ[[i]], recursive=TRUE)  
  }

  setwd(RPROJ$PROJHOME)

  fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
  sapply(fileSources,source,.GlobalEnv)
 }
 
