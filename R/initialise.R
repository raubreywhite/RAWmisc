
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
 
InitialiseProject <- function(PROJHOME=NULL,PROJRAW=NULL,PROJCLEAN=NULL,PROJBAKED=NULL,PROJFINAL=NULL,PROJSHARED=NULL,HOME=NULL,RAW=NULL,CLEAN=NULL,BAKED=NULL,FINAL=NULL,SHARED=NULL){
  if(Sys.getenv("PROJHOME")!=""){
    HOME = PROJHOME = Sys.getenv("PROJHOME")
  }
  if(Sys.getenv("PROJRAW")!=""){
    RAW = PROJRAW = Sys.getenv("PROJRAW")
  }
  if(Sys.getenv("PROJCLEAN")!=""){
    CLEAN = PROJCLEAN = Sys.getenv("PROJCLEAN")
  }
  if(Sys.getenv("PROJBAKED")!=""){
    BAKED = PROJBAKED = Sys.getenv("PROJBAKED")
  }
  if(Sys.getenv("PROJFINAL")!=""){
    FINAL = PROJFINAL = Sys.getenv("PROJFINAL")
  }
  if(Sys.getenv("PROJSHARED")!=""){
    SHARED = PROJSHARED = Sys.getenv("PROJSHARED")
  }
  
  if(is.null(PROJHOME) & !is.null(HOME)){
    PROJHOME <- HOME  
  }
  if(is.null(PROJRAW) & !is.null(RAW)){
    PROJRAW <- RAW  
  }
  if(is.null(PROJCLEAN) & !is.null(CLEAN)){
    PROJCLEAN <- CLEAN
  }
  if(is.null(PROJBAKED) & !is.null(BAKED)){
    PROJBAKED <- BAKED
  }
  if(is.null(PROJFINAL) & !is.null(FINAL)){
    PROJFINAL <- FINAL
  }
  if(is.null(PROJSHARED) & !is.null(SHARED)){
    PROJSHARED <- SHARED
  }
  
  if(!is.null(PROJHOME) & is.null(HOME)){
    PROJHOME -> HOME  
  }
  if(!is.null(PROJRAW) & is.null(RAW)){
    PROJRAW -> RAW  
  }
  if(!is.null(PROJCLEAN) & is.null(CLEAN)){
    PROJCLEAN -> CLEAN
  }
  if(!is.null(PROJBAKED) & is.null(BAKED)){
    PROJBAKED -> BAKED
  }
  if(!is.null(PROJFINAL) & is.null(FINAL)){
    PROJFINAL -> FINAL
  }
  if(!is.null(PROJSHARED) & is.null(SHARED)){
    PROJSHARED -> SHARED
  }

  assign("RPROJ", list(
    PROJHOME = PROJHOME, 
    PROJRAW = PROJRAW,
    PROJCLEAN = PROJCLEAN,
    PROJBAKED = PROJBAKED,
    PROJFINAL = PROJFINAL,
    PROJSHARED = PROJSHARED,
    HOME = HOME, 
    RAW = RAW,
    CLEAN = CLEAN,
    BAKED = BAKED,
    FINAL = FINAL,
    SHARED = SHARED
  ), envir=globalenv())
  
  assign("PROJ", list(
    HOME = HOME, 
    RAW = RAW,
    CLEAN = CLEAN,
    BAKED = BAKED,
    FINAL = FINAL,
    SHARED = SHARED
  ), envir=globalenv())
  
  for(i in names(PROJ)){
    if(!is.null(PROJ[[i]])) if(!dir.exists(PROJ[[i]])) dir.create(PROJ[[i]], recursive=TRUE)  
  }

  setwd(PROJ$HOME)

  fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
  sapply(fileSources,source,.GlobalEnv)
 }
 
