#' Initialises project
#' @param PROJHOME a
#' @param PROJRAW a
#' @param PROJCLEAN a
#' @param PROJBAKED a
#' @param PROJFINAL a
#' @param PROJSHARED a
#' @param HOME a
#' @param RAW a
#' @param CLEAN a
#' @param BAKED a
#' @param FINAL a
#' @param SHARED a
#' @export InitialiseProject
InitialiseProject <- function(PROJHOME=NULL,
                              PROJRAW=NULL,
                              PROJCLEAN=NULL,
                              PROJBAKED=NULL,
                              PROJFINAL=NULL,
                              PROJSHARED=NULL,
                              HOME=NULL,
                              RAW=NULL,
                              CLEAN=NULL,
                              BAKED=NULL,
                              FINAL=NULL,
                              SHARED=NULL){
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

  RPROJ$PROJHOME = PROJHOME
  RPROJ$PROJRAW = PROJRAW
  RPROJ$PROJCLEAN = PROJCLEAN
  RPROJ$PROJBAKED = PROJBAKED
  RPROJ$PROJFINAL = PROJFINAL
  RPROJ$PROJSHARED = PROJSHARED
  RPROJ$HOME = HOME
  RPROJ$RAW = RAW
  RPROJ$CLEAN = CLEAN
  RPROJ$BAKED = BAKED
  RPROJ$FINAL = FINAL
  RPROJ$SHARED = SHARED

  PROJ$HOME <- HOME
  PROJ$RAW <- RAW
  PROJ$CLEAN <- CLEAN
  PROJ$BAKED <- BAKED
  PROJ$FINAL <- FINAL
  PROJ$SHARED <- SHARED
  PROJ$SHARED_TODAY <- file.path(SHARED,lubridate::today())

  # Delete empty folders in shared folder
  for(f in list.files(PROJ$SHARED)){
    f2 <- file.path(PROJ$SHARED,f)
    if(length(list.files(f2))==0){
      unlink(f2, recursive = T)
    }
  }

  for(i in names(PROJ)){
    if(!is.null(PROJ[[i]])) if(!dir.exists(PROJ[[i]])) dir.create(PROJ[[i]], recursive=TRUE)
  }

  setwd(PROJ$HOME)

  fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
  sapply(fileSources,source,.GlobalEnv)
 }

