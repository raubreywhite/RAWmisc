#' Allows for InitialiseProject to manipulate files
#' on your system
#' @export AllowFileManipulationFromInitialiseProject
AllowFileManipulationFromInitialiseProject <- function(){
  CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT <- TRUE
}

#' Initialises project
#' @param HOME a
#' @param RAW a
#' @param CLEAN a
#' @param BAKED a
#' @param FINAL a
#' @param SHARED a
#' @export InitialiseProject
InitialiseProject <- function(HOME=NULL,
                              RAW=NULL,
                              CLEAN=NULL,
                              BAKED=NULL,
                              FINAL=NULL,
                              SHARED=NULL){

  PROJ$HOME <- HOME
  PROJ$RAW <- RAW
  PROJ$CLEAN <- CLEAN
  PROJ$BAKED <- BAKED
  PROJ$FINAL <- FINAL
  PROJ$SHARED <- SHARED
  if(is.null(PROJ$SHARED)){
    PROJ$SHARED_TODAY <- NULL
  } else {
    PROJ$SHARED_TODAY <- file.path(SHARED,lubridate::today())
  }

  if(CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT){
    for(i in names(PROJ)){
      if(!is.null(PROJ[[i]])) if(!dir.exists(PROJ[[i]])) dir.create(PROJ[[i]], recursive=TRUE)
    }

    # Delete empty folders in shared folder
    if(!is.null(PROJ$SHARED)) for(f in list.files(PROJ$SHARED)){
      f2 <- file.path(PROJ$SHARED,f)
      if(length(list.files(f2))==0){
        unlink(f2, recursive = T)
      }
    }
  }

  setwd(PROJ$HOME)

  fileSources = file.path("code",list.files("code",pattern="*.[rR]$"))
  sapply(fileSources,source,.GlobalEnv)
 }

