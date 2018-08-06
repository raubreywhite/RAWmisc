#' Wrapper for RClone Sync
#' @param from a
#' @param to a
#' @param createLocalIfRemoteDoesntExist a
#' @importFrom processx run
#' @importFrom stringr str_detect
#' @export RCloneSync
RCloneSync <- function(from,to,createLocalIfRemoteDoesntExist=FALSE){
  from <- gsub("//","/",from)
  to <- gsub("//","/",to)

  if(processx::run("which","rclone",error_on_status=F, echo=F)$status!=0){
    warning("RClone not installed")
    return(0)
  }
  if(!file.exists("/home/rstudio/.config/rclone/rclone.conf")){
    warning("/home/rstudio/.config/rclone/rclone.conf doesnt exist")
    return(0)
  }

  fromIsRemote <- stringr::str_detect(from,":")
  if(fromIsRemote){
    if(!CONFIG$FORCE_RCLONE_RESYNC & file.exists(file.path(to,"xxxxx_data_downloaded_4938"))){
      warning("Data has already been downloaded")
      warning("Data will not be redownloaded")
      return(0)
    }
  } else {
    if(file.exists(file.path(from,"xxxxx_data_downloaded_4938"))){
      unlink(file.path(from,"xxxxx_data_downloaded_4938"))
    }
  }

  output <- processx::run(
    "rclone",
    c("ls",from),
    error_on_status=F, echo=F)

  if(fromIsRemote & createLocalIfRemoteDoesntExist & output$status!=0){
    # from folder doesn't exist, create local folder
    warning(sprintf("%s did not exist remotely, creating %s locally",from,to))
    processx::run(
      "mkdir",
      c("-p",to)
    )
  } else if(output$status==0) {
    # from folder does exist
    output <- processx::run(
      "rclone",
      c("sync",from,to),
      error_on_status=F, echo=F)

    if(output$status!=0){
      warning("SYNC FAILED")
    }
  }
}

#' Allows for InitialiseProject to manipulate files
#' on your system
#' @export AllowFileManipulationFromInitialiseProject
AllowFileManipulationFromInitialiseProject <- function(){
  CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT <- TRUE
}

#' Allows for InitialiseProject to manipulate files
#' on your system
#' @export UseRClone
UseRClone <- function(){
  if(dir.exists("/home/xrstudio/.config/rclone")){
    if(!dir.exists("/home/rstudio/.config")) dir.create("/home/rstudio/.config")
    if(!dir.exists("/home/rstudio/.config/rclone")) file.copy("/home/xrstudio/.config/rclone","/home/rstudio/.config/",recursive = T)
  }
  if(file.exists("/home/rstudio/.config/rclone/rclone.conf")) CONFIG$USE_RCLONE <- TRUE
}

#' Forces RCloneSync to resync, even if
#' already downloaded
#' @export ForceRCloneResync
ForceRCloneResync <- function(){
  CONFIG$FORCE_RCLONE_RESYNC <- TRUE
}

#' Initialises project
#' @param HOME a
#' @param RAW a
#' @param CLEAN a
#' @param BAKED a
#' @param FINAL a
#' @param SHARED a
#' @param RCLONE_RAW a
#' @param RCLONE_CLEAN a
#' @param RCLONE_BAKED a
#' @param RCLONE_FINAL a
#' @param RCLONE_SHARED a
#' @importFrom lubridate today
#' @importFrom stringr str_detect
#' @export InitialiseProject
InitialiseProject <- function(HOME=NULL,
                              RAW=NULL,
                              CLEAN=NULL,
                              BAKED=NULL,
                              FINAL=NULL,
                              SHARED=NULL,
                              RCLONE_RAW=NULL,
                              RCLONE_CLEAN=NULL,
                              RCLONE_BAKED=NULL,
                              RCLONE_FINAL=NULL,
                              RCLONE_SHARED=NULL){

  PROJ$HOME <- HOME
  PROJ$RAW <- RAW
  PROJ$CLEAN <- CLEAN
  PROJ$BAKED <- BAKED
  PROJ$FINAL <- FINAL
  PROJ$SHARED <- SHARED

  PROJ$RCLONE_RAW <- RCLONE_RAW
  PROJ$RCLONE_CLEAN <- RCLONE_CLEAN
  PROJ$RCLONE_BAKED <- RCLONE_BAKED
  PROJ$RCLONE_FINAL <- RCLONE_FINAL
  PROJ$RCLONE_SHARED <- RCLONE_SHARED

  if(is.null(PROJ$SHARED)){
    PROJ$SHARED_TODAY <- NULL
    PROJ$RCLONE_SHARED_TODAY <- NULL
  } else {
    PROJ$SHARED_TODAY <- file.path(PROJ$SHARED,lubridate::today())
    if(!is.null(PROJ$RCLONE_SHARED)) PROJ$RCLONE_SHARED_TODAY <- file.path(PROJ$RCLONE_SHARED,lubridate::today())
  }

  if(CONFIG$USE_RCLONE){
    for(i in c("RAW","CLEAN","BAKED","FINAL","SHARED_TODAY")){
      if(!is.null(PROJ[[i]]) & !is.null(PROJ[[sprintf("RCLONE_%s",i)]])) RCloneSync(
        from=PROJ[[sprintf("RCLONE_%s",i)]],
        to=PROJ[[i]],
        createLocalIfRemoteDoesntExist=T)
    }
  }

  if(CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT){
    for(i in c("HOME","RAW","CLEAN","BAKED","FINAL","SHARED","SHARED_TODAY")){
      if(!is.null(PROJ[[i]])) if(!dir.exists(PROJ[[i]])) dir.create(PROJ[[i]], recursive=TRUE)
    }

    # Delete empty folders in shared folder
    if(!is.null(PROJ$SHARED)) for(f in list.files(PROJ$SHARED)){
      if(stringr::str_detect(f,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")) if(f==lubridate::today()) next # don't want to delete today's folder
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

#' Initialises project on a unix machine in an opinionated manner
#' @param project location of project
#' @export InitialiseOpinionatedUnix
InitialiseOpinionatedUnix <- function(project){
  if(.Platform$OS.type=="unix"){
    UseRClone()
    AllowFileManipulationFromInitialiseProject()

    if(dir.exists("/dropbox")){
      SHARED <- sprintf("/dropbox/analyses/results_shared/%s",project)
      RCLONE_SHARED <- NULL
    } else {
      SHARED <- sprintf("/tmp/results_shared/%s",project)
      RCLONE_SHARED <- sprintf("data:/analyses/results_shared/%s/",project)
    }

    InitialiseProject(
      HOME = sprintf("/git/%s/",project),
      RAW = sprintf("/tmp/data_raw/%s/",project),
      CLEAN = sprintf("/tmp/data_clean/%s/",project),
      BAKED = sprintf("/tmp/results_baked/%s/",project),
      FINAL = sprintf("/tmp/results_final/%s/",project),
      SHARED = SHARED,
      RCLONE_RAW = sprintf("crypt:/data_raw/%s/",project),
      RCLONE_SHARED = RCLONE_SHARED
    )
  }
}

#' Initialises project
#' @export SaveProject
SaveProject <- function(){
  if(CONFIG$USE_RCLONE){
    for(i in c("CLEAN","BAKED","FINAL","SHARED_TODAY")){
      if(!is.null(PROJ[[i]]) & !is.null(PROJ[[sprintf("RCLONE_%s",i)]])) RCloneSync(
        from=PROJ[[i]],
        to=PROJ[[sprintf("RCLONE_%s",i)]])
    }
  }
}

