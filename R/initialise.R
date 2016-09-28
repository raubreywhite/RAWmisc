
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
 
