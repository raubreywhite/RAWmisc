DashboardInitialiseClean <- function(){
  if (Sys.getenv("RPROJ") != "") {
        assign("RPROJ", list(PROJHOME = Sys.getenv("RPROJ")), envir = globalenv())
        setwd(RPROJ$PROJHOME)
  }
  PROJNAME <- rev(stringr::str_split(RPROJ$PROJHOME,"/")[[1]])[1]
  PROJSTUB <- gsub(paste0("cleaning/",PROJNAME),"",RPROJ$PROJHOME)
   assign("RPROJ", list(PROJHOME = RPROJ$PROJHOME, 
                        PROJNAME = PROJNAME,
                        PROJSTUB = PROJSTUB), 
            envir = globalenv())
   
   fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
    sapply(fileSources, source, .GlobalEnv)
}

DashboardFolder <- function(inside="data_raw",f=NULL){
  retVal <- paste0(RPROJ$PROJSTUB,inside,"/",RPROJ$PROJNAME,"/")
  if(!is.null(f)){
    retVal <- paste0(retVal,f)
  }
  return(retVal)
}

DashboardEmail <- function(emailBCC,emailSubject,emailText){
  emails <- readxl::read_excel("/etc/gmailr/emails.xlsx")
  emails <- na.omit(emails[[emailBCC]])
  
  mime() %>%
    to("dashboards@fhi.no") %>%
    from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    bcc(paste0(emails,collapse=",")) %>%
    subject(emailSubject) %>%
    text_body(emailText) -> text_msg
  
  currentWD <- getwd()
  tmp <- tempdir()
  file.copy("/etc/gmailr/.httr-oauth",paste0(tmp,"/.httr-oauth"))
  setwd(tmp)
  gmail_auth()
  send_message(text_msg)
  setwd(currentWD)
}
