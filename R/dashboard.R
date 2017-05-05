#' If folders are setup according to the
#' dashboard philosophy, then this function
#' sets RPROJ
#' @export DashboardInitialiseClean
DashboardInitialiseClean <- function(){
  if (Sys.getenv("INSIDE_HOME") != "") {
    INSIDE_HOME <- Sys.getenv("INSIDE_HOME")
  } else {
    INSIDE_HOME <- rev(stringr::str_split(getwd(),"/")[[1]])[2]
  }

  if (Sys.getenv("NAME") != "") {
    NAME <- Sys.getenv("NAME")
  } else {
    NAME <- rev(stringr::str_split(getwd(),"/")[[1]])[1]
  }

  if (Sys.getenv("STUB") != "") {
    STUB <- Sys.getenv("STUB")
  } else {
    STUB <- stringr::str_replace(getwd(),file.path(INSIDE_HOME,NAME),"")
  }

  PROJ$NAME <- NAME
  PROJ$STUB <- STUB
  PROJ$INSIDE_HOME <- INSIDE_HOME

  setwd(file.path(PROJ$STUB,PROJ$INSIDE_HOME,PROJ$NAME))

   fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
    sapply(fileSources, source, .GlobalEnv)
}

#' If folders are setup according to the
#' dashboard philosophy, then this function
#' finds folders according to the dashboard
#' @param inside where it is inside
#' @param f an optional file
#' @export DashboardFolder
DashboardFolder <- function(inside="data_raw",f=NULL){
  retVal <- file.path(PROJ$STUB,inside,PROJ$NAME)
  retVal <- paste0(retVal,"/")
  if(!is.null(f)){
    retVal <- file.path(retVal,f)
  }
  return(retVal)
}

#' Sends out mass emails that are stored in an xlsx file
#' @param emailBCC a
#' @param emailSubject a
#' @param emailText a
#' @param XLSXLocation a
#' @param OAUTHLocation a
#' @importFrom magrittr %>%
#' @export DashboardEmail
DashboardEmail <- function(emailBCC,
                           emailSubject,
                           emailText,
                           XLSXLocation=file.path("etc","gmailr","emails.xlsx"),
                           OAUTHLocation=file.path("etc","gmailr",".httr-oauth")){
  emails <- readxl::read_excel(XLSXLocation)
  emails <- stats::na.omit(emails[[emailBCC]])

  emailText <- paste0(emailText,
    "<br><br><br>
    ------------------------
    <br>
    DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
    <br>
    To add or remove people to/from this notification list, send their details to richard.white@fhi.no
    ")

  gmailr::mime() %>%
    gmailr::to("dashboards@fhi.no") %>%
    gmailr::from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    gmailr::bcc(paste0(emails,collapse=",")) %>%
    gmailr::subject(emailSubject) %>%
    gmailr::html_body(emailText) -> text_msg

  currentWD <- getwd()
  tmp <- tempdir()
  file.copy(OAUTHLocation,paste0(tmp,"/.httr-oauth"))
  setwd(tmp)
  gmailr::gmail_auth()
  gmailr::send_message(text_msg)
  setwd(currentWD)
}

