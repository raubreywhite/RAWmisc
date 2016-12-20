DashboardsEmail <- function(emailBCC,emailSubject,emailText){
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
