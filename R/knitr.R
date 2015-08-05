#' Uses preset CSS to knit html report
#' Also defaults to base64 inline images
RmdToHTML <- function(inFile="",outFile=""){
  
  css <- system.file("extdata","custom.css",package="RAWmisc")
  css <- readChar(css, file.info(css)$size)
  
  opts_knit$set(upload.fun=image_uri)
  
  knit2html(
    inFile,
    outFile,
    options=c("toc", markdown::markdownHTMLOptions(TRUE)),
    header = c('<style type="text/css">', css, '</style>'))
}
