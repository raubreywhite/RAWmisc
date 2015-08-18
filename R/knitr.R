RMDToHTMLKnitr <- function(inFile="",outFile="", tocDepth=2){
  css <- system.file("extdata","custom.css",package="RAWmisc")
  css <- readChar(css, file.info(css)$size)
  
  opts_knit$set(upload.fun=image_uri)
  
  knit2html(
    inFile,
    outFile,
    options=c("toc", markdown::markdownHTMLOptions(TRUE)),
    header = c('<style type="text/css">', css, '</style>'))
}

RMDToHTMLPandoc <- function(inFile="", outFile="", tocDepth=2){
  css <- system.file("extdata","custom.css",package="RAWmisc")
  
  outFile <- str_split(outFile,"/") %>%
    unlist
  if(length(outFile)==1){
    outDir <- getwd()
  } else {
    outDir <- file.path(getwd(),outFile[-length(outFile)])
    outFile <- outFile[length(outFile)]
                  
  }
  
  rmarkdown::render(
    input=inFile,
    output_file=outFile,
    output_dir=outDir,
    output_format=html_document(toc=TRUE,toc_depth=tocDepth,css=css))
}


#' Uses preset CSS to knit html report
#' Also defaults to base64 inline images
#' CSS file taken from Max Gordon (http://gforge.se/packages/)
#' If pandoc is available, it uses pandoc (and hence bibliography/citations)
#' Otherwise uses knitr and no bibliography/citations
RmdToHTML <- function(inFile="",outFile="", tocDepth=2){
  pandoc.installed <- system('pandoc -v')==0
  if(pandoc.installed){
    RMDToHTMLPandoc(inFile=inFile,outFile=outFile, tocDepth=tocDepth)
  } else {
    RMDToHTMLKnitr(inFile=inFile,outFile=outFile, tocDepth=tocDepth)
  }
}


