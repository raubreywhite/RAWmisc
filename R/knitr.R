PandocInstalled <- function(){
  sink(tempfile())
  pandoc.installed <- system('pandoc -v')==0
  sink()
  if(pandoc.installed) return(TRUE)
  
  rstudio.environment.installed <- Sys.getenv("RSTUDIO_PANDOC")
  if(rstudio.environment.installed!=""){
    rstudio.environment.installed <- paste0('"',rstudio.environment.installed,'/pandoc" -v')
    sink(tempfile())
    rstudio.environment.installed <- system(rstudio.environment.installed)==0
    sink()
  } else rstudio.environment.installed <- FALSE
  if(rstudio.environment.installed) return(TRUE)
  
  sink(tempfile())
  rstudio.pandoc.installed <- system('"C:/Program Files/RStudio/bin/pandoc/pandoc" -v')==0
  sink()
  if(rstudio.pandoc.installed){
    Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc") 
  }
  if(rstudio.pandoc.installed) return(TRUE)
  
  return(FALSE)
}

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
  
  outFile <- stringr::str_split(outFile,"/") %>%
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
  if(PandocInstalled()){
    RMDToHTMLPandoc(inFile=inFile,outFile=outFile, tocDepth=tocDepth)
  } else {
    RMDToHTMLKnitr(inFile=inFile,outFile=outFile, tocDepth=tocDepth)
  }
}

#' Uses preset CSS to knit html report
#' Also defaults to base64 inline images
#' CSS file taken from Max Gordon (http://gforge.se/packages/)
#' If pandoc is available, it uses pandoc (and hence bibliography/citations)
#' Otherwise uses knitr and no bibliography/citations
RmdToDOCX <- function(inFile="",outFile="", tocDepth=2){
  if(!PandocInstalled()) stop("pandoc not installed")

  outFile <- stringr::str_split(outFile,"/") %>%
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
    output_format=docx_document(toc=TRUE,toc_depth=tocDepth))
}

