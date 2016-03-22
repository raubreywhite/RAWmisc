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
RmdToHTMLDOCX <- function(inFile="",outFile="", tocDepth=2, copyFrom=NULL){
  if(!is.null(copyFrom)){
    if(!stringr::str_detect(inFile,paste0("^",copyFrom,"/"))){
      stop(paste0("inFile does not start with ",copyFrom,"/ and you are using copyFrom=",copyFrom))
    }
    file.copy(inFile,gsub(paste0("^",copyFrom,"/"),"",inFile), overwrite=TRUE)
    inFile <- gsub(paste0("^",copyFrom,"/"),"",inFile)
  }
  
  try({
    if (RAWmisc::PandocInstalled()) {
      outFile <- unlist(stringr::str_split(outFile, "/"))
      if (length(outFile) == 1) {
        outDir <- getwd()
      } else {
        outDir <- file.path(getwd(), outFile[-length(outFile)])
        outFile <- outFile[length(outFile)]
      }
      css <- system.file("extdata","custom.css",package="RAWmisc")
      
      rmarkdown::render(
        input=inFile,
        output_file=outFile,
        output_dir=outDir,
        output_format=html_document(toc=TRUE,toc_depth=tocDepth,css=css))
    }
    else {
      
    }
  }, TRUE)
  
   if(!is.null(copyFrom)){
    file.remove(inFile)
  }
}

RMDToHTML <- function (inFile = "", outFile = "", copyFrom = NULL, tocDepth = 2, toc_float = TRUE, 
               number_sections = FALSE, fig_width = 7, fig_height = 5, fig_retina = if (!fig_caption) 2, 
               fig_caption = TRUE, dev = "png", code_folding = c("none", "show", "hide"), smart = TRUE, self_contained = TRUE, 
               theme = "paper", highlight = "default", mathjax = "default", 
               template = "default", includes=NULL) 
{
  if (!is.null(copyFrom)) {
    if (!stringr::str_detect(inFile, paste0("^", copyFrom, 
                                            "/"))) {
      stop(paste0("inFile does not start with ", copyFrom, 
                  "/ and you are using copyFrom=", copyFrom))
    }
    file.copy(inFile, gsub(paste0("^", copyFrom, "/"), "", 
                           inFile), overwrite = TRUE)
    inFile <- gsub(paste0("^", copyFrom, "/"), "", inFile)
  }
  try({
    if (RAWmisc::PandocInstalled()) {
      outFile <- unlist(stringr::str_split(outFile, "/"))
      if (length(outFile) == 1) {
        outDir <- getwd()
      }
      else {
        outDir <- file.path(getwd(), outFile[-length(outFile)])
        outFile <- outFile[length(outFile)]
      }
      
      output_format <- rmarkdown::html_document(toc=TRUE, tocDepth = tocDepth, 
                                           toc_float = toc_float, number_sections = number_sections, 
                                           fig_width = fig_width, fig_height = fig_height, 
                                           fig_retina = fig_retina, fig_caption = fig_caption, 
                                           dev = dev, code_folding = code_folding, smart = smart, 
                                           self_contained = self_contained, theme = theme, 
                                           highlight = highlight, mathjax = mathjax, template = template, includes=includes)
                                           
      rmarkdown::render(input = inFile, output_file = outFile, 
                        output_dir = outDir, output_format =output_format)
    }
    else {
    }
  }, TRUE)
  if (!is.null(copyFrom)) {
    file.remove(inFile)
  }
}

RmdToPres <- function(inFile = "", outFile = "", copyFrom=NULL){
  if(!is.null(copyFrom)){
    if(!stringr::str_detect(inFile,paste0("^",copyFrom,"/"))){
      stop(paste0("inFile does not start with ",copyFrom,"/ and you are using copyFrom=",copyFrom))
    }
    file.copy(inFile,gsub(paste0("^",copyFrom,"/"),"",inFile), overwrite=TRUE)
    inFile <- gsub(paste0("^",copyFrom,"/"),"",inFile)
  }
  try({
    if (RAWmisc::PandocInstalled()) {
      outFile <- unlist(stringr::str_split(outFile, "/"))
      if (length(outFile) == 1) {
        outDir <- getwd()
      } else {
        outDir <- file.path(getwd(), outFile[-length(outFile)])
        outFile <- outFile[length(outFile)]
      }
      unlink(paste0(outDir,"/pres_files"),recursive=TRUE,force=TRUE)
      
      revealjs_path <- system.file("reveal.js-3.2.0", package = "revealjs")
      system(paste0("robocopy ",revealjs_path," ",outDir,"/pres_files/reveal.js-3.2.0 /e /MT:32"))
      
      rmarkdown::render(input=inFile, output_file=outFile, output_dir=outDir,
                        output_format=revealjs::revealjs_presentation(
                          theme="night",
                          highlight="default",
                          smart=FALSE))
      
      unlink(paste0(outDir,"/pres_files"),recursive=TRUE,force=TRUE)
    }
    else {
      
    }
  }, TRUE)
   if(!is.null(copyFrom)){
    file.remove(inFile)
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

