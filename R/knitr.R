#' Wrapper around rmarkdown::render for HTML files
#' @param inFile a
#' @param outFile a
#' @param copyFrom a
#' @param ... a
#' @export RmdToHTML
RmdToHTML <- function (inFile = "", outFile = "", copyFrom = NULL, ...)
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

    outFile <- unlist(stringr::str_split(outFile, "/"))
    if (length(outFile) == 1) {
      outDir <- getwd()
    }
    else {
      outDir <- file.path(getwd(), outFile[-length(outFile)])
      outFile <- outFile[length(outFile)]
    }

    output_format <- rmarkdown::html_document(...)

    rmarkdown::render(input = inFile, output_file = outFile,
                      output_dir = outDir, output_format =output_format)

  }, TRUE)
  if (!is.null(copyFrom)) {
    file.remove(inFile)
  }
}

#' Wrapper around rmarkdown::render for docx files
#' @param inFile a
#' @param outFile a
#' @param copyFrom a
#' @param ... a
#' @export RmdToDOCX
RmdToDOCX <- function (inFile = "", outFile = "", copyFrom = NULL, ...)
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
    outDir <- tempdir()
    originalOutFile <- outFile

    outFile <- unlist(stringr::str_split(outFile, "/"))
    if (length(outFile) == 1) {
      #outDir <- getwd()
    }
    else {
      #outDir <- file.path(getwd(), outFile[-length(outFile)])
      outFile <- outFile[length(outFile)]
    }

    rmarkdown::render(input = inFile, output_file = outFile,
                      output_dir = outDir, output_format = rmarkdown::word_document(...))

    cmd <- paste0("rm -f ",file.path(getwd(),originalOutFile))
    system(cmd)
    print(cmd)
    cmd <- paste0("cp -f ",file.path(outDir,outFile)," ",file.path(getwd(),originalOutFile))
    system(cmd)
    print(cmd)
  }, TRUE)
  if (!is.null(copyFrom)) {
    file.remove(inFile)
  }
}

