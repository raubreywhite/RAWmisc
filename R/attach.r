
#' Bulk loads the most common analysis libraries
Libraries <- function(
  lib=c(
    "data.table",
    "tidyr",
    "magrittr",
    "grid",
    "stringr",
    "R6",
    "ggplot2",
    "dplyr",
    "gridExtra",
    "knitr",
    "rmarkdown",
    "brew",
    "scales",
    "Hmisc",
    "Gmisc",
    "htmlTable",
    "ggvis",
    "Formula",
    "roxygen2",
    "packrat",
    "rms",
    "survey"
  )){
  for(i in lib) library(i, character.only=TRUE, warn.conflicts=FALSE)
}