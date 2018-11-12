#' PkgdownUndocumented
#'
#' @param pkgroot Root the package
#' @param pkgdown Location of the pkgdown.yml file
#' @importFrom yaml read_yaml
#' @importFrom pkgdown as_pkgdown
#' @export PkgdownUndocumented
PkgdownUndocumented <- function(pkgroot = ".", pkgdown = file.path(pkgroot, "pkgdown", "_pkgdown.yml")) {
  documentedYAML <- yaml::read_yaml(pkgdown)$reference
  documented <- c()
  for (i in seq_along(documentedYAML)) {
    documented <- c(documented, documentedYAML[[i]]$contents)
  }

  allYAML <- pkgdown::as_pkgdown(pkgroot)
  allYAML <- allYAML$topics[!allYAML$topics$internal, , drop = FALSE]
  if (nrow(allYAML) == 0) {
    return(TRUE)
  }
  undocumented <- allYAML$name[!allYAML$name %in% documented]
  if (length(undocumented) == 0) {
    return(TRUE)
  }

  for (i in undocumented) {
    message(i, " is undocumented")
  }
  stop("Undocumented functions")
}
