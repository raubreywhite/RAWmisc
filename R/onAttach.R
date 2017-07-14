
.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  packageStartupMessage('www.github.com/raubreywhite/')
  if (interactive()) {
    packageStartupMessage('[interactive session]')
  }
}
