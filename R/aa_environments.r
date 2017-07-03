#' PROJ
#' @export PROJ
PROJ <- new.env(parent = emptyenv())
PROJ$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT <- FALSE

#' BLAH
#' @export NORCHAR
NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u01E2"
NORCHAR$ae <- "\u01E3"

