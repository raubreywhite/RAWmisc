#' PROJ
#' @export PROJ
PROJ <- new.env(parent = emptyenv())

#' CONFIG
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT <- FALSE
CONFIG$USE_RCLONE <- FALSE

#' BLAH
#' @export NORCHAR
NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u00C6"
NORCHAR$ae <- "\u00E6"

