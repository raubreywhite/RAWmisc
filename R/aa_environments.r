#' PROJ
#' @export PROJ
PROJ <- new.env(parent = emptyenv())

#' CONFIG
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$ALLOW_FILE_MANIPULATION_FROM_INITIALISE_PROJECT <- FALSE
CONFIG$USE_RCLONE <- FALSE
CONFIG$FORCE_RCLONE_RESYNC <- FALSE
CONFIG$UUID_DIGITS <- 4

#' BLAH
#' @export NORCHAR
NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u00C6"
NORCHAR$ae <- "\u00E6"

#' BLAH
#' @export CONFIG_STACK
CONFIG_STACK <- new.env(parent = emptyenv())
CONFIG_STACK$GRAPH_VARS <- c("graphExposureScaleMultiply",
               "graphExposureScaleAdd",
               "graphReference",
               "graphExposureLocations",
               "graphFileName",
               "graphTitleMain",
               "graphTitleX"
               )
