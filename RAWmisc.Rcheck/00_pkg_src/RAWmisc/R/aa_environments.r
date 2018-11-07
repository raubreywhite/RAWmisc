#' Environment for holding configs
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$UUID_DIGITS <- 4

#' Norwegian characters in unicode
#' @export NORCHAR
NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u00C6"
NORCHAR$ae <- "\u00E6"

#' Environment for holding configs relevant to stack analyses
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
