#' Makes a footnote on the bottom right hand corner
#' Taken from Mark Heckmann
#' https://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
#' @param footnoteText a
#' @param size a
#' @param color a
#' @export MakeFootnote
MakeFootnote <- function(
  footnoteText = strftime(Sys.time(), format="%d/%m/%Y"),
  size = 1.2,
  color = "black"
  ){

  grid::pushViewport(grid::viewport())
  grid::grid.text(label = footnoteText ,
    x = grid::unit(1,"npc") - grid::unit(2, "mm"),
    y = grid::unit(2, "mm"),
    just = c("right", "bottom"),
    gp = grid::gpar(cex = size, col = color))
  grid::popViewport()
}

#' Creates png with default dimensions of A4
#' @param file a
#' @param w a
#' @param h a
#' @param landscape a
#' @export png_a4
png_a4 <- function(
  file = "Figure.png",
  w = 1,
  h = 1,
  landscape = FALSE
  ){

  width <- 2480
  height <- 3508

  if(landscape){
    width <- 3508
    height <- 2480
  }

  width <- width * w
  height <- height * h

  grDevices::png(file, width = width, height = height)
}

#' Creates png with default dimensions of half A4
#' @param file a
#' @param w a
#' @param h a
#' @param landscape a
#' @export SMAOpng
SMAOpng <- function (file = "Figure.png", w = 1, h = 1, landscape = TRUE)
{
  width <- 2480/2
  height <- 3508/2
  if (landscape) {
    width <- 3508/2
    height <- 2480/2
  }
  width <- width * w
  height <- height * h

  grDevices::png(file, width = width, height = height)
}

#' Internal version 1 of a minimalist ggplot theme
#' @param base_size base size
#' @param base_family text family
#' @import ggplot2
#' @export theme_SMAO_V1
theme_SMAO_V1 <- function(base_size = 24, base_family = "") {
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      color = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                      margin = margin(), debug = FALSE),
    axis.text =          element_text(size = rel(1), colour = "black", margin=margin(unit(c(2, 2, 2, 2), "lines"))),
    strip.text =         element_text(size = rel(1), colour = "black"),

    axis.line.x =        element_line(size=base_size/20),
    axis.line.y =        element_line(size=base_size/20),
    axis.text.x =        element_text(vjust = 1, margin=margin(0.01*base_size,0,0.05*base_size,0,"lines")),
    axis.text.y =        element_text(hjust = 1, margin=margin(0,0.01*base_size,0,0.05*base_size,"lines")),
    axis.ticks =         element_line(),
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(vjust = 0, margin=margin(5,5,10,5,"pt")),
    axis.title.y =       element_text(angle = 90, vjust=0, margin=margin(5,5,10,5,"pt")),
    axis.ticks.length =  unit(0.03*base_size, "lines"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "white", colour = "black"),
    legend.key.size =    unit(0.1*base_size, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8), colour = "black"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0, colour = "white"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = NA, colour = NA),
    panel.border =       element_rect(fill = NA, colour = NA),
    panel.grid.major =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.grid.minor =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = "white", colour = "white", size=3),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),

    plot.background =    element_rect(colour = NA, fill = NA),
    plot.title =         element_text(size = rel(1.2), vjust=1, margin=margin(0,0,0.1*base_size,0,"lines")),
    plot.margin =        unit(c(2, 2, 2, 2), "lines"),

    panel.margin =        unit(c(2, 2, 2, 2), "lines"),

    complete = TRUE
  )
}

#' Internal version 2 of a minimalist ggplot theme
#' @param base_size base size
#' @param base_family text family
#' @import ggplot2
#' @export theme_SMAO_V2
theme_SMAO_V2 <- function(base_size=24, base_family=""){
  half_line <- base_size / 2
  theme_gray(base_size) +
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    axis.line =          element_line(size=base_size/20),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(1), colour = "black", margin=margin(unit(c(2, 2, 2, 2), "lines"))),
    axis.text.x =        element_text(vjust = 1, margin=margin(0.01*base_size,0,0.05*base_size,0,"lines")),
    axis.text.x.top =    element_text(vjust = 1, margin=margin(0.01*base_size,0,0.05*base_size,0,"lines")),
    axis.text.y =        element_text(hjust = 1, margin=margin(0,0.01*base_size,0,0.05*base_size,"lines")),
    axis.text.y.right =  element_text(hjust = 1, margin=margin(0,0.01*base_size,0,0.05*base_size,"lines")),
    axis.ticks =         element_line(),
    axis.ticks.length =  unit(0.03*base_size, "lines"),

    legend.key =         element_rect(fill = "white", colour = "black"),
    legend.key.size =    unit(0.1*base_size, "lines"),

    panel.background =   element_rect(fill = NA, colour = NA),
    panel.grid.major =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.grid.minor =   element_line(colour = "black", size = rel(0.4), linetype=3),

    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text =         element_text(),
    complete = TRUE
  )

}

#' Internal version 1 of a minimalist ggplot theme
#' @param base_size base size
#' @param base_family text family
#' @import ggplot2
#' @export theme_SMAO45
theme_SMAO45 <- function(base_size = 12, base_family = "") {
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      color = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                      margin = margin(), debug = FALSE),
    axis.text =          element_text(size = rel(0.8), colour = "black"),
    strip.text =         element_text(size = rel(0.8), colour = "black"),

    axis.line.x =        element_line(size=base_size/20),
    axis.line.y =        element_line(size=base_size/20),
    axis.text.x =        element_text(vjust = 1,angle=-45,hjust=0, margin=margin(5,5,10,5,"pt")),
    axis.text.y =        element_text(hjust = 1, margin=margin(5,5,10,5,"pt")),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(vjust = 1),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.3, "lines"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "white", colour = "black"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8), colour = "black"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0, colour = "white"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "white", colour = "black"),
    panel.border =       element_rect(fill = NA, colour = "white"),
    panel.grid.major =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.grid.minor =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = NA, colour = NA),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),

    plot.background =    element_rect(colour = NA, fill = NA),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 1, 1), "lines"),

    complete = TRUE
  )
}

#' Wrapper for ggplot theme
#' @param base_size base size
#' @param base_family text family
#' @param v version
#' @export theme_SMAO
theme_SMAO <- function(base_size = 24, base_family = "", v=1) {
  if(v==1){
   return(theme_SMAO_V1(base_size=base_size, base_family=base_family))
  } else if(v==2){
   return(theme_SMAO_V2(base_size=base_size, base_family=base_family))
  }
}


