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

#' Makes a footnote on the bottom right hand corner
#' @param footnoteText a
#' @param size a
#' @param color a
#' @export FootnoteGridArrange
FootnoteGridArrange <- function(
  footnoteText = strftime(Sys.time(), format="%d/%m/%Y"),
  size = 1.2,
  color = "black"
){
  grid::textGrob(
    footnoteText,
    gp = grid::gpar(cex = size),
    hjust = 1,
    x = 1
  )
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

#' theme_gray from ggplot2, but with responsive legend.key.size and panel.grid.major
#' @param base_size base size
#' @param base_family text family
#' @import ggplot2
#' @export theme_gray
theme_gray <- function(base_size=24, base_family=""){
  half_line <- base_size / 2
  theme_gray(base_size) +
    theme(
      # Elements in this first block aren't used directly, but are inherited
      # by others
      legend.key.size =    unit(0.11*base_size, "lines"),
      panel.grid.major =   element_line(colour = "white", size = base_size*0.045),
      panel.grid.minor =   element_line(colour = "white", size = half_line*0.045)
    )

}

#' theme_gray from ggplot2, but with responsive legend.key.size and panel.grid.major
#' @param q base size
#' @param filename text family
#' @param landscape test
#' @importFrom ggplot2 ggsave
#' @export saveA4
saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

