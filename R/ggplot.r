#' Formats ggplot to a more minimalistic style
FormatGGPlot <- function(
  q,
  sizeMultiplier = 1,
  legendKey = 1,
  xAngle=0,
  stripes=FALSE
  ){

  q <- q + theme(axis.ticks         = element_line(colour="black"))
  q <- q + theme(panel.background   = element_rect(
                                        colour="white",
                                        fill="white"
                                      ))
  q <- q + theme(axis.line          = element_line(
                                        colour="black",
                                        size=0.5*sizeMultiplier
                                      ))
  q <- q + theme(strip.background   = element_blank())
  q <- q + theme(panel.grid.major   = element_blank())
  q <- q + theme(panel.grid.minor   = element_blank())
  if(stripes){
    q <- q + theme(panel.grid.major = element_line(
                                        colour="black",
                                        size=0.25*sizeMultiplier,
                                        linetype=3
                                      ))
    q <- q + theme(panel.grid.minor = element_line(
                                        colour="black",
                                        size=0.25*sizeMultiplier,
                                        linetype=3
                                       ))
  }
  q <- q + theme(legend.key.size 	  = unit(legendKey,"lines"))
  q <- q + theme(legend.key         = element_blank())
  q <- q + theme(axis.title.x 	    = element_text(size=10*sizeMultiplier,vjust=0,colour="black"))
  q <- q + theme(axis.title.y		    = element_text(size=10*sizeMultiplier,angle=90,vjust=0.25,colour="black"))
  q <- q + theme(axis.text.y		    = element_text(size=10*sizeMultiplier,hjust=1,vjust=0.4,colour="black"))
  q <- q + theme(axis.text.x        = element_text(size=10*sizeMultiplier,hjust=0.5,vjust=1,colour="black",angle=xangle))
  if(xangle!=0){
    q <- q + theme(axis.text.x      = element_text(size=10*sizeMultiplier,hjust=0,vjust=0.5,colour="black",angle=xangle))
  }
  q <- q + theme(strip.text.y       = element_text(size=10*sizeMultiplier,hjust=0.5,colour="black"))
  q <- q + theme(strip.text.x       = element_text(size=10*sizeMultiplier,hjust=0.5,colour="black"))
  q <- q + theme(legend.text        = element_text(size=10*sizeMultiplier,hjust=0.5,colour="black"))
  q <- q + theme(legend.title       = element_text(size=10*sizeMultiplier,hjust=0.5,colour="black"))
  q <- q + theme(legend.position    = "right")
  q <- q + theme(plot.margin        = unit(c(0.5,0.5,1,0.5),"lines"))
  q <- q + theme(plot.title         = element_text(size=14*sizeMultiplier,hjust=0.5,vjust=1))

  return(q)
}

#' Makes a footnote on the bottom right hand corner
#' Taken from Mark Heckmann
#' https://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
MakeFootnote <- function(
  footnoteText = strftime(Sys.time(), format="%d/%m/%Y"),
  size = 1.2,
  color = "black"
  ){
  
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
    x = unit(1,"npc") - unit(2, "mm"),
    y = unit(2, "mm"),
    just = c("right", "bottom"),
    gp = gpar(cex = size, col = color))
  popViewport()
}

#' Creates png with default dimensions of A4
png_a4 <- function(
  file = "Figure.png",
  w = 1, 
  h = 1, 
  landscale = FALSE
  ){

  width <- 2480
  height <- 3508

  if(landscape){
    width <- 3508
    height <- 2480
  }

  width %<>% times(w)
  height %<>% times(h)

  png(file, width = width, height = height)
}






