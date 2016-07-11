theme_SMAO <- function(base_size = 12, base_family = "") {
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
    axis.text.x =        element_text(vjust = 1, margin=margin(5,5,10,5,"pt")),
    axis.text.y =        element_text(hjust = 1, margin=margin(5,5,10,5,"pt")),
    axis.ticks =         element_line(),
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(vjust = 1),
    axis.title.y =       element_text(angle = 90, vjust=1),
    axis.ticks.length =  unit(0.3, "lines"),
    
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
    panel.border =       element_rect(fill = NA, colour = "black"),
    panel.grid.major =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.grid.minor =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "white", colour = "white", size=3),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),
    
    plot.background =    element_rect(colour = NA, fill = NA),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(2, 2, 2, 2), "lines"),
    
    complete = TRUE
  )
}

attr(theme_SMAO(), "complete")

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

attr(theme_SMAO45(), "complete")
