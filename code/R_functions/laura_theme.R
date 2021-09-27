laura_theme <- function(){
  theme(
    panel.border = element_rect(colour="black", fill="transparent", size=1),
    panel.background = element_blank(),
    plot.background = element_blank(), 
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key = element_rect(fill="transparent", colour=NA),
    axis.line = element_line(colour="black"),
    panel.grid.minor = element_line(colour="transparent"),
    text=element_text(family="Arial"),
    axis.title.x = element_text(size=20, vjust=-.25),
    axis.title.y = element_text(size=20, vjust=1),
    axis.text = element_text(size=20, colour="black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20),
    legend.text = element_text(size=14),
    legend.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5)
  )
} 
  