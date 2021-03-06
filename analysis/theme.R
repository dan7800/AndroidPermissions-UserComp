get.theme <- function(){
  plot.theme <-
    theme_bw() +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", margin = ggplot2::margin(5,0,15,0),
        hjust = 0.5
      ),
      axis.text.x = element_text(
        size=10, colour = "black", angle = 50, vjust = 1, hjust = 1
      ),
      axis.title.x = element_text(
        colour = "black", face = "bold", margin = ggplot2::margin(10,0,5,0)
      ),
      axis.text.y = element_text(size=10, colour = "black"),
      axis.title.y = element_text(
        colour = "black", face = "bold", margin = ggplot2::margin(0,10,0,5)
      ),
      strip.text.x = element_text(size=10, face="bold"),
      legend.position = "bottom"
    )
  return(plot.theme)
}
