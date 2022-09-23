#' Lab4: Linkoping University theme
#' @import grDevices
#' @importFrom ggplot2 %+replace%
#' @description the function plot graph with university's theme
#' @export
Theme_linkoping<-function()
{
  library(ggplot2)
  ggplot2::theme_set(theme_cowplot())
  myplot<-ggdraw() + ggplot2::theme(plot.background = element_rect(fill = '#00b9e7', colour = NA),
                           panel.grid = element_line(colour = "#00b9e7"),
                           text = element_text(colour = "#ffffff"),
                           panel.border = element_rect( fill = NA,colour = "black", size = NA),
                           axis.text = element_text(colour = "#ffffff", size = 10),
                           plot.title = element_text(size = 15,hjust = 0),
                           plot.caption = element_text(family = "font", size = 8,color = "black")) +
    draw_image("https://liu.se/polopoly/KoM/facebookbilder/linkuniv_sigill_facebook_314px.jpg", scale = 0.3, x = -0.3, y = -0.3)


  return(myplot)

}
#Theme_linkoping()
