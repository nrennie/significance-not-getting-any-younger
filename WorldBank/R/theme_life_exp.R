#' Life Expectancy custom ggplot2 theme
#'
#' Function to customise layout of ggplot2 plots for shiny app.
#' @import ggplot2
#' @export

theme_life_exp <- function(){

  #start with light theme as base
  theme_light() %+replace%

    theme(
      #panel elements
      panel.background = element_rect(fill = "white", colour="white"),
      panel.grid.minor = element_blank(),

      #plot elements
      plot.background = element_rect(fill = "white", colour="white"),
      plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left

      #title elements
      plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
      plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
      plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),

      #legend elements
      legend.position="none",
      legend.key = element_rect(colour = "white", fill="white"),
      legend.spacing.x = unit(0.5,"cm"),
      legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
      legend.text = element_text(colour="black", size=10, hjust = 0),
      legend.background = element_rect(fill = "transparent"),

      #axis elements
      axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
      axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
      axis.ticks=element_blank()
    )
}
