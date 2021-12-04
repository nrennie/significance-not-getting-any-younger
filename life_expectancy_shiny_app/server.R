library(shiny)
library(shinydashboard)
library(tidyverse)
library(extrafont)
library(plotly)
library(WorldBank) #devtools::install_github("nrennie/significance_early_career_writing_competition/WorldBank/")

#read in data
app_data <- readRDS("data/app_data.rds")

#### SERVER ####
server <- function(input, output) {
  output$lifePlot <- renderPlotly({
    select_data <- filter(app_data, 
                          country_name == input$country,
                          gender %in% input$gender) %>%
      pivot_longer(cols=3:63, 
                   names_to="year", 
                   values_to="Life Expectancy") %>%
      mutate(Year = as.numeric(year))
    
    #check if any variables have been selected
    if (nrow(select_data) < 1){
      p <- ggplot() +
        labs(title="Please select at least one of: Female, Male, Overall") +
        theme_life_exp()
    } 
    
    #plot life expectancy
    else{
      p <- ggplot() +
        geom_line(data=select_data, mapping=aes(x=Year, y=`Life Expectancy`, colour=gender)) +
        labs(title=paste("Life expectancy in ", input$country, sep=""), 
             caption = "N. Rennie | Source: World Bank World Development Indicators", 
             x="") +
        scale_colour_manual("", values=c("Female"="#af8dc3","Male"="#7fbf7b", "Overall"="black"), 
                            labels=c("Female", "Male", "Overall")) +
        scale_y_continuous("Life expectancy (years)\n", breaks=seq(15,90,5), limits=c(15,90)) +
        scale_x_continuous("", breaks=seq(1970, 2025, 10), limits=c(1960,2025)) +
        coord_cartesian(expand = F) +
        theme_life_exp() + 
        theme(legend.position=c(0,1))
      p
    }
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = 0, y = 1))
  })
  
  #plot life expectancy growth
  output$growthPlot <- renderPlotly({
    select_data <- as.numeric(filter(app_data, 
                                     country_name == input$country,
                                     gender == input$gender2)[,3:63])
    
    #apply function to estimate trend
    output <- est_trend(select_data, year_vec=1960:2020, window_val=input$window)
    plot_data <- tibble("Year"=output$`Test years`, 
                        "Trend estimate"=output$`Trend estimate`, 
                        "Upper limit"=output$`Upper limit`, 
                        "Lower limit"=output$`Lower limit`)
    
    #plot trend parameter
    p <- ggplot() +
      geom_ribbon(data=plot_data, aes(ymin=`Trend estimate`, ymax=`Upper limit`, x=Year), fill=alpha("#a6dba0", 0.7)) +
      geom_ribbon(data=plot_data, aes(ymin=`Lower limit`, ymax=`Trend estimate`, x=Year), fill=alpha("#a6dba0", 0.7)) +
      geom_line(data=plot_data, mapping=aes(x=Year, y=`Trend estimate`), colour="#00441b") +
      labs(title=paste("Estimate of life expectancy trend in ", input$country, sep=""), 
           caption = "N. Rennie | Source: World Bank World Development Indicators", x="") +
      scale_y_continuous("Estimate of trend parameter", limits=c(-0.1,2)) +
      scale_x_continuous("", breaks=seq(1970, 2020, 10), limits=c(1960,2020)) +
      coord_cartesian(expand = F) +
      theme_life_exp()
    p
    ggplotly(p)
  })
}