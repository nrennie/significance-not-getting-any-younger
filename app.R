library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(extrafont)
library(countrycode)
library(patchwork)
library(ggplot2)
library(plotly)

world_all <- readRDS("data/world_all.rds")

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "Life Expectancy"),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(
    selectInput("country", "Country:",
                sort(unique(world_all$country_code)))
    ),
  
    # Main panel for displaying outputs ----
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    fluidRow(
      #visualise network
      box(title = "Life Expectancy", height=600, status = "primary", width = 6, checkboxGroupInput("gender", "Select:", c("Female" = "Female", "Male" = "Male", "Overall" = "Overall"), inline = T, selected=c("Female","Male","Overall")), plotlyOutput("lifePlot")),
      box(title = "Life Expectancy Growth", height=600, status = "primary", width = 6, 
          radioButtons("gender2", "Select:", c("Female" = "Female", "Male" = "Male", "Overall" = "Overall"), selected=c("Overall"),inline = TRUE), plotlyOutput("growthPlot")),
    )
      
    )
)

# Define server logic required to draw plots ----
server <- function(input, output) {
  output$lifePlot <- renderPlotly({
    select_data <- filter(world_all, country_code == input$country & gender %in% input$gender)
    long_data <- pivot_longer(select_data, cols=3:63, names_to="year", values_to="life_exp")
    x1 <- c(NA, 2022)[("Female" %in% input$gender) + 1]
    y1 <- c(NA, as.numeric(filter(select_data, gender == "Female")[tail(which(!is.na(filter(select_data, gender == "Female"))),1)]))[("Female" %in% input$gender) + 1]
    x2 <- c(NA, 2022)[("Male" %in% input$gender) + 1]
    y2 <- c(NA, as.numeric(filter(select_data, gender == "Male")[tail(which(!is.na(filter(select_data, gender == "Male"))),1)]))[("Male" %in% input$gender) + 1]
    x3 <- c(NA, 2022)[("Overall" %in% input$gender) + 1]
    y3 <- c(NA, as.numeric(filter(select_data, gender == "Overall")[tail(which(!is.na(filter(select_data, gender == "Overall"))),1)]))[("Overall" %in% input$gender) + 1]
    if (nrow(select_data) < 1){
      p <- ggplot() +
        labs(title="Please select at least one of: Female, Male, Overall") +
        theme_light() +
        theme(panel.background = element_rect(fill = "white", colour="white"),
              plot.background = element_rect(fill = "white", colour="white"),
              legend.background = element_rect(fill = "transparent"),
              plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
              plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
              plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
              legend.position="none",
              legend.key = element_rect(colour = "white", fill="white"),
              plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
              legend.spacing.x = unit(0.5,"cm"),
              legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
              legend.text = element_text(colour="black", size=10, hjust = 0),
              axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
              axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
              axis.ticks=element_blank(),
              panel.grid.minor = element_blank()
        )
    } else{
      p <- ggplot() +
        geom_line(data=long_data, mapping=aes(x=as.numeric(year), y=life_exp, col=gender)) +
        labs(title=paste("Life expectancy in ", input$country, sep=""), caption = "N. Rennie | Source: World Bank’s World Development Indicators", x="") +
        scale_colour_manual("", values=c("Female"="#af8dc3","Male"="#7fbf7b", "Overall"="black"), labels=c("Female", "Male", "Overall")) +
        scale_y_continuous("Life expectancy (years)\n", breaks=seq(15,90,5), limits=c(15,90)) +
        scale_x_continuous("", breaks=seq(1970, 2025, 10), limits=c(1960,2025)) +
        geom_text(data=data.frame(x=x1, y=y1, label="Female"), aes(x=x, y=y, label=label, colour="Female"), size=3, hjust=1, family="Times New Roman") +
        geom_text(data=data.frame(x=x2, y=y2, label="Male"), aes(x=x, y=y, label=label, colour="Male"), size=3, hjust=1, family="Times New Roman") +
        geom_text(data=data.frame(x=x3, y=y3, label="Overall"), aes(x=x, y=y, label=label, colour="Overall"), size=3, hjust=1, family="Times New Roman") +
        coord_cartesian(expand = F) +
        theme_light() +
        theme(panel.background = element_rect(fill = "white", colour="white"),
              plot.background = element_rect(fill = "white", colour="white"),
              legend.background = element_rect(fill = "transparent"),
              plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
              plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
              plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
              legend.position="none",
              legend.key = element_rect(colour = "white", fill="white"),
              plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
              legend.spacing.x = unit(0.5,"cm"),
              legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
              legend.text = element_text(colour="black", size=10, hjust = 0),
              axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
              axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
              axis.ticks=element_blank(),
              panel.grid.minor = element_blank()
        )
      p
    }
    ggplotly(p)
  })
  
  output$growthPlot <- renderPlotly({
    select_data <- filter(world_all, country_code == input$country & gender == input$gender2)
    year <- 1960:2020
    test_years <- 1969:2020
    est_beta <- numeric(length(test_years))
    upper_beta <- numeric(length(test_years))
    lower_beta <- numeric(length(test_years))
    for (i in 1:length(test_years)){
      x <- year[(which(year == test_years[i])-9):which(year == test_years[i])]
      y <- as.numeric(select_data[((which(year == test_years[i])-9)+2):(which(year == test_years[i])+2)])
      mod <- lm(y~x)
      est_beta[i] <- mod$coefficients[2]
      confint(mod)
      upper_beta[i] <- confint(mod)[2,2]
      lower_beta[i] <- confint(mod)[2,1]
    }
    plot_data <- tibble(year=1969:2020, est_beta=est_beta, upper_beta=upper_beta, lower_beta=lower_beta)
    p <- ggplot() +
      geom_ribbon(data=plot_data, aes(ymin=est_beta, ymax=upper_beta, x=year), fill=alpha("#a6dba0", 0.7)) +
      geom_ribbon(data=plot_data, aes(ymin=lower_beta, ymax=est_beta, x=year), fill=alpha("#a6dba0", 0.7)) +
      geom_line(data=plot_data, mapping=aes(x=year, y=est_beta), colour="#00441b") +
      labs(title="Estimate of life expectancy trend in the United Kingdom", caption = "N. Rennie | Source: World Bank’s World Development Indicators", x="") +
      scale_y_continuous("Estimate of trend parameter", limits=c(-0.1,2)) +
      scale_x_continuous("", breaks=seq(1970, 2020, 10), limits=c(1960,2020)) +
      coord_cartesian(expand = F) +
      theme_light() +
      theme(panel.background = element_rect(fill = "white", colour="white"),
            plot.background = element_rect(fill = "white", colour="white"),
            legend.background = element_rect(fill = "white"),
            plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
            plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
            plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
            legend.position="top",
            legend.key = element_rect(colour = "black", fill="black"),
            plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
            legend.spacing.x = unit(0.5,"cm"),
            legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
            legend.text = element_text(colour="black", size=10, hjust = 0),
            axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
            axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
            axis.ticks=element_blank(),
            panel.grid.minor = element_blank()
      )
    p
    ggplotly(p)
  })
}

shinyApp(ui, server)
