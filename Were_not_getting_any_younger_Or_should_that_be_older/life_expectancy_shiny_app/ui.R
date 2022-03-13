library(shiny)
library(shinydashboard)
library(tidyverse)
library(extrafont)
library(plotly)
library(LifeExpectancyPkg) 

#read in data
app_data <- readRDS("data/app_data.rds")

#### UI ####
ui <- dashboardPage(
  
  #title of shiny ap
  dashboardHeader(title = "Life Expectancy"),
  
  #add elements to sidebar
  dashboardSidebar(
    #choose country
    selectInput("country", "Country:",
                sort(unique(app_data$country_name))), 
    
    #choose window length
    sliderInput("window", "Window length:",
                min = 5, max = 15,
                value = 10)
  ),
  
  #display plots
  dashboardBody(
    #define css display 
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ), 
    fluidRow(
      #life expectancy plot
      box(title = "Life Expectancy", height=600, status = "primary", width = 6, 
          checkboxGroupInput("gender", "Select:", 
                             c("Female" = "Female", "Male" = "Male", "Overall" = "Overall"), 
                             inline = T, selected=c("Female","Male","Overall")), 
          plotlyOutput("lifePlot")),
      #life expectancy growth plot
      box(title = "Life Expectancy Growth", height=600, status = "primary", width = 6, 
          radioButtons("gender2", "Select:", 
                       c("Female" = "Female", "Male" = "Male", "Overall" = "Overall"), 
                       selected=c("Overall"),inline = TRUE), 
          plotlyOutput("growthPlot")),
    )
    
  )
)



