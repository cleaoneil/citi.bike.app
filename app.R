# Intro to Data Science - HW 11
# Copyright 2026, Christopher Dunham.
# Please do not post online.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Follow the instructions in the PDF file accompanying this script to complete the assignment.

library(shiny)
library(shinydashboard)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(ggmap)
library(dplyr)
library(DT)

# get station status from API
station_status <- (getURL('https://gbfs.citibikenyc.com/gbfs/en/station_status.json') |>
                    fromJSON())$data$stations
#remove duplication
station_status <- unique(station_status)

# do not include stations that are not in service
#station_status = station_status |> filter(station_status != 'out_of_service')

# get station information from API
station_information <- (getURL('https://gbfs.citibikenyc.com/gbfs/en/station_information.json') |>
                         fromJSON())$data$stations
station_information <- unique(station_information)

# Define UI for application that explores the Citi Bike program in New York City, NY, USA
ui <- dashboardPage(
  dashboardHeader(title = paste('Hot Spots for Disabled Bikes')),
  dashboardSidebar(
  HTML('This application is used by bike maintenance personnel, who with limited time and resources, want to keep as many bikes operating as possible. It shows Citi Bike stations in New York City and how many bikes are disabled at each one. The filtering function can be used to view all stations or only stations with disabled bikes. The larger the dot, the more disabled bikes at said station.'),
  selectInput('station_filter', 'Show:', c('All stations', 'Stations with disabled bikes'))
  ),
  dashboardBody(
    fluidRow(box(textOutput('total_bikes_disabled'))), # fill this in with appropriate function arguments then un-comment
    fluidRow(box(plotOutput('bike_map', width = '100%'))),
    br(),
    fluidRow(box(DTOutput('bike_table', width = '100%'))),
    ) # end dashboard body
  ) # end dashboard page

# Define server logic
server <- function(input, output) {
  
  output$total_bikes_disabled <- renderText({
    paste('Total Disabled Bikes', sum(bike_data()$num_bikes_disabled))
  })
  bike_data <- reactive({
    
    # use station information and station status that were retrieved at start of app
    # to build a data set used for the map and table

     data <- station_information |> 
        inner_join(station_status, by = 'station_id') |> 
        select(
          # variables from station_information
          station_name = name, lat, lon, capacity,
          # variables from station_status
          num_bikes_available, num_bikes_disabled, num_docks_available, num_docks_disabled
        )
     if(input$station_filter == 'Stations with disabled bikes'){
       data <- data |> filter(num_bikes_disabled > 0)
  }
   data 
})

  output$bike_table <- renderDT(
    bike_data() |> 
      select(-lat, -lon) |> 
      datatable()
    )
  
  output$bike_map <- renderPlot({
    
    # bounding box for the map
    bb = c(left = min(bike_data()$lon),
           bottom = min(bike_data()$lat),
           right = max(bike_data()$lon),
           top = max(bike_data()$lat))
    
    # retrieve base bap
    print(tempdir())
    
    register_stadiamaps('5eafc2f2-b969-4659-b603-dadc581c9ed4') 
    
    ## Create a ggmap object with the tile map background from Stamen
    base_map <- get_stadiamap(bbox = bb, zoom = 10, maptype = 'stamen_toner_lite')
   
    # map output
    ggmap(base_map) +
      geom_point(data = bike_data(),
                 alpha = 0.5,
                 aes(x = lon,
                     y = lat,
                     color = num_bikes_available,
                     size = num_bikes_disabled)) +
      ggtitle(paste('Citi Bike Map', input$station_filter))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
