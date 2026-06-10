# Libraries ----
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)

# Server ----
server <- function(input, output, session){

# Update UI Inputs ----

## Site ----
observeEvent(input$park, {
  req(input$park != "")
  
  sites <- wqp_data |> 
    filter(Park %in% input$park) |> 
    pull(MonitoringLocationName) |> 
    unique() |> 
    sort()
  
  updateSelectInput(session,
                    "station",
                    choices = c("Pick site" = "",
                                sites),
                    selected = "")
})

# Data Reactive ----
user_data <- reactive({
  # required
  req(input$park, input$station)
  
  wqp_data |> 
    filter(Park %in% input$park,
           MonitoringLocationName %in% input$station)
           # ,
           # CharacteristicName == input$parameter)
})

## Time Series mod activation ----
  ts_server("ts", user_data)

## Depth Profile mod activation ----
  dp_server("dp", user_data)

## Boxplot mod activation ----
  bp_server("bp", user_data)

## Correlation mod activation ----
cp_server("cp", user_data)

# Map Reactive ----
observeEvent(input$station, {
  req(input$station)
  
  site <- wqp_data |>
    filter(MonitoringLocationName %in% input$station)
  
  leafletProxy("map") |>
    setView(
      lng = site$lon[1],
      lat = site$lat[1],
      zoom = 12
    ) |>
    clearPopups() |>
    addPopups(
      lng = site$lon[1],
      lat = site$lat[1],
      popup = site$MonitoringLocationName[1]
    )
})

# Map ----
output$map <- leaflet::renderLeaflet({
  
  ## Initial map ----
  leaflet(wqp_data) |>
    addTiles() |> 
    addCircleMarkers(lng = ~lon,
                     lat = ~lat,
                     radius = 5,
                     color = "blue") |> 
    fitBounds(lng1 = min(wqp_data$lon, na.rm = TRUE),
              lat1 = min(wqp_data$lat, na.rm = TRUE),
              lng2 = max(wqp_data$lon, na.rm = TRUE),
              lat2 = max(wqp_data$lat, na.rm = TRUE))
})

}