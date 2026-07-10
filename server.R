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
      dplyr::filter(Park %in% input$park) |> 
      dplyr::pull(MonitoringLocationName) |> 
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
      dplyr::filter(Park %in% input$park,
                    MonitoringLocationName %in% input$station) 
  })
  
  # Map Reactive ----
  observeEvent(input$station, {
    req(input$station)
    
    # selecting last station to zoom to
    last_station <- tail(input$station, 1)
    
    site <- wqp_data |>
      dplyr::filter(MonitoringLocationName == last_station) |> 
      dplyr::distinct()
    
    leafletProxy("map") |>
      setView(lng = site$lon[1],
              lat = site$lat[1],
              zoom = 15) |>
      clearPopups() |>
      addPopups(lng = site$lon[1],
                lat = site$lat[1],
                popup = site$MonitoringLocationName[1])
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
  
  # Parent Servers ----
  ## Time Series mod activation ----
  ## Plot
  ts <- ts_server("ts", user_data)
  
  ## Tables 
  details_server("details_ts", 
                 data_from = ts$timeseries_data)
  
  
  ## Depth Profile mod activation ----
  ## Plot
  dp <- dp_server("dp", user_data)
  
  ## Tables 
  details_server("details_dp", 
                 data_from = dp$depthprofile_data)
  
  ## Boxplot mod activation ----
  ## Plot
  bp <- bp_server("bp", user_data)
  
  ## Tables
  details_server("details_bp",
                 data_from = bp$boxplot_data)
  
  ## Correlation mod activation ----
  ## Plot
  cp <- cp_server("cp", user_data)
  
  ## Tables
  details_server("details_cp",
                 data_from = cp$correlation_long)
  
}