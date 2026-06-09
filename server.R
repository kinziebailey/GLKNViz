# Libraries ----
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)

# Server ----
server <- function(input, output, session){

# Map ----
output$map <- leaflet::renderLeaflet({
  leaflet::leaflet() |>
    leaflet::addTiles()
    # leaflet::addTiles(group = "Map", # access token issues
    #                   urlTemplate = NPSbasic,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "Imagery",
    #                   urlTemplate = ESRIimagery,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "Topo",
    #                   urlTemplate = ESRItopo,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "NatGeo",
    #                   urlTemplate = ESRINatGeo,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addLayersControl(baseGroups = c("Map",
    #                                          "Imagery",
    #                                          "Topo",
    #                                          "NatGeo"),
    #                           options = layersControlOptions(collapsed = T))

})

# Update UI Inputs ----

## Site ----
observeEvent(input$park, {
  sites <- wqp_data |> 
    filter(Park %in% input$park) |> 
    pull(MonitoringLocationName) |> 
    unique() |> 
    sort()
  
  updateSelectInput(session,
                    "station",
                    choices = sites,
                    selected = sites[1])
})

## Parameter ----
observeEvent(input$station, {
  params <- wqp_data |> 
    filter(MonitoringLocationName %in% input$station) |> 
    pull(CharacteristicName) |> 
    unique() |> 
    sort()
  
  updateSelectInput(session,
                    "parameter",
                    choices = params,
                    selected = params[1])
})

## Date ----
observeEvent(list(input$station, input$parameter), {
  df <- wqp_data |> 
    filter(MonitoringLocationName %in% input$station,
           CharacteristicName == input$parameter)
  
  updateDateRangeInput(session,
                       "date_range",
                       start = min(df$end_date, na.rm = TRUE),
                       end = max(df$end_date, na.rm = TRUE))
})

# Data Reactive ----
user_data <- reactive({
  # required
  req(input$park, input$station, input$date_range)
  
  wqp_data |> 
    filter(Park %in% input$park,
           MonitoringLocationName %in% input$station,
           CharacteristicName == input$parameter,
           end_date >= input$date_range[1],
           end_date <= input$date_range[2])
  
})

# Time Series Plot ----

## Reactive for time series ----
ts_data <- reactive({
  
  # data wrangling
  series_df1 <- user_data() 
  
  # Warning if no data
  shiny::validate(
    shiny::need(nrow(series_df1) > 0,
                "No data available for the selected Park / Site / Parameter"))
  
  # continue if data exists
  series_df <- series_df1 |> 
    summarise(value = mean(value, na.rm = TRUE),
              .by = c("MonitoringLocationName",
                      "end_date"))
}) 

## Render Time Series ----
output$TimeSeriesPlot <- renderPlot({
  
  # plotting
  ggplot(data = ts_data(),
         aes(end_date,
             value,
             color = MonitoringLocationName,
             shape = MonitoringLocationName)) +
    geom_point() +
    geom_line() + 
    theme_minimal()
})

}