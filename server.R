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

## Parameter ----
observeEvent(input$station, {
  req(input$station != "")
  
  params <- wqp_data |> 
    filter(MonitoringLocationName %in% input$station) |> 
    pull(CharacteristicName) |> 
    unique() |> 
    sort()
  
  updateSelectInput(session,
                    "parameter",
                    choices = c("Choose Parameter" = "",
                                params),
                    selected = "")
})

## Date ----
observeEvent(list(input$station, input$parameter), {
  req(input$station != "", input$parameter != "")
  
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

# Map Reactive ----
observeEvent(input$station, {
  req(input$station)
  
  site <- wqp_data |>
    filter(MonitoringLocationName == input$station)
  
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
    dplyr::summarise(value = mean(value, na.rm = TRUE),
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

# Depth Profile Plot ----

## Reactive for depth profiles ----
profile_data <- reactive({

  # data wrangling 
  profile_df1 <- user_data()
  
  # Warning if no data
  shiny::validate(
    shiny::need(nrow(profile_df1) > 0,
                "No data available for the selected Park / Site / Parameter"))
  
  # continue if data exists 
  profile_df <- profile_df1 |> 
    dplyr::filter(year %in% lubridate::year(end_date)) |> 
    dplyr::mutate(month = lubridate::month(end_date)) |> 
    dplyr::arrange(MonitoringLocationName,
                   month, 
                   value)
})


## Render Depth Profile Plot ----
output$ProfilePlot <- renderPlot({
  
  # plotting
  ggplot(data = profile_data(),
         aes(x = value,
             y = depth,
             color = MonitoringLocationName,
             shape = MonitoringLocationName)) +
    geom_path() + 
    geom_point() +
    facet_wrap(~month) +
    theme_minimal()
})

# Boxplots ----

## Reactive for box plots ----
boxplot_data <- reactive({
  
  # data wrangling 
  boxplot_df1 <- user_data()
  
  # Warning if no data
  shiny::validate(
    shiny::need(nrow(boxplot_df1) > 0,
                "No data available for the selected Park / Site / Parameter"))
  
  # continue if data exists
  boxplot_df <- boxplot_df1 
})

## Render Boxplot ----

output$BoxPlot <- renderPlot({
  
  # plotting 
  ggplot(data = boxplot_data(),
         aes(x = factor(month),
             y = value,
             fill = MonitoringLocationName)) + 
    geom_boxplot() + 
    theme_minimal()
})
}