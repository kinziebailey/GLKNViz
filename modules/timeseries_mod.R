# Module for the Timeseries plot ----

# User Interface ----
ts_ui <- function(id){
  
  ns <- NS(id) # creating a namespace
  
  tagList(
    # Parameter Selector
    selectInput(
      inputId = ns("select_param"),
      label = "Select Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$PickListName))),
      selected = ""
    ),
    # Date Range Selector 
    sliderInput(
      inputId = ns("date_range"),
      label = "Select Dates",
      min = min(wqp_data$end_date, na.rm = TRUE),
      max = max(wqp_data$end_date, na.rm = TRUE),
      value = c(min(wqp_data$end_date, na.rm = TRUE),
                max(wqp_data$end_date, na.rm = TRUE))
    ),
    # About Button
    actionButton(
      inputId = ns("about_ts"),
      label = "About Time Series",
      class = "btn btn-info"
    ),
    # Plot 
    plotlyOutput(ns("TimeSeriesPlot"))
  )
}

## Server for timeseries plot ----
ts_server <- function(id, user_data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### About Modal ----
    observeEvent(input$about_ts, {
      showModal(
        modalDialog(title = "About Time Series", 
                    footer = modalButton("Close"),
                    tags$iframe(src = "AboutTimeSeries.html",
                                width = "100%",
                                height = "600px",
                                style = "border:none;"))
      )
    })
    
    ### Reactive for time series ----
    timeseries_data <- reactive({
      
      # required date
      req(input$select_param, input$date_range)
      
      # data wrangling
      series_df1 <- user_data() 
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(series_df1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data exists
      series_df <- series_df1 |> 
        # filtering parameter
        dplyr::filter(PickListName %in% input$select_param) |> 
        # filtering date
        dplyr::filter(end_date >= input$date_range[1],
                      end_date <= input$date_range[2]) |> 
        # filtering depth for averaging
        dplyr::filter(depth >= -2 | is.na(depth)) |>
        # summarise data 
        dplyr::summarise(value = case_when(n() == 1 ~ value[1],
                                           n() == 2 ~ mean(value, na.rm = TRUE),
                                           n() >= 3 ~ median(value, na.rm = TRUE)),
                         .by = c(Park,
                                 MonitoringLocationName,
                                 CharacteristicName,
                                 end_date,
                                 AxisName,
                                 lat,
                                 lon,
                                 value_unit,
                                 PickListName,
                                 AxisName)) |>
        dplyr::arrange(Park,
                       MonitoringLocationName,
                       end_date,
                       CharacteristicName)
    }) 
    
    ### Render Time Series ----
    output$TimeSeriesPlot <- plotly::renderPlotly({
      
      # plotting
      ggtimeseries <- ggplot(data = timeseries_data(),
                             aes(end_date,
                                 value,
                                 color = MonitoringLocationName,
                                 shape = MonitoringLocationName)) +
        geom_point() +
        geom_line() + 
        labs(x = "Date",
             y = unique(timeseries_data()$AxisName)) +
        scale_color_natparks_d("Yellowstone") +
        theme_minimal()
      
      # converting to plotly
      ggplotly(ggtimeseries) |> 
        style(hovertemplate = paste0("<br>Site: ", timeseries_data()$MonitoringLocationName,
                                     "<br>Date: ", timeseries_data()$end_date,
                                     "<br>Value: ", timeseries_data()$value))
    })
    
    # returning data details 
    return(list(timeseries_data = timeseries_data))
  })
}