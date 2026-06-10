# Module for the Timeseries plot ----

ts_ui <- function(id){
  
  ns <- NS(id) # creating a namespace
  
  tagList(
    # Parameter Selector
    selectInput(
      inputId = ns("select_param"),
      label = "Select Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$CharacteristicName))),
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
    plotOutput(ns("TimeSeriesPlot"))
  )
}

## Server for time series ----
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
                               style = "border:none;")
                   )
                  )
                 })
    
    ### Reactive for time series ----
    timeseries_data <- reactive({
      
      # require date
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
        dplyr::filter(CharacteristicName %in% input$select_param) |> 
        # filtering date
        dplyr::filter(end_date >= input$date_range[1],
                      end_date <= input$date_range[2]) |> 
        # avg data 
        dplyr::summarise(value = mean(value, na.rm = TRUE),
                         .by = c("MonitoringLocationName",
                                 "end_date"))
    }) 
    
    ### Render Time Series ----
    output$TimeSeriesPlot <- renderPlot({
      
      # plotting
      ggplot(data = timeseries_data(),
             aes(end_date,
                 value,
                 color = MonitoringLocationName,
                 shape = MonitoringLocationName)) +
        geom_point() +
        geom_line() + 
        theme_minimal()
    })
  })
}