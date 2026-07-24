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
    # Add Regression
    radioButtons(
      inputId = ns("regression_selection"),
      label = "Regression Type:",
      choices = list(
        "None" = "none",
        "Linear" = "linear",
        "LOESS" = "loess",
        "Polynomial (2nd degree)" = "poly2"
      ),
      inline = TRUE,
      selected = "none" 
    ),
    # Thresholds Button
    checkboxInput(
      inputId = ns("thresholds"),
      label = "Thresholds",
      value = FALSE
    ),
    # About Button
    actionButton(
      inputId = ns("about_ts"),
      label = "About Time Series"
    ),
    # Plot 
    div(style = "min-height: 250px;
                 height: auto;",
        girafeOutput(ns("TimeSeriesPlot"))
    ),
    # saving option
    # div(style = "margin-top: 8px;",
    #     downloadButton(ns("download_png"),
    #                    "Download PNG"))
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
      
      # required data
      req(input$select_param, input$date_range)
      
      # continue if data exists
      series_df <- user_data()  |> 
        # filtering parameter
        dplyr::filter(PickListName %in% input$select_param) |> 
        # filtering date
        dplyr::filter(end_date >= input$date_range[1],
                      end_date <= input$date_range[2]) |> 
        # filtering depth for averaging
        dplyr::filter(depth >= -2 | is.na(depth)) |>
        # summarise data 
        dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
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
                                 AxisName,
                                 LowerPoint,
                                 UpperPoint,
                                 ResultDetectionConditionText)) |>
        dplyr::arrange(Park,
                       MonitoringLocationName,
                       end_date,
                       CharacteristicName)
    }) 
    
    ### Reactive for Regressions ----
    regression_type <- reactive({

      df <- timeseries_data()

      # building regressions:
      ## no regression, start here 
      if(input$regression_selection == "none") return(NULL)

      # creating regressions for each option
      df_reg <- df |>
        # converting date to numeric for loess
        dplyr::mutate(end_date_num = as.numeric(end_date)) |> 
        # for multiple sites
        dplyr::group_by(MonitoringLocationName) |>
        # removing NA
        dplyr::filter(!is.na(value),
                      !is.na(end_date_num)) |>
        # ordering data 
        dplyr::arrange(end_date_num,
                       .by_group = TRUE) |> 
        # if regression selection, predict regression output
        dplyr::mutate(fit = {if(input$regression_selection == "linear"){
          predict(lm(value ~ end_date_num))
        } else if(input$regression_selection == "loess"){
          predict(loess(value ~ end_date_num))
        } else if(input$regression_selection == "poly2"){
          predict(lm(value ~ poly(end_date_num, 2)))
        } else{NA_real_}
        }) |>
        dplyr::ungroup()

      # add regression line
      geom_line(data = df_reg,
                aes(x = end_date,
                    y = fit,
                    color = MonitoringLocationName))
    })
    
    ### Render Time Series Plot ----
    output$TimeSeriesPlot <- ggiraph::renderGirafe({
      
      df <- timeseries_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # Data for Threshold lines
      threshold_df <- timeseries_data() |>
        dplyr::select(UpperPoint,
                      LowerPoint) |> 
        dplyr::distinct() |> 
        tidyr::pivot_longer(cols = everything(),
                            names_to = "Threshold",
                            values_to = "thresh") |> 
        dplyr::mutate(Threshold = recode(Threshold,
                                         UpperPoint = "Upper Threshold",
                                         LowerPoint = "Lower Threshold"))
      
      # Reporting Limits 
      ## number of values plotted
      n_data <- timeseries_data() |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally()
      
      ## below quantification limit
      n_reporting_limit <- timeseries_data() |> 
        dplyr::filter(ResultDetectionConditionText == "Present Below Quantification Limit") |> 
        dplyr::tally()
      
      ## below detection limit
      n_detection_limit <- timeseries_data() |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally()
      
      # plotting
      ggtimeseries <- ggplot(data = timeseries_data(),
                             aes(end_date,
                                 value,
                                 color = MonitoringLocationName)) +
        geom_point_interactive(aes(tooltip = paste0("Site: ", MonitoringLocationName,
                                                    "\nDate: ", end_date,
                                                    "\nValue: ", value))) +
        geom_line() + 
        labs(x = "Date",
             y = unique(timeseries_data()$AxisName),
             color = "Site") +
        regression_type() +
        ggtitle(paste0("Total Measurements: ",
                       n_data,
                       "\nValues < Quantificantion Limit: ",
                       n_reporting_limit,
                       "\nValues < Detection Limit: ",
                       n_detection_limit)) +
        scale_color_natparks_d("Yellowstone") +
        theme_minimal() +
        theme(plot.title = element_text(size = 5),
              axis.title = element_text(size = 8),
              axis.text = element_text(size = 6),
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8))
      
      # adding threshold lines 
      if(input$thresholds){
        ggtimeseries = ggtimeseries +
          geom_hline(data = threshold_df,
                     aes(yintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }
      
      girafe(ggobj = ggtimeseries,
             height_svg = 3,
             width_svg = 6,
             options = list(opts_toolbar(saveaspng = FALSE)))
      
    })
    
    # output$download_png <- downloadHandler(filename = function(){
    #   paste0("timeseries_",
    #          Sys.Date(),
    #          ".png")
    # },
    # content = function(file){
    #   ggplot2::ggsave(file,
    #                   ggtimeseries,
    #                   dpi = 600)
    # })
    
    # returning data details 
    return(list(timeseries_data = timeseries_data))
  })
}