# Module for the Correlation plot ----

# User Interface ----
cp_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    # Parameter 1 Selector
    selectInput(
      inputId = ns("select_param1"),
      label = "Select x-axis Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$PickListName))),
      selected = ""
    ),
    # Parameter 2 Selector 
    selectInput(
      inputId = ns("select_param2"),
      label = "Select y-axis Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$PickListName))),
      selected = ""
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
    # About Button
    actionButton(
      inputId = ns("about_cp"),
      label = "About Correlation Plot"
    ),
    div(style = "min-height: 300px;
                 height: auto;",
    girafeOutput(ns("CorrelationPlot"))
    )
  )
}

## Server for correlation plot ----
cp_server <- function(id, user_data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### About Modal ----
    observeEvent(input$about_cp, {
      showModal(
        modalDialog(title = "About Correlation Plot",
                    footer = modalButton("Close"),
                    tags$iframe(src = "AboutCorrelations.html",
                                width = "100%",
                                height = "600px",
                                style = "border:none;"))
      )
    })
    
    ### Reactive for correlation plots ----
    # data table df
    correlation_long <- reactive({
      
      # required data
      req(input$select_param1, input$select_param2)

      # continue if data exists
      correlation_long <- user_data() |> 
        dplyr::filter(PickListName %in% c(input$select_param1,
                                          input$select_param2)) |> 
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
                                 ResultDetectionConditionText))
    })
    
    # plot df
    correlation_data <- reactive({
      
      correlation_df <- correlation_long() |> 
        dplyr::select(Park,
                      end_date,
                      MonitoringLocationName,
                      PickListName,
                      value) |> 
        tidyr::pivot_wider(names_from = PickListName,
                           values_from = value)
    })
    
    ### Reactive for Regressions ----
    regression_type <- reactive({
      
      df <- correlation_data()
      
      # to be able to select parameters in data
      x <- input$select_param1
      y <- input$select_param2
      
      # building regressions
      ## no regression, start here
      if(input$regression_selection == "none") return(NULL)
      
      # creating regressions for each option
      df_reg <- df |> 
        # for multiple sites
        dplyr::group_by(MonitoringLocationName) |> 
        # removing NA
        dplyr::filter(!is.na(.data[[x]]),
                      !is.na(.data[[y]])) |> 
        # if regression selection, predict regression output
        dplyr::mutate(fit = {if(input$regression_selection == "linear"){
          predict(lm(.data[[y]] ~ .data[[x]]))
        } else if(input$regression_selection == "loess"){
          predict(loess(.data[[y]] ~ .data[[x]]))
        } else if(input$regression_selection == "poly2"){
          predict(lm(.data[[y]] ~ poly(.data[[x]], 2)))
        } else{NA_real_}
    }) |> 
        dplyr::ungroup()
      
      # add regression line 
      geom_line(data = df_reg, 
                aes(x = .data[[x]],
                    y = fit,
                    color = MonitoringLocationName),
                inherit.aes = FALSE) #  dont use "global" aes
    })
    
    ## Render Correlation Plot ----
    output$CorrelationPlot <- ggiraph::renderGirafe({

      df1 <- correlation_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(df1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # Need both parameters 
      ## x-axis
      shiny::validate(
        shiny::need(input$select_param1 %in% names(df1),
                    "No data available for the x-axis parameter for the selected Park / Site.")
      )
      
      ## y-axis
      shiny::validate(
        shiny::need(input$select_param2 %in% names(df1),
                    "No data available for the y-axis parameter for the selected Park / Site.")
      )
      
      ## Need paired data
      shiny::validate(
        shiny::need(sum(!is.na(df1[[input$select_param1]]) &
                          !is.na(df1[[input$select_param2]])) > 0,
                    "Not enough paired data tp produce a correlation plot.")
      )
      
      # Calling datasets 
      correlation_longdf <- correlation_long()
      correlation_df <- correlation_data()
      
      # Reporting Limits
      ## number of values plotted
      n_data <- correlation_long() |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally()
      
      ## below quantification limit
      n_reporting_limit <- correlation_long() |> 
        dplyr::filter(ResultDetectionConditionText == "Present Below Quantification Limit") |> 
        dplyr::tally()
      
      ## below detection limit
      n_detection_limit <- correlation_long() |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally()

      # Axis Labels
      x_axis <- unique(correlation_longdf$AxisName[correlation_longdf$PickListName == input$select_param1])
      y_axis <- unique(correlation_longdf$AxisName[correlation_longdf$PickListName == input$select_param2])
      
      # plotting 
      ggcorrelation <- ggplot(data = correlation_df,
                              aes(x = .data[[input$select_param1]],
                                  y = .data[[input$select_param2]],
                                  color = MonitoringLocationName)) +
        geom_point_interactive(aes(tooltip = paste0("Site: ", MonitoringLocationName,
                                                    "\nDate: ", end_date,
                                                    "\n", input$select_param1, ": ", .data[[input$select_param1]],
                                                    "\n", input$select_param2, ": ", .data[[input$select_param2]]))) + 
        labs(x = x_axis,
             y = y_axis,
             color = "Site") +
        regression_type() +
        scale_color_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements: ",
                       n_data,
                       "\nValues < Quantificantion Limit: ",
                       n_reporting_limit,
                       "\nValues < Detection Limit: ",
                       n_detection_limit))  +
        theme_minimal() +
        theme(plot.title = element_text(size = 5),
              axis.title = element_text(size = 8),
              axis.text = element_text(size = 6),
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8))
      
      girafe(ggobj = ggcorrelation,
             height_svg = 5,
             width_svg = 6)
      
    })
    
    # returning data details 
    return(list(correlation_data = correlation_data, # for plot
                correlation_long = correlation_long)) # for table 
  })
}