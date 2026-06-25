# Module for the Correlation plot ----

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
    plotlyOutput(ns("CorrelationPlot"))
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
      
      req(input$select_param1, input$select_param2)
      
      # data wrangling 
      correlation_long1 <- user_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(correlation_long1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data exists
      correlation_long <- correlation_long1 |> 
        dplyr::filter(PickListName %in% c(input$select_param1,
                                          input$select_param2)) |> 
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
        # averaging if multiple values per sample period
        dplyr::summarise(value = mean(value, na.rm = TRUE),
                         .by = c(Park,
                                 end_date,
                                 MonitoringLocationName, 
                                 CharacteristicName,
                                 AxisName,
                                 PickListName,
                                 lat,
                                 lon,
                                 value_unit))
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
      
      # building hover text 
      correlation_df$hover <- paste0("<br>Site: ", correlation_df$MonitoringLocationName,
                                     "<br>Date: ", correlation_df$end_date,
                                     "<br>", input$select_param1, ": ", correlation_df[[input$select_param1]],
                                     "<br>", input$select_param2, ": ", correlation_df[[input$select_param2]])
      
      correlation_df
    })
    
    ### Reactive for Regressions ----
    regression_type <- reactive({
      
      df <- correlation_data()
      
      x <- input$select_param1
      y <- input$select_param2
      
      # building regressions
      ## none
      if(input$regression_selection == "none") return(NULL)
      
      # adding multiple site options 
      df_reg <- df |> 
        dplyr::group_by(MonitoringLocationName) |> 
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
                inherit.aes = FALSE)
    })
    
    ## Render Correlation Plot ----
    
    output$CorrelationPlot <- plotly::renderPlotly({

      # Calling datasets 
      correlation_longdf <- correlation_long()
      correlation_df <- correlation_data()

      # Axis Labels
      x_axis <- unique(correlation_longdf$AxisName[correlation_longdf$PickListName == input$select_param1])
      y_axis <- unique(correlation_longdf$AxisName[correlation_longdf$PickListName == input$select_param2])
      
      # plotting 
      ggcorrelation <- ggplot(data = correlation_df,
                              aes(x = .data[[input$select_param1]],
                                  y = .data[[input$select_param2]],
                                  color = MonitoringLocationName,
                                  text = hover)) +
        geom_point() + 
        labs(x = x_axis,
             y = y_axis,
             color = "Site") +
        regression_type() +
        # regression_equation() +
        scale_color_natparks_d("Yellowstone") +
        # geom_smooth(method = lm,
        # se = FALSE) +
        # stat_poly_eq(use_label(c("eq", "R2"))) +
        theme_minimal()
      
      ggplotly(ggcorrelation,
               tooltip = "text") #|> 
      # style(hovertemplate = paste0("<br>Site: ", correlation_data()$MonitoringLocationName,
      #                              "<br>Date: ", correlation_data()$end_date,
      #                              "<br>X-axis Value: ", .data[[input$select_param1]],
      #                              "<br>Y-axis Value: ", .data[[input$select_param2]]))
    })
    
    # returning data details 
    return(list(correlation_data = correlation_data, # for plot
                correlation_long = correlation_long)) # for table 
  })
}