# Module for the depth profile plot ----

dp_ui <- function(id){
  
  ns <- NS(id) # creating a namespace
  
  tagList(
    # Parameter Selector
    selectInput(
      inputId = ns("select_param"),
      label = "Select Parameter",
      choices = c("Choose Parameter" = "",
                  c("Dissolved Oxygen (DO)" = "Dissolved oxygen (DO)",
                    "Dissolved Oxygen Saturation" = "Dissolved oxygen saturation",
                    "pH" = "pH",
                    "Specific Conductance" = "Specific conductance",
                    "Water Temperature" = "Temperature, water")),
      selected = ""
    ),
    # Year Selector
    selectInput(
      inputId = ns("select_year"),
      label = "Select Years",
      choices = sort(unique(wqp_data$year)),
      selected = max(wqp_data$year),
      multiple = TRUE
    ),
    # Thresholds Button
    checkboxInput(
      inputId = ns("thresholds"),
      label = "Thresholds",
      value = FALSE
    ),
    # About Button
    actionButton(
      inputId = ns("about_dp"),
      label = "About Depth Profiles"
    ),
    plotlyOutput(ns("DepthProfilePlot"),
                 height = "auto")
  )
}

## Server for depth profiles ----
dp_server <- function(id, user_data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### About Modal ----
    observeEvent(input$about_dp, {
      showModal(
        modalDialog(title = "About Depth Profiles", 
                    footer = modalButton("Close"),
                    tags$iframe(src = "AboutProfilePlot.html",
                                width = "100%",
                                height = "600px",
                                style = "border:none;")
        )
      )
    })
    
    ### Reactive Plot height ----
    plotht <- reactiveVal(400)
    
    observe({
      req(input$select_year)
      
      nvbox <- length(input$select_year)
      
      plotht(400 + (nvbox - 1) * 120)
    })
    
    ## Reactive for depth profiles ----
    profile_data <- reactive({
      
      # required date
      req(input$select_param, input$select_year)
      
      # data wrangling 
      profile_df1 <- user_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(profile_df1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data exists 
      profile_df <- profile_df1 |> 
        # filtering parameter
        dplyr::filter(CharacteristicName %in% input$select_param) |> 
        # filtering date
        dplyr::filter(year %in% input$select_year) |> 
        # dplyr::mutate(month = lubridate::month(end_date)) |> 
        dplyr::arrange(MonitoringLocationName,
                       end_date,
                       depth)
    })
    
    ## Render Depth Profile Plot ----
    output$DepthProfilePlot <- plotly::renderPlotly({
      
      # Data for Threshold lines
      threshold_df <- profile_data() |>
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
      n_data <- profile_data() |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally()
      
      ## below quantification limit
      n_reporting_limit <- profile_data() |> 
        dplyr::filter(ResultDetectionConditionText == "Present Below Quantification Limit") |> 
        dplyr::tally()
      
      ## below detection limit
      n_detection_limit <- profile_data() |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally()
      
      # plotting
      ggdepthprofile <- ggplot(data = profile_data(),
                               aes(x = value,
                                   y = depth,
                                   color = MonitoringLocationName)) +
        geom_path() + 
        geom_point() +
        labs(x = unique(profile_data()$AxisName),
             y = "Depth (m)",
             color = "Site") +
        facet_grid(row = vars(year),
                   cols = vars(month_name)) +
        scale_color_natparks_d("Yellowstone") +
        theme_minimal()
      
      # adding threshold lines 
      if(input$thresholds){
        ggdepthprofile = ggdepthprofile +
          geom_vline(data = threshold_df,
                     aes(xintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }

      # converting to plotly 
      ggplotly(ggdepthprofile,
               height = plotht()) |> 
        layout(title = list(text = paste0("Total Measurements: ",
                                          n_data,
                                          "\nValues < Quantificantion Limit: ",
                                          n_reporting_limit,
                                          "\nValues < Detection Limit: ",
                                          n_detection_limit),
                            font = list(size = 12),
                            x = 0.05),
               margin = list(t = 65)) |> 
        style(hovertemplate = paste0("<br>Site: ", profile_data()$MonitoringLocationName,
                                     "<br>Date: ", profile_data()$end_date,
                                     "<br>Value: ", profile_data()$value))
    })
    
    # returing data details 
    return(list(depthprofile_data = profile_data))
  })
}