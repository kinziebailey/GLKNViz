# Module for the depth profile plot ----

# User Interface ----
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
    selectizeInput(
      inputId = ns("select_year"),
      label = "Select Years",
      choices = sort(unique(wqp_data$year)),
      selected = max(wqp_data$year),
      multiple = TRUE,
      options = list(placeholder = "Choose Year", # options for selectize
                     plugins = list("remove_button"))
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
    div(style = "min-height: 300px;
                 height: auto;",
        girafeOutput(ns("DepthProfilePlot"))
    )
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
      
      # continue if data exists 
      profile_df <- user_data() |> 
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
    output$DepthProfilePlot <- ggiraph::renderGirafe({
      
      df <- profile_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
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
        geom_point_interactive(aes(#shape = ResultDetectionConditionText,
                                   tooltip = paste0("Site: ", MonitoringLocationName,
                                                    "\nDepth: ", depth,
                                                    "\nValue: ", value))) +
        labs(x = unique(profile_data()$AxisName),
             y = "Depth (m)",
             color = "Site") +
        facet_grid(row = vars(year),
                   cols = vars(month_name)) +
        scale_color_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements: ",
                       n_data,
                       "\nValues < Quantificantion Limit: ",
                       n_reporting_limit,
                       "\nValues < Detection Limit: ",
                       n_detection_limit))  +
        theme_minimal() +
        theme(plot.title = element_text(size = 8),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 9),
              legend.text = element_text(size = 9),
              legend.title = element_text(size = 11))
      
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
      
      # facet scaling 
      per_row <- 3.5
      height_in <- max(2.5,
                       length(unique(profile_data()$year)) * per_row)
      width_in <- 10.0
      
      # plotting with ggiraph
      girafe(ggobj = ggdepthprofile,
             height_svg = height_in,
             width_svg = width_in,
             opts_sizing(rescale = TRUE,
             width = 1))

    })
    
    # returing data details 
    return(list(depthprofile_data = profile_data))
  })
}