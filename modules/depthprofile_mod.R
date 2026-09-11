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
    # Depth Range Selector 
    sliderInput(
      inputId = ns("depth_range"),
      label = "Select Depths",
      min = min(wqp_data$depth, na.rm = TRUE),
      max = max(wqp_data$depth, na.rm = TRUE),
      value = c(min(wqp_data$depth, na.rm = TRUE),
                max(wqp_data$depth, na.rm = TRUE))
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
    # Download Button
    downloadButton(ns("download_figure"),
                   "Download Figure"),
    # Plot
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
                                style = "border:none;"))
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
      req(input$select_param, input$select_year, input$depth_range)
      
      # continue if data exists 
      user_data() |> 
        # filtering parameter
        dplyr::filter(CharacteristicName %in% input$select_param) |> 
        # filtering date
        dplyr::filter(year %in% input$select_year) |> 
        # filtering date
        dplyr::filter(depth >= input$depth_range[1],
                      depth <= input$depth_range[2]) |> 
        dplyr::arrange(MonitoringLocationName,
                       end_date,
                       depth)
    })
    
    ## Render Depth Profile Plot ----
    output$DepthProfilePlot <- ggiraph::renderGirafe({
      
      profile_df <- profile_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(profile_df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # Data for Threshold lines
      threshold_df <- profile_df |>
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
      n_data <- profile_df |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## below quantification limit
      n_below_quant <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "< Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## above quantification limit
      n_above_quant <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "> Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not detected
      n_detection_limit <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not reported
      n_report_limit <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Reported") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      # filtering out NA values for plotting
      profile_df <- profile_df |> 
        dplyr::filter(!is.na(value))
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(profile_df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # plotting
      ggdepthprofile <- ggplot(data = profile_df,
                               aes(x = value,
                                   y = depth,
                                   color = MonitoringLocationName,
                                   shape = MonitoringLocationName,
                                   group = end_date)) +
        geom_path() + 
        geom_point_interactive(aes(tooltip = paste0("Site: ", MonitoringLocationName,
                                                    "\nDate: ", end_date,
                                                    "\nDepth: ", depth,
                                                    "\nValue: ", value,
                                                    "\n", ResultDetectionConditionText))) +
        labs(x = unique(profile_df$AxisName),
             y = "Depth (m)",
             color = "Site",
             shape = "Site",
             alt = "A depth profile figure for parameter of intrest.") +
        facet_grid(row = vars(year),
                   cols = vars(month_name)) +
        scale_color_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements Plotted: ", n_data,
                       "\nValues < Quantification Limit: ", n_below_quant,
                       "\nValues > Quantification Limit: ", n_above_quant,
                       "\nValues < Detection Limit: ", n_detection_limit,
                       "\nValues Not Reported: ", n_report_limit))  +
        theme_minimal() +
        theme(plot.title = element_text(size = 8),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 9),
              legend.text = element_text(size = 9),
              legend.title = element_text(size = 11))
      
      # adding threshold lines 
      if(input$thresholds){
        ggdepthprofile <- ggdepthprofile +
          geom_vline(data = threshold_df,
                     aes(xintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }
      
      # facet scaling 
      per_row <- 3.5
      height_in <- max(2.5, length(unique(profile_df$year)) * per_row)
      width_in <- 10.0
      
      # plotting with ggiraph
      girafe(ggobj = ggdepthprofile,
             height_svg = height_in,
             width_svg = width_in,
             opts_sizing(rescale = TRUE,
                         width = 1),
             options = list(opts_toolbar(hidden = "saveaspng")))

    })
    
    ## Static plot for download ----
    static_profile <- reactive({
      
      profile_df <- profile_data()
      
      req(nrow(profile_df) > 0)
      
      # Data for Threshold lines
      threshold_df <- profile_df |>
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
      n_data <- profile_df |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## below quantification limit
      n_below_quant <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "< Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## above quantification limit
      n_above_quant <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "> Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not detected
      n_detection_limit <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not reported
      n_report_limit <- profile_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Reported") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      # plotting
      p <- ggplot(data = profile_df,
                  aes(x = value,
                      y = depth,
                      color = MonitoringLocationName,
                      shape = MonitoringLocationName,
                      group = end_date)) +
        geom_path() + 
        geom_point() +
        labs(x = unique(profile_df$AxisName),
             y = "Depth (m)",
             color = "Site",
             shape = "Site",
             alt = "A depth profile figure for parameter of intrest.") +
        facet_grid(row = vars(year),
                   cols = vars(month_name)) +
        scale_color_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements Plotted: ", n_data,
                       "\nValues < Quantification Limit: ", n_below_quant,
                       "\nValues > Quantification Limit: ", n_above_quant,
                       "\nValues < Detection Limit: ", n_detection_limit,
                       "\nValues Not Reported: ", n_report_limit))  +
        theme_minimal() +
        theme(plot.title = element_text(size = 8),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 9),
              legend.text = element_text(size = 9),
              legend.title = element_text(size = 11))
      
      # adding threshold lines 
      if(input$thresholds){
        p <- p +
          geom_vline(data = threshold_df,
                     aes(xintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }
      
      p
      
    })
    
    ### Download handler ----
    output$download_figure <- downloadHandler(filename = function() {
      
      # file name 
      paste0("Profile_", gsub("\\s+", "_", input$select_param), "_",
             format(Sys.Date(), "%Y"),
             ".png")
    },
    
    # saving
    content = function(file) {
      
      p <- static_profile()
      
      ggsave(filename = file,
             plot = p,
             width = 6, 
             height = 3, 
             units = "in", 
             dpi = 300, 
             background = "white")
    })
    
    # returning data details 
    return(list(depthprofile_data = profile_data))
  })
}