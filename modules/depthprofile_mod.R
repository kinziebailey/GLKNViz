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
    # checkboxInput(
    #   inputId = ns("thresholds"),
    #   label = "Thresholds",
    #   value = FALSE
    # ),
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
      
      # plotting
      ggdepthprofile <- ggplot(data = profile_data(),
                               aes(x = value,
                                   y = depth,
                                   color = MonitoringLocationName,
                                   shape = MonitoringLocationName)) +
        geom_path() + 
        geom_point() +
        labs(x = unique(profile_data()$AxisName),
             y = "Depth (m)",
             color = "Site",
             shape = "Site") +
        facet_grid(row = vars(year),
                   cols = vars(month_name)) +
        scale_color_natparks_d("Yellowstone") +
        theme_minimal()
      
      # adding threshold lines 
      # if(input$thresholds){
      #   ggtimeseries = ggtimeseries +
      #     geom_vline(xintercept = profile_data()$LowerPoint) +
      #     geom_vline(xintercept = profile_data()$UpperPoint)
      # }
      # 
      # converting to plotly 
      ggplotly(ggdepthprofile,
               height = plotht()) |> 
        style(hovertemplate = paste0("<br>Site: ", profile_data()$MonitoringLocationName,
                                     "<br>Date: ", profile_data()$end_date,
                                     "<br>Value: ", profile_data()$value))
    })
    
    # returing data details 
    return(list(depthprofile_data = profile_data))
  })
}