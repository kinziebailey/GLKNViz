# Module for the depth profile plot ----

dp_ui <- function(id){
  
  ns <- NS(id) # creating a namespace
  
  tagList(
    # Parameter Selector
    selectInput(
      inputId = ns("select_param"),
      label = "Select Parameter",
      choices = c("Choose Parameter" = "",
                  c("Dissolved oxygen (DO)" = "Dissolved oxygen (DO)",
                    "Dissolved Oxygen Saturation" = "Dissolved oxygen saturation",
                    "pH" = "pH",
                    "Specific Conductivity" = "Specific conductance",
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
    # About Button
    actionButton(
      inputId = ns("about_ts"),
      label = "About Depth Profiles",
      class = "btn btn-info"
    ),
    plotOutput(ns("DepthProfilePlot"))
  )
}

## Server for depth profiles ----
dp_server <- function(id, user_data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### About Modal ----
    observeEvent(input$about_ts, {
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
    
    ## Reactive for depth profiles ----
    profile_data <- reactive({
      
      # require date
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
    output$DepthProfilePlot <- renderPlot({
      
      # plotting
      ggplot(data = profile_data(),
             aes(x = value,
                 y = depth,
                 color = MonitoringLocationName,
                 shape = MonitoringLocationName)) +
        geom_path() + 
        geom_point() +
        facet_grid(row = vars(year),
                   cols = vars(month)) +
        # facet_wrap(~month) +
        theme_minimal()
    })
  })
}