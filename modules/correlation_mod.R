# Module for the Correlation plot ----

cp_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    # Parameter 1 Selector
    selectInput(
      inputId = ns("select_param1"),
      label = "Select x-axis Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$CharacteristicName))),
      selected = ""
    ),
    # Parameter 2 Selector 
    selectInput(
      inputId = ns("select_param2"),
      label = "Select y-axis Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$CharacteristicName))),
      selected = ""
    ),
    # Add Regression
    # radioButtons(
    #   inputId = ns("regression_selection"),
    #   label = "Regression Type:",
    #   choices = list("Linear" = "linear",
    #                  "LOESS" = "loess",
    #                  "Polynomial" = "polynomial"),
    #   inline = TRUE
    # ),
    # About Button
    actionButton(
      inputId = ns("about_cp"),
      label = "About Correlation Plot",
      class = "btn btn-info"
    ),
    plotOutput(ns("CorrelationPlot"))
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
    correlation_data <- reactive({
      
      req(input$select_param1, input$select_param2)
      
      # data wrangling 
      correlation_df1 <- user_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(correlation_df1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data exists
      correlation_df <- correlation_df1 |> 
        # averaging if multiple values per sample period
        dplyr::summarise(value = mean(value, na.rm = TRUE),
                         .by = c(end_date,
                                 MonitoringLocationName, 
                                 CharacteristicName)) |> 
        dplyr::filter(CharacteristicName %in% c(input$select_param1,
                                                input$select_param2)) |> 
        tidyr::pivot_wider(names_from = CharacteristicName,
                           values_from = value)
    })
    
    ## Render Correlation Plot ----
    
    output$CorrelationPlot <- renderPlot({
      
      # plotting 
      ggplot(data = correlation_data(),
             aes(x = .data[[input$select_param1]],
                 y = .data[[input$select_param2]],
                 color = MonitoringLocationName)) +
        geom_point() + 
        geom_smooth(method= lm,
                    se = FALSE) +
        stat_poly_eq(use_label(c("eq", "R2"))) +
        theme_minimal()
    })
  })
}