# Module for the boxplot ----

bp_ui <- function(id){
  
  ns <- NS(id) # creating a namespace
  
  tagList(
    # Parameter Selector
    selectInput(
      inputId = ns("select_param"),
      label = "Select Parameter",
      choices = c("Choose Parameter" = "",
                  sort(unique(wqp_data$PickListName))),
      selected = "",
    ),
    # Date Format Grouping Selector
    radioButtons(
      inputId = ns("date_grouping"),
      label = "Compare By:",
      choices = list("Year" = "year",
                     "Month" = "month",
                     "Site" = "MonitoringLocationName"),
      inline = TRUE
    ),
    # About Button
    actionButton(
      inputId = ns("about_bp"),
      label = "About Boxplots",
      class = "btn btn-info"
    ),
    # Plot
    plotOutput(ns("BoxPlot"))
  )
}

## Server for boxplots ----
bp_server <- function(id, user_data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### About Modal ----
    observeEvent(input$about_bp, {
      showModal(
        modalDialog(title = "About Boxplots", 
                    footer = modalButton("Close"),
                    tags$iframe(src = "AboutBoxplots.html",
                                width = "100%",
                                height = "600px",
                                style = "border:none;")
        )
      )
    })
    
    ### Reactive for box plots ----
    boxplot_data <- reactive({
      
      # required data
      req(input$select_param, input$date_grouping)
      
      # data wrangling 
      boxplot_df1 <- user_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(boxplot_df1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data exists
      boxplot_df <- boxplot_df1 |> 
        # filtering parameter
        dplyr::filter(CharacteristicName %in% input$select_param)
    })
    
    ## Render Boxplot ----
    output$BoxPlot <- renderPlot({
      
      # plotting 
      ggplot(data = boxplot_data(),
             aes(x = factor(.data[[input$date_grouping]]), # from radiobutton
                 y = value,
                 fill = MonitoringLocationName)) + 
        geom_boxplot() + 
        theme_minimal()
    })
    
    # returing data details
    return(list(boxplot_data = boxplot_data))
  })
}