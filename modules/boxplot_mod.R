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
                     "Month" = "month_name",
                     "Site" = "MonitoringLocationName"),
      inline = TRUE
    ),
    # Thresholds Button
    checkboxInput(
      inputId = ns("thresholds"),
      label = "Thresholds",
      value = FALSE
    ),
    # About Button
    actionButton(
      inputId = ns("about_bp"),
      label = "About Boxplots"
    ),
    # Download Button
    downloadButton(ns("download_figure"),
                   "Download Figure"),
    # Plot
    div(style = "min-height: 300px;
                 height: auto;",
        girafeOutput(ns("BoxPlot"))
    )
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

      # continue if data exists
      user_data() |> 
        # filtering parameter
        dplyr::filter(PickListName %in% input$select_param)
    })
    
    ## Render Boxplot ----
    output$BoxPlot <- ggiraph::renderGirafe({
      
      boxplot_df <- boxplot_data()
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(boxplot_df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # Data for Threshold lines 
      threshold_df <- boxplot_df |>
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
      n_data <- boxplot_df |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## below quantification limit
      n_below_quant <- boxplot_df |> 
        dplyr::filter(ResultDetectionConditionText == "< Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## above quantification limit
      n_above_quant <- boxplot_df |> 
        dplyr::filter(ResultDetectionConditionText == "> Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not detected
      n_detection_limit <- boxplot_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not reported
      n_report_limit <- boxplot_df |> 
        dplyr::filter(ResultDetectionConditionText == "Not Reported") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      # Correcting Label Names 
      grouping_names <- c("Year" = "year",
                          "Month" = "month_name",
                          "Site" = "MonitoringLocationName")
      
      # x-axis labels 
      x_axis <- names(grouping_names)[grouping_names == input$date_grouping]
      
      # filtering out NA values for plotting
      boxplot_df <- boxplot_df |> 
        dplyr::filter(!is.na(value))
      
      # Warning if no data
      shiny::validate(
        shiny::need(nrow(boxplot_df) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # plotting 
      ggboxplot <- ggplot(data = boxplot_df,
                          aes(x = factor(.data[[input$date_grouping]]), # from radiobutton
                              y = value,
                              fill = MonitoringLocationName)) + 
        geom_boxplot_interactive(aes(tooltip = after_stat({paste0("Site: ", .data$fill,
                                                                  "\nQ1: ", prettyNum(.data$lower),
                                                                  "\nMedian: ", prettyNum(.data$middle),
                                                                  "\nQ3: ", prettyNum(.data$upper))}))) + 
        labs(x = x_axis,
             y = unique(boxplot_df$AxisName),
             fill = "Site",
             alt = "A boxplot firgure for the parameter of interest.") + 
        scale_fill_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements Plotted: ", n_data,
                       "\nValues < Quantification Limit: ", n_below_quant,
                       "\nValues > Quantification Limit: ", n_above_quant,
                       "\nValues < Detection Limit: ", n_detection_limit,
                       "\nValues Not Reported: ", n_report_limit)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 5),
              axis.title = element_text(size = 8),
              axis.text = element_text(size = 6),
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8))
      
      # adding threshold lines 
      if(input$thresholds){
        ggboxplot <- ggboxplot +
          geom_hline(data = threshold_df,
                     aes(yintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }
      
    girafe(ggobj = ggboxplot,
           height_svg = 3,
           width_svg = 6,
           options = list(opts_toolbar(hidden = "saveaspng")))
      
    })
    
    ## Static plot for download ----
    static_boxplot <- reactive({
      
      df_boxplot <- boxplot_data()
      
      req(nrow(df_boxplot) > 0)
      
      # Data for Threshold lines 
      threshold_df <- df_boxplot |>
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
      n_data <- df_boxplot |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## below quantification limit
      n_below_quant <- df_boxplot |> 
        dplyr::filter(ResultDetectionConditionText == "< Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## above quantification limit
      n_above_quant <- df_boxplot |> 
        dplyr::filter(ResultDetectionConditionText == "> Quantification Limit") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not detected
      n_detection_limit <- df_boxplot |> 
        dplyr::filter(ResultDetectionConditionText == "Not Detected") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      ## not reported
      n_report_limit <- df_boxplot |> 
        dplyr::filter(ResultDetectionConditionText == "Not Reported") |> 
        dplyr::tally() |> 
        dplyr::pull(n)
      
      # Correcting Label Names 
      grouping_names <- c("Year" = "year",
                          "Month" = "month_name",
                          "Site" = "MonitoringLocationName")
      # x-axis labels 
      x_axis <- names(grouping_names)[grouping_names == input$date_grouping]
      
      # plotting 
      b <- ggplot(data = df_boxplot,
                  aes(x = factor(.data[[input$date_grouping]]), # from radiobutton
                      y = value,
                      fill = MonitoringLocationName)) + 
        geom_boxplot() + 
        labs(x = x_axis,
             y = unique(df_boxplot$AxisName),
             fill = "Site") + 
        scale_fill_natparks_d("Yellowstone") +
        ggtitle(paste0("Total Measurements Plotted: ", n_data,
                       "\nValues < Quantification Limit: ", n_below_quant,
                       "\nValues > Quantification Limit: ", n_above_quant,
                       "\nValues < Detection Limit: ", n_detection_limit,
                       "\nValues Not Reported: ", n_report_limit)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 5),
              axis.title = element_text(size = 8),
              axis.text = element_text(size = 6),
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8))
      
      # adding threshold lines 
      if (isTRUE(input$thresholds)) {
        b <- b +
          geom_hline(data = threshold_df,
                     aes(yintercept = thresh,
                         linetype = Threshold),
                     color = "black") +
          scale_linetype_manual(values = c("Upper Threshold" = "dashed",
                                           "Lower Threshold" = "dotted"))
      }
      
      b
      
    })
    
    ### Download handler ----
    output$download_figure <- downloadHandler(filename = function() {
      
      # file name 
      paste0("Boxplot_", gsub("\\s+", "_", input$select_param), "_",
             input$date_grouping,
             ".png")
    },
    
    # saving
    content = function(file) {
      
      b <- static_boxplot()

      ggsave(filename = file,
             plot = b,
             width = 6, 
             height = 3, 
             units = "in",
             device = svglite,
             dpi = 300)
    })
    
    # returning data details
    return(list(boxplot_data = boxplot_data))
  })
}