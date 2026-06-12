# Module for the Data Details ----

## User Interface ----
details_ui <- function(id){
  
  ns <- NS(id) # creating namespace 
  
  tabsetPanel(
    id = ns("data_tabs"),
    # tabPanel("Summary",
    #          dataTableOutput(ns("summary"))),
    tabPanel("Table",
             DT::DTOutput(ns("table"))),
    # tabPanel("Exceedances",
    #          verbatimTextOutput(ns("exceedances")))
  )
}

## Server for data tabs ----
details_server <- function(id, data){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ### Render tables ----
    # Summary
    # output$summary <- renderDataTable({
    #   
    #   # data from timeseries plot
    #   req(data())
    #   
    #   summary_df1 <- data()
    #   
    #   # Warning if no data
    #   shiny::validate(
    #     shiny::need(nrow(summary_df1) > 0,
    #                 "No data available for the selected Park / Site / Parameter"))
    #   
    #   # data wrangling
    #   summary_df <- summary_df1 |> 
    #     filter(MonitoringLocationName == "Locator Lake" &
    #              CharacteristicName %in% c("pH", "Silicate"))|> 
    #     dplyr::select(Park,
    #                   MonitoringLocationName,
    #                   year,
    #                   month,
    #                   CharacteristicName,
    #                   value) |> 
    #     dplyr::summarise(count = sum(!is.na(value)),
    #                      Minimum = if (count >= 1) min(value, na.rm = TRUE) else NA_real_,
    #                      Q1 = if (count >= 1) quantile(value, probs = 0.25, na.rm = TRUE) else NA_real_,
    #                      Mean = if (count >= 1) mean(value, na.rm = TRUE) else NA_real_,
    #                      Meadian = if (count >= 1) median(value, na.rm = TRUE) else NA_real_,
    #                      Q3 = if (count >= 1) quantile(value, probs = 0.75, na.rm = TRUE) else NA_real_,
    #                      Maximum = if (count >= 1) max(value, na.rm = TRUE) else NA_real_,
    #                      StandardDeviation = if (count >= 1) sd(value, na.rm = TRUE) else NA_real_,
    #                      TotalMeasurements = if (count >= 1) n() else NA_real_,
    #                      MissingValues = if (count >= 1) sum(is.na(value)) else NA_real_,
    #                      .by = c("Park",
    #                              "MonitoringLocationName",
    #                              "year",
    #                              # "month",
    #                              "CharacteristicName"))
    #   
    #   # summarize 
    #   summary(data())
    # })
    
    # Data Table 
    output$table <- DT::renderDT({
      
      # data from timeseries plot
      req(data())
      
      data_values1 <- data()
      
      # warning if no data 
      shiny::validate(
        shiny::need(nrow(data_values1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # continue if data
      data_values <- data_values1 |> 
        dplyr::select(any_of(c("Park",
                               "MonitoringLocationName",
                               "lat",
                               "lon",
                               "end_date",
                               "DisplayName",
                               "value",
                               "value_unit"))) |> 
        dplyr::arrange(Park,
                       MonitoringLocationName,
                       end_date,
                       DisplayName) |> 
        dplyr::rename(Site = MonitoringLocationName,
                      Latitude = lat,
                      Longitude = lon,
                      Date = end_date,
                      Parameter = DisplayName,
                      Value = value,
                      Units = value_unit)
      
      # table  
      DT::datatable(data_values,
       options = list(autoWidth = F,
                      buttons = c("Copy",
                                  "CSV",
                                  "Excel"),
                      keys = TRUE),
       class = "stripe hover order-column cell-border compact",
       rownames = FALSE,
       filter = "top",
       extension = c("Buttons",
                     "KeyTable"))
            
    })
    
    # Exceedances
    # output$summary <- renderPrint({
    #   
    #   # data from timeseries plot
    #   req(data())
    #   
    #   # summarize 
    #   summary(data())
    # })
  })
}