# Module for the Data Details ----

## User Interface ----
details_ui <- function(id){
  
  ns <- NS(id) # creating namespace 
  
  tabsetPanel(
    id = ns("data_tabs"),
    tabPanel("Table",
             DT::DTOutput(ns("table"))),
    tabPanel("Exceedances",
             DT::DTOutput(ns("exceedances")))
  )
}

## Server for data tabs ----
details_server <- function(id, data_from){
  
  ## Loading module ----
  moduleServer(id, function(input, output, session){
    
    ## Getting data if reactive or dataframe
    get_data <- reactive({
      
      if(shiny::is.reactive(data_from)) data_from() else data_from
      
    })
    
    # Data Table 
    output$table <- DT::renderDT(server = FALSE, {
      
      # required data
      req(get_data())
      
      data_values1 <- get_data()
      
      # warning if no data 
      shiny::validate(
        shiny::need(nrow(data_values1) > 0,
                    "No data available for the selected Park / Site / Parameter"))
      
      # Selecting existing columns to arrange by
      cols <- c("Park", 
                "MonitoringLocationName", 
                "end_date", 
                "AxisName", 
                "depth")
      
      cols_existing <- intersect(cols, names(data_values1))
      
      
      # continue if data
      data_values <- data_values1 |> 
        dplyr::select(any_of(c("Park",
                               "MonitoringLocationName",
                               "lat",
                               "lon",
                               "end_date",
                               "depth",
                               "AxisName",
                               "value",
                               "value_unit"))) |> 
        dplyr::arrange(dplyr::pick(all_of(cols_existing))) |> 
        dplyr::rename(Site = MonitoringLocationName,
                      Latitude = lat,
                      Longitude = lon,
                      Date = end_date,
                      Parameter = AxisName,
                      Value = value,
                      Unit = value_unit) |> 
        dplyr::rename_with(.fn = \(x) sub("^depth$",
                                          "Depth (m)",
                                          x),
                           .cols = any_of("depth")) |> 
        dplyr::relocate(any_of("Depth (m)"), 
                        .after = Parameter)
      
      # table  
      DT::datatable(data_values,
                    extension = c("Buttons",
                                  "KeyTable"),
                    options = list(dom = "Bfrtip",
                                   autoWidth = F,
                                   buttons = c("copy",
                                               "csv",
                                               "excel"),
                                   keys = TRUE),
                    class = "stripe hover order-column cell-border compact",
                    rownames = FALSE,
                    filter = "top")
      
    })
    
    # Exceedances
    output$exceedances <- DT::renderDT(server = FALSE, {
      
      # required data
      req(get_data())
      
      exeedance_values1 <- get_data()
      
      # warning if no data 
      shiny::validate(
        shiny::need(nrow(exeedance_values1) > 0,
                    "No data available for the selected Park / Site / Parameter"))

      # continue if data 
      exceedance_values2 <- wqp_data |> 
        dplyr::semi_join(exeedance_values1,
                         by = c("Park", 
                                "MonitoringLocationName", 
                                "end_date",
                                "AxisName")) |> 
        dplyr::filter(value > UpperPoint | value < LowerPoint) 
      
      # Selecting existing coluns to arrange by 
      cols_e <- c("Park", 
                "MonitoringLocationName", 
                "end_date", 
                "AxisName", 
                "depth")
      
      cols_e_existing <- intersect(cols_e, names(exceedance_values2))
      
      exceedance_values <- exceedance_values2 |> 
        dplyr::select(any_of(c("Park",
                               "MonitoringLocationName",
                               "lat",
                               "lon",
                               "end_date",
                               "depth",
                               "AxisName",
                               "value",
                               "value_unit",
                               "LowerPoint",
                               "UpperPoint",
                               "LowerDescription",
                               "UpperDescription"))) |>  
        dplyr::arrange(dplyr::pick(all_of(cols_e_existing))) |>
        dplyr::rename(Site = MonitoringLocationName,
                      Latitude = lat,
                      Longitude = lon,
                      Date = end_date,
                      Parameter = AxisName,
                      Value = value,
                      Units = value_unit,
                      `Lower Threshold` = LowerPoint,
                      `Upper Threshold` = UpperPoint) |> 
        dplyr::rename_with(.fn = \(x) sub("^depth$",
                                          "Depth (m)",
                                          x),
                           .cols = any_of("depth")) |> 
        dplyr::relocate(any_of("Depth (m)"), 
                        .after = Parameter)
        
      # table  
      DT::datatable(exceedance_values,
                    extension = c("Buttons",
                                  "KeyTable"),
                    options = list(dom = "Bfrtip",
                                   autoWidth = F,
                                   buttons = c("copy",
                                               "csv",
                                               "excel"),
                                   keys = TRUE),
                    class = "stripe hover order-column cell-border compact",
                    rownames = FALSE,
                    filter = "top")
      
      
    })
  })
}