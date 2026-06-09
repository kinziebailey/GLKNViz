# The User Interface for the GLKN Water Quality Visualizer 

# Libraries ----
library(shiny) # shiny app
library(leaflet) # interactive maps 


# Main UI ----

ui <- shinyUI(
  fluidPage(
    
    ## Page Header ----
    tags$header(
      ### style ----
      style = "background-color: #222;
               color: white; 
               padding: 15px; 
               font-size: 22px; 
               font-weight:bold;
               margin-bottom: 10px;",
      ### GLKN link ----
      HTML("<span> 
      <a href='https://www.nps.gov/im/glkn/'> <img src='ah_small_black.gif' 
      alt='Water Quality Visualizer'> </a> GLKN Water Quality Data Visualizer</span>")
    ),
    
  fluidRow(
    ## Left Column ----
    ### Map ----
    column(
      width = 3, # 3/12 of the page
      leafletOutput("map",
                    height = "85vh")
    ),
    
    # Right Column ----
    column(width = 9, # 9/12 of the page
           ## User Controls ----
           ### Park ----
           selectInput(
             "park",
             "Choose Park:",
             choices = sort(unique(wqp_data$Park)),
             selected = unique(wqp_data$Park)[1]
           ),
           ### Site ---
           selectInput(
             "station",
             "Choose Site:",
             choices = sort(unique(wqp_data$MonitoringLocationName)),
             selected = unique(wqp_data$MonitoringLocationName)[1]
           ),
           ### Param ----
           selectInput(
             "parameter",
             "Choose Parameter:",
             choices = sort(unique(wqp_data$CharacteristicName)),
             selected = unique(wqp_data$CharacteristicName)[1]
           ),
           ### Date Range ----
           dateRangeInput(
             "date_range",
             "Date Range:",
             start = min(wqp_data$end_date, na.rm = TRUE),
             end = max(wqp_data$end_date, na.rm = TRUE)
           ),
           ## Instructions Panel ----
           tabsetPanel(
             tabPanel(
             "Instructions",
             includeHTML("www/Instruction.html")
             ),
             tabPanel(
               "Time Series",
               plotOutput("TimeSeriesPlot")
             )
           )
           
      )
    )
  )
)    