# The User Interface for the GLKN Water Quality Visualizer 

# Libraries ----
library(shiny) # shiny app
library(leaflet) # interactive maps 


# Main UI ----

ui <- shinyUI(
  fluidPage(
    ## formatting ----
    # tags$link(rel = "stylesheet",
    #           type = "text/css",
    #           href = "styles.css"),
    
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
    column(
      width = 3, # 3/12 of the page
      ### Park ----
      selectInput(
        "park",
        "Park",
        choices = c("Choose Park" = "",
                    sort(unique(wqp_data$Park))),
        selected = ""
      ),
      ### Site ---
      selectInput(
        "station",
        "Site",
        choices = c("Choose Site" = "",
                    sort(unique(wqp_data$MonitoringLocationName))),
        selected = "",
        multiple = TRUE
      ),
      ### Map ----
      leafletOutput("map",
                    height = "400px")
    ),
    
    # Right Column ----
    column(width = 9, # 9/12 of the page
           ## Instructions Panel ----
           tabsetPanel(
             tabPanel(
               # Tab Name
               h4("Instructions"),
               # Instruction HTML
               includeHTML("www/Instruction.html")
             ),
             tabPanel(
               # Tab Name
               h4("Time Series"),
               # timeseries_mod.R
               ts_ui("ts"),
               details_ui("details")
             ),
             tabPanel(
               # Tab Name
              h4("Profile Plots"),
              # depthprofile_mod.R
              dp_ui("dp")
             ),
             tabPanel(
               # Tab Name
               h4("Boxplot"),
               # boxplot_mod.R
               bp_ui("bp")
             ),
             tabPanel(
               # Tab Name
               h4("Correlation Plot"),
               # correlation_mod.R
               cp_ui("cp")
             )
           )
           
      )
    )
  )
)    