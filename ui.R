# The User Interface for the GLKN Water Quality Visualizer 

# Libraries ----
library(shiny) # shiny app
library(leaflet) # interactive maps


# Main UI ----

ui <- shinyUI(
  fluidPage(
    ## formatting ----
    theme = "https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css"),
    
    ## Page Header ----
    tags$header(id = "main-header",
                ### GLKN link ----
                  HTML("<span>
                  <a href='https://www.nps.gov/im/glkn/'> <img src='ah_small_black.gif'
                  alt='Water Quality Visualizer'> </a> GLKN Water Quality Data Visualizer</span>")),
    fluidRow(
      
      ## Left Column ----
      column(
        width = 3, # 3/12 of the page
        ### Park ----
        div(id = "park-select",
            selectInput(
              "park",
              "Select Park",
              choices = c("Choose Park" = "",
                          sort(unique(wqp_data$Park))),
              selected = ""
            )
        ),
        ### Site ---
        div(id = "site-select",
            selectizeInput( # needed for deselect x on site
              "station",
              "Select Site",
              choices = sort(unique(wqp_data$MonitoringLocationName)),
              multiple = TRUE, # allows for multiple selections
              options = list(placeholder = "Choose Site", # options for selectize
                             plugins = list("remove_button"))
            )
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
                 # Instruction.html
                 includeHTML("www/Instruction.html")
               ),
               tabPanel(
                 # Tab Name
                 h4("Time Series"),
                 # timeseries_mod.R
                 ts_ui("ts"),
                 details_ui("details_ts")
               ),
               tabPanel(
                 # Tab Name
                 h4("Profile Plots"),
                 # depthprofile_mod.R
                 dp_ui("dp"),
                 details_ui("details_dp")
               ),
               tabPanel(
                 # Tab Name
                 h4("Boxplot"),
                 # boxplot_mod.R
                 bp_ui("bp"),
                 details_ui("details_bp")
               ),
               tabPanel(
                 # Tab Name
                 h4("Correlation Plot"),
                 # correlation_mod.R
                 cp_ui("cp"),
                 details_ui("details_cp")
               ),
               tabPanel(
                 # Tab Name
                 h4("About"),
                 # Instruction.html
                 includeHTML("www/AboutVisualizer.html")
               ),
             )
             
      )
    )
  )
)    