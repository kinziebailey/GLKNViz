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
      HTML("<span> <a href='https://www.nps.gov/im/glkn/'> <img src='ah_small_black.gif',
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
           ## Instructions Panel ----
           tabsetPanel(
             tabPanel(
             "Instructions",
             suppressWarnings(includeHTML("www/instructions.html"))
             )
           )#,
      )
    )
  )
)    