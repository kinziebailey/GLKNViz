# Global file for GLKN Visualizer 

# Libraries ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(plotly)
library(lubridate)
library(NatParksPalettes)

# Load data ----
## WQ Data ----
wqp_data1 <- read.csv('data/wqp_glkn.csv')

### Data wrangling ----
wqp_data <- wqp_data1 |> 
  dplyr::mutate(start_date = as.Date(ActivityStartDate),
                end_date = as.Date(ActivityEndDate),
                month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE)) |>
  dplyr::rename(depth = ActivityDepthHeightMeasure.MeasureValue,
                depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
                value = ResultMeasureValue,
                value_unit = ResultMeasure.MeasureUnitCode,
                lat = LatitudeMeasure,
                lon = LongitudeMeasure) |> 
  dplyr::select(-ActivityStartDate,
                -ActivityEndDate)

## Aquarius Data ----
# temp_array1 <- read.csv('data/temp_array_data.csv')

### Data wrangling ----
# Following code not working 
# temp_array <- temp_data |> 
#   dplyr::mutate(date_time = lubridate::ymd_hms(sub("Z$",
#                                                    "",
#                                                    date_time),
#                                                tz = "America/Denver"))

## Map Data ----
ESRIimagery <- "http://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
ESRItopo <- "http://services.arcgisonline.com/arcgis/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
ESRINatGeo <- "http://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}"

# Make NPS map Attribution
NPSAttrib <- HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")

## Loading Modules ----
### Time Series
source("modules/timeseries_mod.R")

### Depth Profiles
source("modules/depthprofile_mod.R")

### Boxplots 
source("modules/boxplot_mod.R")

### Correlation Plot 
source("modules/correlation_mod.R")

### Details Tables 
source("modules/details_mod.R")