# Global file for GLKN Visualizer 

# Libraries ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(lubridate)
library(NatParksPalettes)
library(ggiraph)

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