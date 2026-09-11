# The Global file for GLKN Visualizer 

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

### Date filtering for out of schedule data ----
sampling_periods <- data.frame(Park =     c("APIS",  "INDU",  "ISRO",  "PIRO",  "SLBE",  "VOYA",  "SACN"),
                               start_md = c("06-01", "04-15", "06-01", "06-01", "05-15", "05-16", "03-15"),
                               end_md =   c("09-20", "10-15", "09-30", "09-15", "09-30", "09-30", "11-30"))

data_filter <- function(df){
  df |> 
    left_join(sampling_periods,
              by = "Park") |> 
    mutate(month_day = format(end_date, "%m-%d")) |> 
    filter(month_day >= start_md,
           month_day <= end_md) |> 
    select(-month_day,
           -start_md,
           -end_md)
}

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