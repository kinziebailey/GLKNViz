# Information ----
# This code will need to be run annually, after the WQP data have been 
# updated. It will make sure the downloaded data are in the correct format 
# to run with the GLKNViz dashboard. It will only need to be run once 
# before the dashboard is updated and republished. 
# 
# Before this code is run. Please make sure you have added a 'data' folder to 
# the working directory. Run the following in the console: dir.create("data")
# Please make sure stations.csv, chr_lookup.csv, and thresholds.csv are up to 
# date and added to the 'data' folder.

# Once the folder and data files have been added/updated, you should be able
# to run this code and then run the app.


# Loading required packages ----
# Ignore any warnings and messages from the libraries
library(dataRetrieval) # download from WQP
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(stringr) # data wrangling
library(tidyr) # data wrangling 
library(purrr) # map functions
library(lubridate) # data wrangling

# Loading data ----
# station data 
glkn_stations <- read_csv("./data/station.csv")

# char lookup data 
chr_lookup <- read_csv("./data/chr_lookup.csv")

# wqp threshold data 
thresholds <- read_csv("./data/thresholds.csv")

# OPTION: Getting WQP Data ---- 
# parks <- sort(unique(glkn_stations$Park))
# 
# WQPViews <- lapply(parks, function(park){
# 
#   sites <- glkn_stations |>
#     dplyr::filter(Park == park) |>
#     dplyr::pull(MonitoringLocationIdentifier)
# 
#   message("\nPulling WQP data for ", park)
# 
#   # Create progress bar for THIS park
#   pb <- txtProgressBar(min = 0, max = length(sites), style = 3)
# 
#   # Download each site with progress bar
#   dat_list <- vector("list", length(sites))
# 
#   for (i in seq_along(sites)) {
#     dat_list[[i]] <- suppressMessages(readWQPdata(siteid = sites[i])) |>
#       dplyr::mutate(ResultMeasureValue = as.character(ResultMeasureValue))
# 
#     setTxtProgressBar(pb, i)
#   }
# 
#   close(pb)
# 
#   # Combine all sites for this park
#   dplyr::bind_rows(dat_list)
# })
# 
# # Combine all parks
# wqp_data_all <- dplyr::bind_rows(WQPViews)



# new data import

# data_full <- readWQPdata(siteid = "11NPSWRD_WQX-VOYA_01",
#                          # characteristicName = "pH",
#                          # dataProfile = "fullPhysChem",
#                          service = "ResultWQX3")

# Getting WQP Data ---- 
# Looping through parks to get WQP data. This will give you updated data for all
# parks and sites listed in stations.csv. ----
WQPViews <- lapply(sort(unique(glkn_stations$Park)), function(park){
  
  # Getting site ID for park
  sites <- glkn_stations |>
    dplyr::filter(Park == park) |>
    dplyr::pull(MonitoringLocationIdentifier)
  
  message("Pulling WQP data for ", park)
  
  # Getting WQP Data
  dat <- tryCatch(
    {
      suppressMessages(readWQPdata(siteid = sites))
    },
    # warning, still returns partial data
    warning = function(mess){
      warning("Warning: ", conditionMessage(mess), " while pulling for ", park)
              suppressMessages(readWQPdata(siteid = sites))
    },
    # error, return null for park
    error = function(err){
      warning("ERROR: ", conditionMessage(err), " while pulling for ", park)
      return(NULL)
    }
    )
  
  # make ResultMeasureValue all chr
  if(!is.null(dat)){
  dat <- dat |> 
    dplyr::mutate(ResultMeasureValue = as.character(ResultMeasureValue))
  }
  
  dat
  
})

# creating dataframe
wqp_data_all <- bind_rows(WQPViews)

write_csv(wqp_data_all,
          "./data/wqp_glkn_all.csv")

# Data Wrangling ----

## removing unneeded data ----
wqp_data1 <- wqp_data_all |> 
  # removing unneeded CharacteristicNames
  semi_join(chr_lookup,
            by = join_by(CharacteristicName)) |> 
  mutate(year = year(ActivityEndDate)) |> 
  # removing quality control
  filter(!grepl("Quality Control",
                ActivityTypeCode)) |> 
  # removing air and other
  filter(!grepl("Air|Other",
                ActivityMediaName)) |> 
  filter(!grepl("/", 
                ActivityIdentifier)) |>
  # removing additional bottom samples for thermally stratified sites
  filter(!(CharacteristicName %in% c("Ammonium",
                                     "Nitrate + Nitrite",
                                     "Nitrogen",
                                     "Phosphorus") & 
             ActivityTopDepthHeightMeasure.MeasureValue > 2)) |> 
  filter(!(CharacteristicName %in% c("Ammonium",
                                     "Nitrate + Nitrite",
                                     "Nitrogen",
                                     "Phosphorus") & 
             SampleCollectionEquipmentName == "Van Dorn Bottle")) |> 
  filter(!(LaboratoryName == "White Water Associates" &
             year >= 2014)) |>
  # removing non-detected data
  # filter(!ResultDetectionConditionText == "Not Detected") |> 
  # adding censored data conditions
  mutate(ResultMeasureValue = case_when(ResultDetectionConditionText == "Present Below Quantification Limit" ~ 
                                          str_extract(ResultCommentText, "\\d*\\.?\\d+"),
                                        TRUE ~ ResultMeasureValue),
         # adding to ResultDetectionConditionText
         ResultDetectionConditionText = case_when(
           ResultDetectionConditionText == "Present Above Quantification Limit" ~ "> Quantification Limit",
           ResultDetectionConditionText == "Present Below Quantification Limit" ~ "< Quantification Limit",
           ResultDetectionConditionText == "" ~ "Detected and Quantified",
           TRUE ~ ResultDetectionConditionText),
         # correcting depth measurements
         ActivityDepthHeightMeasure.MeasureValue = if_else(ActivityDepthHeightMeasure.MeasureValue < 0, 0,
                                                           ActivityDepthHeightMeasure.MeasureValue),
         ActivityDepthHeightMeasure.MeasureValue = -abs(ActivityDepthHeightMeasure.MeasureValue),
         ResultMeasureValue = as.numeric(ResultMeasureValue))

## adding station data ----
wqp_data_stations <- wqp_data1 |>
  # adding station data
  left_join(glkn_stations,
            by = join_by(OrganizationIdentifier,
                         OrganizationFormalName,
                         MonitoringLocationIdentifier))

## adding threshold data ----

# thresholds with MLN
thresh_mln <- thresholds |> 
  filter(!is.na(MonitoringLocationName))

# thresholds without MLN
thresh_no <- thresholds |> 
  filter(is.na(MonitoringLocationName)) |> 
  select(-MonitoringLocationName)

### joining thresholds that have MonitoringLocationName
wqp_data_ml <- wqp_data_stations |>
  left_join(thresh_mln,
            by = join_by(CharacteristicName,
                         Park, MonitoringLocationName))

### joining thresholds that have no MonitoringLocationName
wqp_data_thresh <- wqp_data_ml |>
  left_join(thresh_no,
            by = c("Park",
                   "CharacteristicName")) |>
  mutate(LowerPoint = coalesce(LowerPoint.x,
                               LowerPoint.y),
         UpperPoint = coalesce(UpperPoint.x,
                               UpperPoint.y),
         LowerDescription = coalesce(LowerDescription.x,
                                     LowerDescription.y),
         UpperDescription = coalesce(UpperDescription.x,
                                     UpperDescription.y),
         Reference = coalesce(Reference.x,
                              Reference.y),
         Notes = coalesce(Notes.x,
                          Notes.y)) |> 
  select(-ends_with(".x"),
         -ends_with(".y"))

## Removing VOYA Sites (Agnes, Jorgans, Oleary, Quarterline, and War Club) ----
wqp_data_new <- wqp_data_thresh |> 
  filter(!(MonitoringLocationName %in% c("Agnes Lake",
                                         "Jorgens Lake",
                                         "O'Leary Lake",
                                         "Quarter Line Lake",
                                         "War Club Lake")))

## Adding characteristicNames and cleaning up columns ----
wqp_data <- wqp_data_new |> 
  # adding char names
  left_join(chr_lookup,
            by = "CharacteristicName") |> 
  # adding year for cleaning purposes
  mutate(year = format(ActivityEndDate, "%Y"),
         month = format(ActivityEndDate, "%m")) |>
  # filtering by sites that have >= 5 years of data
  filter(n_distinct(year) >= 5,
         .by = c(MonitoringLocationIdentifier,
                 CharacteristicName)) |>
  # selecting necessary columns 
  select(ActivityIdentifier,
         MonitoringLocationIdentifier,
         ActivityMediaName,
         ActivityStartDate,
         ActivityEndDate,
         year,
         month,
         ActivityStartTime.TimeZoneCode,
         ActivityDepthHeightMeasure.MeasureValue,
         ActivityDepthHeightMeasure.MeasureUnitCode,
         ActivityTopDepthHeightMeasure.MeasureValue,
         ActivityTopDepthHeightMeasure.MeasureUnitCode,
         ResultDetectionConditionText,
         DetectionQuantitationLimitTypeName,
         CharacteristicName,
         ResultMeasureValue,
         ResultMeasure.MeasureUnitCode,
         Park,
         MonitoringLocationName,
         MonitoringLocationTypeName,
         HUCEightDigitCode,
         LatitudeMeasure,
         LongitudeMeasure,
         HorizontalCoordinateReferenceSystemDatumName,
         LowerPoint,
         UpperPoint,
         LowerDescription,
         UpperDescription,
         Reference,
         PickListName,
         AxisName)
  
# Writing the new wqp_data ----
write_csv(wqp_data,
          "./data/wqp_glkn.csv")
