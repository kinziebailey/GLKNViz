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


# For Aquarius Data 
# remotes::install_github("nationalparkservice/imd-fetchaquarius")  # note repo name diff from package name
# library(fetchaquarius)

# Loading data ----
# station data 
glkn_stations <- read_csv("./data/station.csv")

# char lookup data 
chr_lookup <- read_csv("./data/chr_lookup.csv")

# wqp threshold data 
thresholds <- read_csv("./data/thresholds.csv")

# Getting WQP Data ---- 
# THIS IS EXTREMELY SLOW ----
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
      warning("ERROR: ", conitionMessage(err), " while pulling for ", park)
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

# Data Wrangling ----

## removing unneeded data ----
wqp_data1 <- wqp_data_all |> 
  # removing unneeded CharacteristicNames
  semi_join(chr_lookup,
            by = join_by(CharacteristicName)) |> 
  # removing quality control
  filter(!grepl("Quality Control",
                ActivityTypeCode)) |> 
  # removing air and other
  filter(!grepl("Air|Other",
                ActivityMediaName)) |> 
  filter(!grepl("/", 
                ActivityIdentifier)) |> 
  # adding censored data conditions
  mutate(ResultMeasureValue = case_when(ResultDetectionConditionText == "Present Below Quantification Limit" ~ 
                                          str_extract(ResultCommentText, "\\d*\\.?\\d+"),
                                        TRUE ~ ResultMeasureValue)) |> 
  # removing no detection/not reported
  # filter(!grepl("Not Detected|Not Reported",
  #               ResultDetectionConditionText)) |> # leaving these in for now so we can try to report how many values are there. 
  # correcting depth measurements
  mutate(ActivityDepthHeightMeasure.MeasureValue = if_else(ActivityDepthHeightMeasure.MeasureValue < 0, 0,
                                                           ActivityDepthHeightMeasure.MeasureValue),
         ActivityDepthHeightMeasure.MeasureValue = -abs(ActivityDepthHeightMeasure.MeasureValue),
         ResultMeasureValue = as.numeric(ResultMeasureValue))

## adding station data ----
wqp_data_stations <- wqp_data1 |>
  # adding station data
  left_join(glkn_stations)

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
  left_join(thresh_mln)

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

## Adding characteristicNames and cleaning up columns ----
wqp_data <- wqp_data_thresh |> 
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
         PickListName,
         AxisName)
  
# Writing the new wqp_data ----
write_csv(wqp_data,
          "./data/wqp_glkn.csv")


# Getting Aquarius Data ----
## Note: make sure you are connected to the VPN
## This code takes a while to run and it is just because there is so much data.

## Toolbox
source("https://raw.githubusercontent.com/AndrewBirchHydro/albAquariusTools/main/Aquarius%20basics.R")
timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
publishapiurl='https://aquarius.nps.gov/aquarius/Publish/v2'

# Sites 
# Lake Richie == GLKN_ISRO_03
# Grand Sable == PIRO_01
# Beaver == PIRO_04
# Manitou == SLBE_01
# Bass (North) == SLBE_05
# Shoepack == VOYA_05
# Little Trout == VOYA_21
# Mukooda == VOYA_22

# Temp Arrays 
# Starts with: Water Temp
# Ends with: _array

temp_sites <- c("GLKN_ISRO_03",
                "GLKN_PIRO_01",
                "GLKN_PIRO_04",
                "GLKN_SLBE_01",
                "GLKN_SLBE_05",
                "GLKN_VOYA_05",
                "GLKN_VOYA_21",
                "GLKN_VOYA_22")

# Getting list of water temp data at each site
temp_data_all <- map_dfr(temp_sites, function(site){
  
  # Get dataset for site 
  # data <- Print_datasets(site)
  text <- capture.output(data <- Print_datasets(site))
  
  # Water Temp Datasets 
  temp_data <- data$Identifier[grepl("Water Temp",
                                     data$Identifier,
                                     ignore.case = FALSE) & ! grepl("backup|borrowed|Historical|Inverse",
                                                                    data$Identifier)]
  
  # get each dataset and "clean it up"
  map_dfr(temp_data, function(depth){
    
    # message
    message("Getting data for ", depth)
    
    # get raw data 
    raw_timeseries <- Get_timeseries2(record = depth)
    
    # catching NULL for empty data
    if(is.null(raw_timeseries) || length(raw_timeseries) == 0){
      message("Skipping epty dataset: ", depth)
      return(NULL)
    }
    
    # cleaning up
    temp_array <- try(TS_simplify(data = raw_timeseries), silent = TRUE)
    
    # catching errors and NULLs in TS_simplify
    if(inherits(temp_array, "try-error") || is.null(temp_array)){
      message("Could not 'clean up' dataset: ", depth)
      return(NULL)
    }
    
    # adding information
    temp_array |> 
      dplyr::mutate(#site = site,
                    dataset = depth)
  })
})

# Data Wrangling ----
temp_data <- temp_data_all |> 
  tidyr::separate_wider_regex(dataset,
                              patterns = c("[^.]*\\.", # ignore everything up to the last period
                                           dataset = "[^@]+", # everything before @
                                           "@",
                                           network = "[^_]+", # characters until _
                                           "_",
                                           site = ".*")) |>  # everything after _
  dplyr::mutate(park = str_extract(site,
                                   "[^_]+"))
  
# Writing to 'data' folder ----
write_csv(temp_data,
          "./data/temp_array_data.csv")
