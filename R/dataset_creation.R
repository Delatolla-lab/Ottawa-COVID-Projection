# This file pulls covid-19 data from Open Ottawa and prepares a dataset
# for visualization and projection simulations.

## Load libraries
library(tidyverse)
library(jsonlite)
library(tidyr)
library(tidyr)
library(lubridate)
library(stats)
library(zoo)
library(stringr)

## Source script
source("R/open_ottawa_scripts.R")

## Pull JSON objects from OpenOttawa
# covid-19-cases-and-deaths-ottawa
ottawa_case_data <-
  fromJSON("https://services.arcgis.com/G6F8XLCl5KtAlZ2G/arcgis/rest/services/COVID_19_Cases_and_Deaths_Ottawa/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
ottawa_test_data <-
  fromJSON("https://opendata.arcgis.com/datasets/26c902bf1da44d3d90b099392b544b81_0/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

#ottawa_test_data[["features"]][[1]][[1]] <- str_replace(ottawa_test_data[["features"]][[1]][[1]], "2020-", "20-")

## Pull adjusted case data from arcGIS
adjusted_episodes <- adjusted_function("https://www.arcgis.com/sharing/rest/content/items/d010a848b6e54f4990d60a202f2f2f99/data")

## Pull data from objects to generate dataset
ott_observed <- data_creation(ottawa_case_data, ottawa_test_data)[["data"]] %>%
  full_join(adjusted_episodes, by = "date") %>%
  filter(date >= "2020-03-01")

## Export data as csv file
write.csv(ott_observed, "Data/Observed data/OPH_Observed_COVID_Data.csv")


