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
ottawa_case_data <-
  fromJSON("https://opendata.arcgis.com/datasets/6bfe7832017546e5b30c5cc6a201091b_0/FeatureServer/0/query?where=1%3D1&outFields=Date,Cumulative_Cases_by_Episode_Date,Total_Active_Cases_by_Date,Cumulative_Deaths_by_Date_of_Death,Daily_Cases_by_Reported_Date,7-day_Average_of_Newly_Reported_cases_by_Reported_Date,Daily_Cases_by_Episode_Date,Daily_Cases_Linked_to_a_Community_Outbreak_by_Episode_Date,Daily_Cases_Linked_to_an_Institutional_Outbreak_by_Episode_Date,Daily_Cases_Not_Linked_to_an_Outbreak_ie_Sporadic_Cases_by_Episode_Date,Cases_Newly_Admitted_to_Hospital,Cases_Currently_in_ICU,Cases_Currently_in_Hospital&outSR=4326&f=json")
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


