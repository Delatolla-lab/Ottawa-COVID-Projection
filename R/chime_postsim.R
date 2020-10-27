# This file prepares the hospital projection dataset for visualization. This
# file takes the newly generated hospitalization projections file from 
# the CHIME simulation and integrates it with the existing projections file
# to be used in the update of 613covid.ca

## Load libraries
library(tidyverse)
library(dplyr)
library(jsonlite)

## Source functions
source("R/open_ottawa_scripts.R")
source("R/chime_functions.R")

## Load datasets
old_proj <- 
  read.csv(file.path(getwd(),"Data/OPH_projection_estimates.csv"))
new_proj <- 
  read.csv(file.path(getwd(),"Python/output/output/hosp_projections.csv"))

## Integrate new projections into old projections
hosp_proj <- hosp_integrate(
  old_proj = old_proj, # old projections
  new_proj = new_proj, # newly generated simulation
  starting_date = "2020-07-07" # starting date of simulation
)

## Extract hospital census & daily admissions from open Ottawa
hosp_object <- 
  fromJSON("https://opendata.arcgis.com/datasets/6bfe7832017546e5b30c5cc6a201091b_0/FeatureServer/0/query?where=1%3D1&outFields=Cases_Newly_Admitted_to_Hospital,Cases_Currently_in_Hospital,_Date&outSR=4326&f=json")

hosp_data <- data_extract(hosp_object)

hosp_admits <- 
  c(hosp_data$Cases_Newly_Admitted_to_Hospital,
    rep(NA, (nrow(hosp_proj) - nrow(hosp_data))))
hosp_census <- c(hosp_data$Cases_Currently_in_Hospital,
                 rep(NA, (nrow(hosp_proj) - nrow(hosp_data))))

## Integrate hospital data into projections file
hosp_proj$hosp_census_observed <- hosp_census
hosp_proj$hosp_admits_observed <- hosp_admits

# Export projection for visualization
write.csv(hosp_proj, "Data/OPH_projection_estimates.csv")