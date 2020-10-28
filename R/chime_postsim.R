# This file prepares the hospital projection dataset for visualization. This
# file takes the newly generated hospitalization projections file from 
# the CHIME simulation and integrates it with the existing projections file
# to be used to update 613covid.ca.

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
hosp_data <- 
  read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))

hosp_admits <- 
  c(hosp_data$observed_new_ICU_p_acute_care	,
    rep(NA, (nrow(hosp_proj) - nrow(hosp_data))))
hosp_census <- c(hosp_data$observed_census_ICU_p_acute_care,
                 rep(NA, (nrow(hosp_proj) - nrow(hosp_data))))

## Integrate hospital data into projections file
hosp_proj$hosp_census_observed <- hosp_census
hosp_proj$hosp_admits_observed <- hosp_admits

# Export projection for visualization
write.csv(hosp_proj, "Data/OPH_projection_estimates.csv")