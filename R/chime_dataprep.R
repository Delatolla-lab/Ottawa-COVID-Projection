# This file prepares the time-series dataset needed to generate covid-19
# hospitalization projections. This is based on the Bayesian extension to the
# CHIME model developed by the University of Pennsylvania Health System.
# Their repository can be found here: https://github.com/pennsignals/chime_sims

# Load libraries
library(tidyverse)
library(dplyr)
library(jsonlite)

# Source functions
source("R/open_ottawa_scripts.R")
source("R/chime_functions.R")

# Generate data file for simulation
hosp_export(
  json_address = # JSON link for Open Ottawa simulation
    "https://opendata.arcgis.com/datasets/6bfe7832017546e5b30c5cc6a201091b_0/FeatureServer/0/query?where=1%3D1&outFields=Date,Cases_Currently_in_Hospital&outSR=4326&f=json",
  starting_date = "2020-07-07", # Starting date for simulation
  dest = "Python/data/OTT_ts.csv")