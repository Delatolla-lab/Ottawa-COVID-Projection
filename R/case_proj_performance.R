# This script runs a loop of projections through previous case data to generate
# a file of historic projections to be used to compare with observed case data.

# load library, packages, functions
library(EpiNow2)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stats)
library(zoo)
library(stringr) 

source("R/epinow_functions.R")
source("R/open_ottawa_scripts.R")

# load covid data
ott_covid_data <-
  read.csv(file.path(getwd(), "Data/Observed Data/OPH_Observed_COVID_Data.csv")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= (max(as.Date(date)) - 16*7))

# Set reporting delay, generation time, incubation period
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

case_proj_hist <- list()
time_frame <- c(0, 28, 56, 84)
x <- 0

for(i in time_frame){
  x <- x + 1
  case_i <- 
    ott_covid_data %>%
    filter(date >= (first(as.Date(ott_covid_data$date)) + i) &
             date <= (first(as.Date(ott_covid_data$date)) + i + 28))
  # Run epinow forecast
  ott_short_forecast_i <- short_term_forecast(
    data = case_i,
    parameter = "observed_new_cases",
    start_date = first(case_i$date), 
    generation_time = generation_time,
    incubation_period = incubation_period,
    reporting_delay = reporting_delay,
    output = "both"
  )
  case_proj_hist[[x]] <- ott_short_forecast_i[[1]]
}

hist_case_projections <- bind_rows(case_proj_hist)

hist_case_projections$type <-
  ifelse(hist_case_projections$date <= max(ott_covid_data$date),
         "historic forecast", "forecast")

#save(hist_case_projections, file = "Data/hist_case_projections.RData")