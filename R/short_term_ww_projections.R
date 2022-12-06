# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)
library(tidyverse)
library(lubridate)
library(zoo)

# Seems unused here
#source("R/open_ottawa_scripts.R")
source("R/wastewater.R")
source("R/epinow_functions.R")

# Date setback variables
time_to_set_back <- -7
measure_to_set_back <- "months"

ww_data <-
  read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv")

ww_clean <- wastewater_prep(ww_data) %>%
  select(date, N1_N2_avg, N1_N2_avg_clean) %>%
  mutate(date = as.Date(date))

# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- NULL
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# Generate start date from end date
end_date <- as.Date(last(ww_clean$date))
start_date <- seq(as.Date(end_date), length = 2, by = paste(time_to_set_back, measure_to_set_back))[2]

ww_forecast <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = start_date,
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both",
  horizon = 7,
  CrI = c(0.2, 0.5, 0.9,0.75)
)

save(ww_forecast, file = "Data/short_term_ww_proj.RData")