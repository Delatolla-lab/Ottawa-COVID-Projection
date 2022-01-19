library(tidyverse)
library(ggplot2)
library(EpiNow2)
library(stringr)
library(purrr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(zoo)
source("R/epinow_functions.R")
source("R/wastewater.R")

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

# Projections using 1 month of data
ww_forecast_1_month <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = as.Date(Sys.Date() - 30),
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "projections"
)

save(ww_forecast_1_month, file = "ww_projections_development/ww_forecast_1_month.RData")

# Projections using 3 months of data
ww_forecast_3_month <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = as.Date(Sys.Date() - (30*3)),
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "projections"
)

save(ww_forecast_3_month, file = "ww_projections_development/ww_forecast_3_month.RData")

# Projections using 6 months of data
ww_forecast_6_month <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = as.Date(Sys.Date() - (30*6)),
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "projections"
)

save(ww_forecast_6_month, file = "ww_projections_development/ww_forecast_6_month.RData")

# Projections using 12 months of data
ww_forecast_12_month <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = as.Date(Sys.Date() - (30*12)),
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "projections"
)

save(ww_forecast_12_month, file = "WW Projections Development/ww_forecast_12_month.RData")