# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)
library(tidyverse)
library(lubridate)
library(zoo)

source("R/open_ottawa_scripts.R")
source("R/wastewater.R")
source("R/epinow_functions.R")

# load data
ott_covid_data <- read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))

# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(10), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")


# run hosp forecast
hosp_forecast_sep21_pres22_delay_10 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-09-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)


# save forecasts
save(hosp_forecast_sep21_pres22_delay_10, file = "ww_projections_development/sep21_present22_hosp_proj_delay_10.RData")

# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(14), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")


# run hosp forecast
hosp_forecast_sep21_pres22_delay_14 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-09-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)


# save forecasts
save(hosp_forecast_sep21_pres22_delay_14, file = "ww_projections_development/sep21_present22_hosp_proj_delay_14.RData")
