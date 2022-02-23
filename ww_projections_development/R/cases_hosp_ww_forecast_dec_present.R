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
ww_data <-
  read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv")

# prep ww data
ww_clean <- wastewater_prep(ww_data) %>%
  select(date, N1_N2_avg, N1_N2_avg_clean) %>%
  mutate(date = as.Date(date))

# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# run case forecast
cases_forecast_dec21_pres22 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_new_cases",
  start_date = "2021-12-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)

# run hosp forecast
hosp_forecast_dec21_pres22 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-12-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)

# run ww forecast
ww_forecast_dec21_pres22 <- short_term_forecast(
  data = ww_clean,
  input = "N1_N2_avg_clean",
  start_date = "2021-12-01",
  input_multiplier = 1000000,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = NULL,
  output = "both"
)

# save forecasts
save(cases_forecast_dec21_pres22, file = "ww_projections_development/Data/dec21_present22_cases_proj.RData")
save(hosp_forecast_dec21_pres22, file = "ww_projections_development/Data/dec21_present22_hosp_proj.RData")
save(ww_forecast_dec21_pres22, file = "ww_projections_development/Data/dec21_present22_ww_proj.RData")


# Set reporting delay to 10 days, generation time, incubation period for simulation of hospital data
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(10), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")


# run hosp forecast
hosp_forecast_dec21_pres22_delay_10 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-12-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)


# save forecasts
save(hosp_forecast_dec21_pres22_delay_10, file = "ww_projections_development/Data/dec21_present22_hosp_proj_delay_10.RData")


# Set reporting delay to 14 days, generation time, incubation period for simulation of hospital cases
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(14), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")


# run hosp forecast
hosp_forecast_dec21_pres22_delay_14 <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-12-01",
  input_multiplier = 1,
  omit_last_date = FALSE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)


# save forecasts
save(hosp_forecast_dec21_pres22_delay_14, file = "ww_projections_development/Data/dec21_present22_hosp_proj_delay_14.RData")

