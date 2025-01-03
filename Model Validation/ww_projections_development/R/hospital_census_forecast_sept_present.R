# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)
library(tidyverse)
library(lubridate)
library(zoo)

source("../R/open_ottawa_scripts.R")
source("../R/wastewater.R")
source("../R/epinow_functions.R")


ott_covid_data <- read.csv(file.path(getwd(), "../Data/Observed data/OPH_Observed_COVID_Data.csv"))
head(ott_covid_data)
# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

hosp_forecast <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = "2021-09-24",
  input_multiplier = 1,
  omit_last_date = TRUE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)

save(hosp_forecast, file = "sept_present_hosp_proj.RData")
load("sept_present_hosp_proj.RData")
