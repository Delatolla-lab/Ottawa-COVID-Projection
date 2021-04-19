# This script runs a loop of projections through previous hosp data to generate
# a file of historic projections to be used to compare with observed hosp data.

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

# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

hosp_proj_hist <- list()

time_frame <- c(0, 28, 56, 84)
x <- 0

for(i in time_frame){
  x <- x + 1
  hosp_i <- ott_covid_data %>%
    select(date, observed_new_cases, observed_census_ICU_p_acute_care) %>%
    mutate(date = as.Date(date)) %>%
    rename(primary = "observed_new_cases", secondary = "observed_census_ICU_p_acute_care") %>%
    filter(date >= (first(as.Date(ott_covid_data$date)) + i) &
             date <= (first(as.Date(ott_covid_data$date)) + i + 28))
  
  hosp_i <- data.table::setDT(hosp_i)
  
  cases_to_hosp_i <- estimate_secondary(hosp_i, 
                                        delays = delay_opts(list(mean = 2.5, mean_sd = 0.2, 
                                                                 sd = 0.47, sd_sd = 0.1, max = 21)),
                                        secondary = secondary_opts(type = "prevalence"),
                                        obs = obs_opts(scale = list(mean = 0.01, sd = 0.0025)),
                                        control = list(adapt_delta = 0.95))
  
  case_forecast_i <- epinow(reported_cases = copy(hosp_i)[, .(date, confirm = primary)], 
                            generation_time = generation_time,
                            delays = delay_opts(incubation_period, reporting_delay),
                            rt = rt_opts(prior = list(mean = 2, sd = 0.5), rw = 7),
                            gp = NULL, horizon = 14)
  
  hosp_forecast_i <- forecast_secondary(cases_to_hosp_i, case_forecast_i$estimates, all_dates = TRUE)
  
  # Prep data to be visualized
  hosp_proj_i <- hosp_forecast_i[[2]]
  
  hosp_proj_i$variable <- "reported_cases"
  
  hosp_proj_i$type <- ifelse(hosp_proj_i$date <= max(hosp_i$date), "estimate", "forecast")
  
  hosp_proj_hist[[x]] <- hosp_proj_i
}

hist_hosp_projections <- bind_rows(hosp_proj_hist)

hist_hosp_projections$type <-
  ifelse(hist_hosp_projections$date <= max(ott_covid_data$date),
         "historic forecast", "forecast")

#save(hist_hosp_projections, file = "Data/hist_hosp_projections.RData")