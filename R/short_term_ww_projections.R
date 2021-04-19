# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)
library(tidyverse)
library(lubridate)

source("R/open_ottawa_scripts.R")
source("R/wastewater.R")

## Data Prep
case_data <-
  read.csv(file.path(getwd(), "Data/Observed Data/OPH_Observed_COVID_Data.csv")) %>% 
  select(date, observed_new_cases) %>%
  mutate(date = as.Date(date))

ww_data <-
  read.csv("https://raw.githubusercontent.com/Big-Life-Lab/covid-19-wastewater/main/data/wastewater_virus.csv")

waste_clean <- wastewater_prep(ww_data) %>%
  select(date, N1_N2_avg) %>%
  mutate(date = as.Date(date)) %>%
  left_join(case_data, by = "date") %>%
  rename(primary = "observed_new_cases", secondary = "N1_N2_avg") %>%
  relocate(secondary, .after = primary) %>%
  mutate(secondary = secondary *1000000) %>%
  na.omit %>%
  mutate(secondary = as.integer(secondary)) %>%
  filter(date >= (max(as.Date(date)) - 12*7))

ww_data <- data.table::setDT(waste_clean)

# Estimate relationship between cases and ww
cases_to_ww <- estimate_secondary(ww_data, 
                                  delays = 
                                    delay_opts(list(mean = -5, mean_sd = 0.2, 
                                                    sd = 0.47, sd_sd = 0.1,
                                                    max = 0)),
                                  secondary =
                                    secondary_opts(type = "prevalence"),
                                  obs =
                                    obs_opts(scale = list(mean = 0.01,
                                                          sd = 0.0025)),
                                  control = list(adapt_delta = 0.95))

# Set reporting delay, generation time, incubation period
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# Forecast hospitalizations using case data
case_forecast <- epinow(reported_cases = copy(ww_data)[, .(date, confirm =
                                                             primary)], 
                        generation_time = generation_time,
                        delays = delay_opts(incubation_period, reporting_delay),
                        rt = rt_opts(prior = list(mean = 1.5, sd = 0.5),
                                     rw = 7),
                        gp = NULL, horizon = 14)

ww_forecast <- forecast_secondary(cases_to_ww, case_forecast$estimates,
                                  all_dates = TRUE)

ww_proj <- ww_forecast[[2]]

ww_proj$variable <- "reported_cases"

ww_proj$type <- ifelse(ww_proj$date <= max(case_data$date), "estimate",
                       "forecast")

save(ww_proj, ww_data, file = "Data/short_term_ww_proj.RData")
