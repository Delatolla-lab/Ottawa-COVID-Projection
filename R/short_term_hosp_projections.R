# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)
library(tidyverse)

# Data Prep
data <- read.csv(file.path(getwd(),"Data/Observed data/OPH_Observed_COVID_Data.csv")) %>%
  select(date, observed_new_cases, observed_census_ICU_p_acute_care) %>%
  mutate(date = as.Date(date)) %>%
  rename(primary = "observed_new_cases", secondary = "observed_census_ICU_p_acute_care") %>%
  filter(date >= (max(as.Date(date)) - 12*7))

data <- data.table::setDT(data)

# Estimate relationship between cases and hospitalization
cases_to_hosp <- estimate_secondary(data, 
                                    delays = delay_opts(list(mean = 2.5, mean_sd = 0.2, 
                                                               sd = 0.47, sd_sd = 0.1, max = 21)),
                                    secondary = secondary_opts(type = "prevalence"),
                                    obs = obs_opts(scale = list(mean = 0.01, sd = 0.0025)),
                                    control = list(adapt_delta = 0.95))

# Forecast cases & hospitalizations
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

case_forecast <- epinow(reported_cases = copy(data)[, .(date, confirm = primary)], 
                        generation_time = generation_time,
                        delays = delay_opts(incubation_period, reporting_delay),
                        rt = rt_opts(prior = list(mean = 1.5, sd = 0.5), rw = 7),
                        gp = NULL, horizon = 14)

hosp_unknown_case_forecast <- forecast_secondary(cases_to_hosp, case_forecast$estimates, all_dates = TRUE)

hosp_proj <- hosp_unknown_case_forecast[[2]]

hosp_proj$variable <- "reported_cases"

hosp_proj$type <- ifelse(hosp_proj$date <= max(data$date), "estimate", "forecast")

save(hosp_proj, file = "Data/short_term_hosp_proj.RData")
