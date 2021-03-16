# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

library(ggplot2)
library(EpiNow2)

# Data Prep
data <- read.csv(file.path(getwd(),"Data/Observed data/OPH_Observed_COVID_Data.csv")) %>%
  select(date, observed_new_cases, observed_census_ICU_p_acute_care) %>%
  mutate(date = as.Date(date)) %>%
  rename(primary = "observed_new_cases", secondary = "observed_census_ICU_p_acute_care") %>%
  filter(date >= (max(as.Date(date)) - 12*7))

train_data <- data %>%
  filter(date < (max(as.Date(date)) - 7*2))

test_data <- data %>%
  filter(date >= (max(as.Date(date)) - 7*2))

train_data <- data.table::setDT(train_data)
test_data <- data.table::setDT(test_data)

# Estimate relationship between cases and hospitalization
cases_to_hosp <- estimate_secondary(train_data, 
                                    delays = delay_opts(list(mean = 2.5, mean_sd = 0.2, 
                                                               sd = 0.47, sd_sd = 0.1, max = 21)),
                                    secondary = secondary_opts(type = "prevalence"),
                                    obs = obs_opts(scale = list(mean = 0.01, sd = 0.0025)),
                                    control = list(adapt_delta = 0.95))

# Forecast using relationship and test data
hosp_forecast <- forecast_secondary(cases_to_hosp, copy(test_data)[, .(date, value = primary)])