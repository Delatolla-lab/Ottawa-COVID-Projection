## This script runs the epinow2 short term projection

# load library, packages, functions
library(EpiNow2)
library(tidyverse)
source("R/epinow_functions.R")

# load covid data
ott_covid_data <-
  read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))

# Set reporting delay, generation time, incubation period
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# Generate start date from end date
end_date <- as.Date(last(ott_covid_data$date))
start_date <- seq(as.Date(end_date), length = 2, by = "-7 months")[2]

# Run epinow forecast
ott_short_forecast <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_new_cases",
  start_date = "2021-09-01", # can be changed
 # end_date = "2020-11-24", # can be changed, if missing will default to last day
  omit_last_date = TRUE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)

# Save projections as CSV 
ott_projections <- ott_short_forecast[[1]]
write.csv(ott_projections,
          file = "Data/short_term_forecast.csv", row.names = FALSE)
write.csv(ott_projections, file = paste(
  paste("Data/Historic Projections/short_term_forecast", Sys.Date(), sep = "_"),
                                        ".csv", sep = ""), row.names = FALSE)
save(ott_short_forecast, file = "Data/short_term_forecast.RData")
save(ott_projections, file = paste(
  paste("Data/Historic Projections/short_term_forecast", Sys.Date(), sep = "_"),
  ".RData", sep = ""))