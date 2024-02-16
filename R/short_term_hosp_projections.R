Sys.setlocale("LC_ALL","English")
library(EpiNow2)
library(lubridate)
library(tidyverse)
source("R/epinow_functions.R")

# CONFIGURATION VARIABLES START
# The next two variables configures the rolling window for the short term 
# hospital projections
# Without the rolling window the hospital projections take too long to complete
# The variables are used to set the start date for the projections. For example,
# If we want the start date to be 1 month before, time_to_set_back would be -1
# and measure_to_set_back would be "months"
time_to_set_back <- -7 
measure_to_set_back <- "months"
# CONFIGURATION VARIABLES END


# load covid data
ott_covid_data <-
  read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))

# Set reporting delay, generation time, incubation period
reporting_delay <- NULL
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# Generate start date from end date
#end_date <- as.Date(last(ott_covid_data$date))
#end_date <- as.Date(tail(ott_covid_data$date))[6]
end_date <- lubridate::add_with_rollback(as.Date(tail(ott_covid_data$date))[6],weeks(-2))
#start_date <- seq(
#  as.Date(end_date), 
#  length = 2, 
#  by = paste(time_to_set_back, measure_to_set_back)
#)[2]
#as.Date(as.Date(end_date)) %m-% months(7)
start_date <- lubridate::add_with_rollback(as.Date(as.Date(end_date)), months(-7))

print("Printing relevant variables for debugging:")
print(start_date)
print(end_date)

# Run epinow forecast
hosp_projections <- short_term_forecast(
  data = ott_covid_data,
  input = "observed_census_ICU_p_acute_care",
  start_date = start_date,
  # end_date = "2020-11-24", # can be changed, if missing will default to last day
  omit_last_date = TRUE,
  generation_time = generation_time,
  incubation_period = incubation_period,
  reporting_delay = reporting_delay,
  output = "both"
)

hosp_proj <- hosp_projections[[1]]

save(hosp_proj, file = "Data/short_term_hosp_proj.RData")
save(hosp_projections, file = "Data/hosp_projections.RData")
save(hosp_proj, file = paste(
  paste("Data/Historic Projections/short_term_hosp_proj", Sys.Date(), sep = "_"),
  ".RData", sep = ""))
write.csv(hosp_proj, file = paste(
  paste("Data/Historic Projections/short_term_hosp_proj", Sys.Date(), sep = "_"),
  ".csv", sep = ""), row.names = FALSE)

print("Printing relevant variables for debugging:")
print(hosp_projections)

