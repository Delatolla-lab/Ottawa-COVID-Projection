library(EpiNow2)
library(tidyverse)
source("R/epinow_functions.R")

# Date setback variables
time_to_set_back <- -7 
measure_to_set_back <- "months"


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
end_date <- as.Date(last(ott_covid_data$date))
start_date <- seq(as.Date(end_date), length = 2, by = paste(time_to_set_back, measure_to_set_back))[2]

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
