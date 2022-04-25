library(tidyverse)
library(ggplot2)
library(EpiNow2)
library(stringr)
library(purrr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(zoo)
source("R/epinow_functions.R")
source("R/wastewater.R")
source("R/historic_forecasts.R")

# load data
ott_covid_data <- read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))


# Set reporting delay, generation time, incubation period for simulation

generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

start_date <- as.Date("2021-07-20")
end_date = as.Date("2021-12-20")
case_data_forecast <- list()
x <- 0
while(end_date <= as.Date("2022-01-20")){
  x <- x + 1
  case_forecast <- short_term_forecast(
    data = ott_covid_data,
    input = "observed_new_cases",
    start_date = start_date,
    end_date = end_date,
    input_multiplier = 1,
    omit_last_date = TRUE,
    generation_time = generation_time,
    incubation_period = incubation_period,
    reporting_delay = bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30),
    output = "projections"
  ) %>%
    filter(variable == "reported_cases",
           type == "forecast")
  
  ott_data <- ott_covid_data %>%
    mutate(date = as.Date(date)) %>%
    select(date, observed_new_cases)
  case_data_forecast[[x]] <- ott_data %>%
    full_join(case_forecast) %>%
    filter(duplicated(date) == FALSE,
           date > first(date),
           !is.na(variable)) %>%
    arrange(date)
  
  start_date <- start_date + 1
  end_date <- end_date + 1
}

case_data_forecast_7_14_days <-
  create_historic_forecast_dataset(case_data_forecast, 
                            obs_column = "observed_new_cases")

save(case_data_forecast_7_14_days,
     file = "ww_projections_development/Data/ottawa_historic_case_proj_7_14_days.RData")

