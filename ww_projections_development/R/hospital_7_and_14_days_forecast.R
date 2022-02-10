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
source("R/create_7_14_days_forecast.R")

# load data
ott_covid_data <- read.csv(file.path(getwd(), "Data/Observed data/OPH_Observed_COVID_Data.csv"))


# Set reporting delay, generation time, incubation period for simulation

generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

end_date = as.Date("2021-01-31")
delay_intervals <- c(4, 10, 14)
hosp_data_forecast <- list()
for (delay in delay_intervals){
  x <- 0
  while(end_date <= as.Date("2022-01-20")){
    x <- x + 1
    hosp_forecast <- short_term_forecast(
      data = ott_covid_data,
      input = "observed_census_ICU_p_acute_care",
      start_date = as.Date("2021-07-20") + 1,
      end_date = as.Date("2021-12-20") + 1,
      input_multiplier = 1,
      omit_last_date = TRUE,
      generation_time = generation_time,
      incubation_period = incubation_period,
      reporting_delay = bootstrapped_dist_fit(rlnorm(100, log(delay), 1), max_value = 30),
      output = "projections"
    ) %>%
      filter(variable == "reported_cases",
             type == "forecast")
    
    ott_data <- ott_covid_data %>%
      mutate(date = as.Date(date)) %>%
      select(date, observed_census_ICU_p_acute_care)
    hosp_data_forecast[[paste0("reporting_delay_",delay)]][[x]] <- ott_data %>%
      full_join(hosp_forecast) %>%
      filter(duplicated(date) == FALSE,
             date > first(date),
             !is.na(variable)) %>%
      arrange(date)
  }
}

hosp_forecast_7_14_days = list()
for (delay in delay_intervals){
  hosp_forecast_7_14_days[[paste0("reporting_delay_",delay)]] = create_7_14_days_forecast(hosp_data_forecast[[paste0("reporting_delay_",delay)]])
}


save(hosp_data_forecast, file = "ww_projections_development/Data/ottawa_historic_hosp_proj.RData")
