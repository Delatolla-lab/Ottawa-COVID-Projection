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

ww_data <-
  read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv")

ww_clean <- wastewater_prep(ww_data) %>%
  select(date, N1_N2_avg_clean) %>%
  mutate(date = as.Date(date))

# Set reporting delay, generation time, incubation period for simulation
reporting_delay <- NULL
generation_time <-
  get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <-
  get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

date_intervals <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)

ww_data <- list()
x <- 0

for(date in date_intervals){
  x <- x + 1
  ww_forecast <- short_term_forecast(
    data = ww_clean,
    input = "N1_N2_avg_clean",
    start_date = as.Date("2020-10-01") + date,
    end_date = as.Date("2021-03-31") + date,
    input_multiplier = 1000000,
    omit_last_date = TRUE,
    generation_time = generation_time,
    incubation_period = incubation_period,
    reporting_delay = reporting_delay,
    output = "projections"
  ) %>%
    filter(variable == "reported_cases",
           type == "forecast")
  
  ww_data[[x]] <- ww_clean %>%
    mutate(N1_N2_avg = N1_N2_avg*1000000) %>%
    full_join(ww_forecast) %>%
    filter(duplicated(date) == FALSE,
           date > first(date),
           !is.na(variable)) %>%
    arrange(date)
}

save(ww_data, file = "ww_projections_development/historic_ww_proj.RData")