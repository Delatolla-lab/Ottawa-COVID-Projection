library(dplyr)
library(zoo)
#load data
library(tidyverse)
library(forecast)
library(ggplot2)
library(EpiNow2)
library(stringr)
library(purrr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(readxl)
library(zoo)

library(RcppRoll)

# Load TOH Data
covid_data <- "Data/Observed data/DailyPos.xlsx"
#NOTE:
#MAKE SURE TO ALWAYS HAVE THE EXCEL FILE CLOSED WHEN TRYING TO OPEN IN RSTUDIO ELSE GET ZIP ERROR

covid_data <- read_excel(covid_data, sheet = "Data")
covid_data[is.na(covid_data)] = 0


dat2 <- data.frame(c_r=RcppRoll::roll_sum(covid_data$covidPosAbs ,11, fill=NA, align="right"))
covid_data$roll_point_11 <- dat2$c_r
cum_val <- data.frame(covid_data$covidPosAbs, c=cumsum(covid_data$covidPosAbs))

covid_data = as.data.frame(covid_data)
covid_data$roll_point_11[is.na(covid_data$roll_point_11)] = cum_val$c[is.na(covid_data$roll_point_11)]
covid_data$cummCovidPosAbs <- cum_val$c
covid_data <- covid_data %>% mutate(date = as.Date(sampleDate)) %>% select(-c(sampleDate)) %>% rename(activeCovidPosAbs = roll_point_11)

save(covid_data, file = paste0("Data/Observed data/cumulative_covid_absenteeism.RData"))
s

