library(jsonlite)
library(dplyr)

open_ottawa_raw <-
  fromJSON("https://opendata.arcgis.com/datasets/6bfe7832017546e5b30c5cc6a201091b_0/FeatureServer/0/query?where=1%3D1&outFields=_Date,Cumulative_Deaths_by_Date_of_Death,Cases_Newly_Admitted_to_Hospital,Cases_Currently_in_Hospital,Cases_Currently_in_ICU,Daily_Cases_by_Reported_Date,7-day_Average_of_Newly_Reported_cases_by_Reported_Date&outSR=4326&f=json")

ottawa_data <- open_ottawa_raw[[8]][[1]] %>%
  rename(Date = "_Date") %>%
  arrange(Date)
  

