# Pull JSON objects
ottawa_case_data <-
  fromJSON("https://opendata.arcgis.com/datasets/6bfe7832017546e5b30c5cc6a201091b_0/FeatureServer/0/query?where=1%3D1&outFields=_Date,Cumulative_Active_Cases_by_Episode_Date,Cumulative_Deaths_by_Date_of_Death,7-day_Average_of_Newly_Reported_cases_by_Reported_Date,Cases_Newly_Admitted_to_Hospital,Cases_Currently_in_Hospital,Cases_Currently_in_ICU,Daily_Cases_by_Episode_Date,Daily_Cases_by_Reported_Date&outSR=4326&f=json")
ottawa_test_data <-
  fromJSON("https://opendata.arcgis.com/datasets/26c902bf1da44d3d90b099392b544b81_0/FeatureServer/0/query?where=1%3D1&outFields=_Date,Daily_%_Positivity&outSR=4326&f=json")

# Clean and merge JSON objects as data frame
data_creation <- function(ottawa_case_data, ottawa_test_data){
  ottawa_test <- ottawa_test_data[[8]][[1]] %>%
    rename(
      date = "_Date",
      observed_pct_positivity = "Daily_%_Positivity"
    ) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "2020-03-17")
  
  ottawa_data <- ottawa_case_data[[8]][[1]]%>%
    replace(is.na(.),0) %>%
    rename(
      date = "_Date",
      observed_new_ICU_p_acute_care = "Cases_Newly_Admitted_to_Hospital",
      observed_census_ICU_p_acute_care = "Cases_Currently_in_Hospital",
      observed_census_ICU = "Cases_Currently_in_ICU",
      observed_cumulative_deaths = "Cumulative_Deaths_by_Date_of_Death",
      observed_new_cases = "Daily_Cases_by_Reported_Date",
      observed_new_episodes = "Daily_Cases_by_Episode_Date",
      observed_active_cases = "Cumulative_Active_Cases_by_Episode_Date",
      observed_daily_average_cases =
        "7-day_Average_of_Newly_Reported_cases_by_Reported_Date"
    ) %>%
    arrange(date) %>%
    mutate(date = as.Date(date), 
           observed_census_acute_care =
             observed_census_ICU_p_acute_care - observed_census_ICU,
           observed_new_deaths = 
             observed_cumulative_deaths - lag(observed_cumulative_deaths)) %>%
    filter(date >= "2020-03-17") %>%
    full_join(ottawa_test, by = "date")
  
  # Save as csv file
  write.csv(ottawa_data,
        "../../Data/Observed data/Ottawa_Observed_COVID_Hospital_Use_test.csv")
}
  

