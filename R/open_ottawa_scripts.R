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
  
}
