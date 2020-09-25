# Clean and merge JSON objects as data frame
data_creation <- function(ottawa_case_data, ottawa_test_data){
  
  # Prep Ottawa testing data
  test_prep <- function(ottawa_test_data) {
    ottawa_test <- ottawa_test_data[[8]][[1]]
    ottawa_test %>%
      rename(
        date = "_Date",
        observed_pct_positivity = "Daily_%_Positivity"
      ) %>%
      mutate(date = as.Date(date),
             observed_pct_positivity = observed_pct_positivity/100) %>%
      filter(date >= "2020-03-17")
  }
  ottawa_test <- test_prep(ottawa_test_data)
  
  # Prep Ottawa case data & merge with testing data
  case_prep <- function(ottawa_case_data, ottawa_test){
    ottawa_data <- ottawa_case_data[[8]][[1]]
    ottawa_data %>%
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
          "7-day_Average_of_Newly_Reported_cases_by_Reported_Date",
        observed_new_cases_community_outbreak =
          "Daily_Cases_Linked_to_a_Community_Outbreak_by_Episode_Date",
        observed_new_cases_institution_outbreak =
          "Daily_Cases_Linked_to_an_Institutional_Outbreak_by_Episode_Date",
        observed_new_cases_sporadic =
          "Daily_Cases_Not_Linked_to_an_Outbreak_ie_Sporadic_Cases_by_Episode_Date"
      ) %>%
      arrange(date) %>%
      mutate(date = as.Date(date), 
             observed_census_acute_care =
               observed_census_ICU_p_acute_care - observed_census_ICU,
             observed_new_deaths = 
               observed_cumulative_deaths - lag(observed_cumulative_deaths),
             cumulative_hosp =
               cumsum(observed_new_ICU_p_acute_care),
             observed_new_community_cases =
               observed_new_cases_community_outbreak +
               observed_new_cases_sporadic) %>%
      filter(date >= "2020-03-17") %>%
      full_join(ottawa_test, by = "date") %>%
      select(date, everything())
  }
  ottawa_data <- case_prep(ottawa_case_data, ottawa_test)
  
  # Run linear regression to generate multiplier value
  regression <- function(ottawa_data){
    ott_regression <- lm(
      cumulative_hosp ~ 0 + observed_new_community_cases + # set y-intercept to 0
        observed_new_community_cases*observed_pct_positivity,
      data = ottawa_data
    )
    alpha <- (ott_regression[[1]][[3]]/ott_regression[[1]][[1]])
    multiplier <- 1 + (alpha*ottawa_data[["observed_pct_positivity"]])
    ottawa_data[["alpha"]] <- alpha
    ottawa_data[["multiplier"]] <- multiplier
    return(ottawa_data)
  }
  ottawa_data <- regression(ottawa_data)
  
  # Use multiplier value to generate adjusted new cases & active cases
  adjusted <- function(ottawa_data){
    ottawa_data %>%
      mutate(
        adjusted_new_episodes = (multiplier*observed_new_community_cases) + 
          observed_new_cases_institution_outbreak,
        adjusted_active_cases = (multiplier*observed_active_cases)
      )
  }
  ottawa_data <- adjusted(ottawa_data)
  return(ottawa_data)
}
