# Extract data frame from JSON object
data_extract <- function(object){
  name_for_features <- "features"
  dataframe <- object[[name_for_features]][[1]]
  if (is.numeric(dataframe[[1]])) {
    # Convert Unix time to Date
    dataframe[[1]] <- as.Date(as.POSIXct(dataframe[[1]] / 1000, origin = "1970-01-01"))
  } else if (is.character(dataframe[[1]])) {
    # Handle date strings
    dataframe[[1]] <- dataframe[[1]] %>%
      # Convert 'yyyy-mm-dd' format directly to Date
      ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", .), as.Date(.),
             # Convert 'yy-mm-dd' format to 'yyyy-mm-dd' by adding '20' prefix
             ifelse(grepl("^\\d{2}-", .), 
                    str_replace(., "^", "20") %>% as.Date(),
                    . %>% as.Date())) %>%
      strtrim(8) %>%
  }
  return(dataframe)
}

# Extract adjusted cases by episode date
adjusted_function <- function(url) {
  # Read the CSV file and select relevant columns
  adjusted_data <- read.csv(url) %>%
    select(Date, Nowcasting.Adjusted.Cases.by.Episode.Date) %>%
    rename(
      date = "Date",
      adjusted_episode_cases = "Nowcasting.Adjusted.Cases.by.Episode.Date"
    )
  # Handle date conversion
  adjusted_data <- adjusted_data %>%
    mutate(
      date = ifelse(
        grepl("^\\d{4}-\\d{2}-\\d{2}$", date),  # If date is in yyyy-mm-dd format
        as.Date(date),
        ifelse(
          grepl("^\\d{2}-", date),  # If date starts with two digits (yy format)
          date %>%
            str_replace("^", "20") %>%  # Add "20" prefix to make it yyyy-mm-dd
            as.Date(),
          as.Date(date)  # For any other format
        )
      )
    )
  return(adjusted_data)
}

# Clean and merge JSON objects as data frame
data_creation <- function(ottawa_case_data, ottawa_test_data){
  # Prep Ottawa testing data
  test_prep <- function(ottawa_test_data) {
    ottawa_test <- data_extract(ottawa_test_data)
    ottawa_test %>%
      select(-OBJECTID) %>%
      rename(
        date = "Date",
        num_non_ltch_tests = "Number_of_Tests,_Excluding_LTCH",
        non_ltch_pct_positivity = "Daily_%_Positivity,_Excluding_LTCH",
        num_ltch_tests = "Number_of_Tests_in_LTCH",
        ltch_pct_positivity = "LTCH_Daily_%_Positivity"
      ) %>%
      mutate(
        non_ltch_pct_positivity = non_ltch_pct_positivity/100,
        ltch_pct_positivity = ltch_pct_positivity/100,
        observed_pct_positivity = (
          (num_non_ltch_tests*non_ltch_pct_positivity) +
            (num_ltch_tests * ltch_pct_positivity)
        )/(num_non_ltch_tests + num_ltch_tests)*100
      ) %>%
      filter(date >= "2020-03-17") %>%
      # add moving average for pct positivity
      mutate(
        pct_positivity_7_day =
          rollapply(observed_pct_positivity, width=7,
                    FUN=function(x) mean(x, na.rm=TRUE),
                    by=1, by.column=TRUE, partial=TRUE,
                    fill=NA, align="right")
      )
  }
  ottawa_test <- test_prep(ottawa_test_data)
  
  # Prep Ottawa case data & merge with testing data
  case_prep <- function(ottawa_case_data, ottawa_test){
    ottawa_data <- data_extract(ottawa_case_data)
    ottawa_data %>%
      replace(is.na(.),0) %>%
      rename(
        date = "Date",
        observed_new_ICU_p_acute_care = "Cases_Newly_Admitted_to_Hospita",
        observed_census_ICU_p_acute_care = "Cases_Currently_in_Hospital",
        observed_census_ICU = "Cases_Currently_in_ICU",
        observed_cumulative_deaths = "Cumulative_Deaths_by_Date_of_De",
        observed_cumulative_cases = "Cumulative_Cases_by_Episode_Dat",
        observed_new_cases = "Daily_Cases_by_Reported_Date",
        observed_new_episodes = "Daily_Cases_by_Episode_Date",
        observed_active_cases = "Total_Active_Cases_by_Date",
        observed_daily_average_cases =
          "F7_day_Average_of_Newly_Reporte",
        observed_new_cases_community_outbreak =
          "Daily_Cases_Linked_to_a_Communi",
        observed_new_cases_institution_outbreak =
          "Daily_Cases_Linked_to_an_Instit",
        observed_new_cases_sporadic =
          "Daily_Cases_Not_Linked_to_an_Ou"
      ) %>%
      arrange(date) %>%
      mutate(observed_census_acute_care =
               observed_census_ICU_p_acute_care - observed_census_ICU,
             observed_new_deaths = 
               as.numeric(observed_cumulative_deaths) - lag(as.numeric(observed_cumulative_deaths)),
             cumulative_hosp =
               cumsum(observed_new_ICU_p_acute_care),
             observed_new_community_cases =
               observed_new_cases_community_outbreak +
               observed_new_cases_sporadic) %>%
      full_join(ottawa_test, by = "date") %>%
      select(date, everything())
  }
  ottawa_data <- case_prep(ottawa_case_data, ottawa_test)
  
  # Run linear regression to generate multiplier value
  regression <- function(ottawa_data){
    ott_regression <- lm(
      observed_new_cases ~ 0 + observed_new_community_cases + # set y-intercept to 0
        observed_new_community_cases*observed_pct_positivity,
      data = ottawa_data
    )
    return(ott_regression)
  }
  ott_regression <- regression(ottawa_data)
  
  alpha <- (ott_regression[[1]][[3]]/ott_regression[[1]][[1]])
  multiplier <- 1 + (alpha*ottawa_data[["observed_pct_positivity"]])
  
  # Use multiplier value to generate adjusted new cases & active cases
  adjusted <- function(ottawa_data, alpha, multiplier){
    ottawa_data %>%
      mutate(
        adjusted_new_episodes = (multiplier*observed_new_community_cases) + 
          observed_new_cases_institution_outbreak,
        adjusted_active_cases = (multiplier*observed_active_cases)
      )
  }
  ottawa_data <- adjusted(ottawa_data, alpha, multiplier)
  list <- list(ottawa_data, ott_regression, alpha, multiplier)
  names(list) <- c("data", "regression", "alpha", "multiplier")
  return(list)
}
