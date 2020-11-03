# Script to clean wastewater data
wastewater_prep <- function(data){
  # Rename gene headers 
  names(data)[1] <- "date"
  names(data)[5] <- "N1"
  names(data)[7] <- "N2"
  
  data %>%
    select(date, N1, N2) %>%
    # Remove rows beyond current date
    mutate(date = as.Date(parse_date_time(date, c('dm', 'dmy')))) %>%
    filter(!is.na(date)) %>%
    # Create mean value of N1 and N2
    mutate(N1_N2_avg = (N1 + N2)/2) %>%
    # create daily rate of change of viral signal
    mutate(
      viral_roc_daily = ((N1_N2_avg - lag(N1_N2_avg))/lag(N1_N2_avg))/
        (as.numeric(as_date(date)-lag(as_date(date))))
    )
}

# Script to merge wastewater data into covid data
merge_data <- function(data1, data2){
  data1 %>%
    full_join(data2, by = "date") %>%
    # create 5 day rolling avg of viral signal
    mutate(
      N1_N2_5_day =
        rollapply(N1_N2_avg, width=5,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="center")
    ) %>%
    # create 7 day rolling avg of viral signal
    mutate(
      N1_N2_7_day =
        rollapply(N1_N2_avg, width=7,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="center")
    ) %>%
    # create 10 day rolling avg of viral signal
    mutate(
      N1_N2_10_day =
        rollapply(N1_N2_avg, width=10,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="center")
    ) %>%
    # create 10 day rolling avg of daily rate of change of viral signal
    mutate(
      avg_viral_roc_10_day =
        rollapply(viral_roc_daily, width=10,
                  FUN=function(x) mean(x),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="center")
    ) %>%
    # create change in 5 day rolling avg from 5 day rolling avg 5 days ago
    mutate(
      change_N1_N2_5_day =
        (N1_N2_5_day - lag(N1_N2_5_day, 5))/lag(N1_N2_5_day, 5) * 100
    ) %>%
    # create change in 7 day rolling avg from 7 day rolling avg 5 days ago
    mutate(
      change_N1_N2_7_day =
        (N1_N2_7_day - lag(N1_N2_7_day, 5))/lag(N1_N2_7_day, 5) * 100
    ) %>%
    # create change in 10 day rolling avg from 10 day rolling avg 10 days ago
    mutate(
      change_N1_N2_10_day =
        (N1_N2_10_day - lag(N1_N2_10_day, 10))/lag(N1_N2_10_day, 10) * 100
    ) %>%
    # create 5, 7, 10 day rolling avgs of reported new cases, new community cases, & active cases
    mutate(
      observed_new_cases_5_day =
        rollapply(
          observed_new_cases, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_cases_7_day =
        rollapply(
          observed_new_cases, width=7,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_cases_10_day =
        rollapply(
          observed_new_cases, width=10,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_episodes_5_day =
        rollapply(
          observed_new_episodes, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_episodes_7_day =
        rollapply(
          observed_new_episodes, width=7,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_episodes_10_day =
        rollapply(
          observed_new_episodes, width=10,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_community_cases_5_day =
        rollapply(
          observed_new_community_cases, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_community_cases_7_day =
        rollapply(
          observed_new_community_cases, width=7,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_new_community_cases_10_day =
        rollapply(
          observed_new_community_cases, width=10,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_active_cases_5_day =
        rollapply(
          observed_active_cases, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_active_cases_7_day =
        rollapply(
          observed_active_cases, width=7,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center"),
      observed_active_cases_10_day =
        rollapply(
          observed_active_cases, width=10,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="center")
    ) %>%
    # create change in 5 day rolling avg of new cases from 5 day rolling avg 5 days ago
    mutate(
      change_new_cases_5_day =
        (observed_new_cases_5_day -
           lag(observed_new_cases_5_day, 5))/lag(observed_new_cases_5_day, 5) *
        100
    ) %>%
    # create change in 7 day rolling avg of new cases from 7 day rolling avg 7 days ago
    mutate(
      change_new_cases_7_day =
        (observed_new_cases_7_day -
           lag(observed_new_cases_7_day, 7))/lag(observed_new_cases_7_day, 7) *
        100
    ) %>%
    # create change in 7 day rolling avg of new cases from 10 day rolling avg 10 days ago
    mutate(
      change_new_cases_10_day =
        (observed_new_cases_10_day -
           lag(observed_new_cases_10_day, 10))/lag(observed_new_cases_10_day, 10) *
        100
    )
}