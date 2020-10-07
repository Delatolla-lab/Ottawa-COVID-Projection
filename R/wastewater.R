# Script to clean wastewater data
wastewater_prep <- function(data){
  # Rename gene headers 
  names(data)[1] <- "date"
  names(data)[2] <- "N1"
  names(data)[3] <- "N1_stdev"
  names(data)[5] <- "N2"
  names(data)[6] <- "N2_stdev"
  names(data)[7] <- "N1_N2_avg"
  names(data)[8] <- "rolling_avg"
  names(data)[9] <- "pct_change"
  # Remove second date header
  data <- data[,-4]
  # Remove rows beyond current date
  data %>%
    mutate(date = as.Date(parse_date_time(date, c('dm', 'dmy')))) %>%
    filter(!is.na(date)) %>%
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
    filter(date >= "2020-04-08") %>%
    # create 10 day rolling avg of viral signal
    mutate(
      N1_N2_10_day =
        rollapply(N1_N2_avg, width=10,
                  FUN=function(x) mean(x, na.rm=TRUE),
                  by=1, by.column=TRUE, partial=TRUE,
                  fill=NA, align="center")
    ) %>%
    # create 10 day rolling avg of daily rate of change of viral signal
    mutate(
      avg_viral_roc_10_day =
        rollapply(viral_roc_daily, width=10,
                  FUN=function(x) mean(x, na.rm=TRUE),
                  by=1, by.column=TRUE, partial=TRUE,
                  fill=NA, align="center")
    ) %>%
    # create change in 10 day rolling avg from 10 day rolling avg 10 days ago
    mutate(
      change_N1_N2_10_day =
        (N1_N2_10_day - lag(N1_N2_10_day, 10))/lag(N1_N2_10_day, 10) * 100
    ) %>%
    # create 5 day rolling avg of reported new cases & active cases
    mutate(
      observed_new_cases_5_day =
        rollapply(
          observed_new_cases, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="right"),
      observed_active_cases_5_day =
        rollapply(
          observed_active_cases, width=5,
          FUN=function(x) mean(x, na.rm=TRUE),
          by=1, by.column=TRUE, partial=TRUE,
          fill=NA, align="right"),
    )
}