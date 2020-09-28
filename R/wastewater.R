# Script to clean wastewater data
wastewater_prep <- function(data){
  # Rename gene headers 
  names(data)[1] <- "date"
  names(data)[2] <- "N1"
  names(data)[3] <- "N1_stdev"
  names(data)[4] <- "N2"
  names(data)[5] <- "N2_stdev"
  names(data)[6] <- "N1_N2"
  names(data)[7] <- "rolling_avg"
  names(data)[8] <- "pct_change"
  # Remove rows beyond current date
  data %>%
    mutate(date = as.Date(parse_date_time(date, c('dm', 'dmy')))) %>%
    filter(!is.na(date))
}

# Script to merge wastewater data into covid data
merge_data <- function(data1, data2){
  data1 %>%
    full_join(data2, by = "date") %>%
    filter(date >= "2020-04-08")
}