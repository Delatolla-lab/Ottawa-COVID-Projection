# Script to clean wastewater data
wastewater_prep <- function(data){
  # Drop blank rows & columns
  data <- data[-c(1:4),-11]
  # Set appropriate column headers
  colnames(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  # Rename gene headers 
  names(data)[3] <- "N1"
  names(data)[7] <- "N2"
  names(data)[10] <- "N1_N2"
  names(data)[11] <- "rolling_avg"
  # Remove redundant columns
  data <- data[,-c(2, 4:6, 8:9, 12)]
  # Remove rows beyond current date
  data %>%
    rename(date = "Sample Date") %>%
    mutate(date = as.Date(parse_date_time(date, c('dm', 'dmy')))) %>%
    filter(!is.na(date))
}

# Script to merge wastewater data into covid data
merge_data <- function(data1, data2){
  data1 %>%
    full_join(data2, by = "date")
}