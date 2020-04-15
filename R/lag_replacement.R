
cal_expected_value <- function(default, rate, len){
  out <- numeric()
  out[1] <- default
  for (i in 2:(len+1)) {
    out[i] <- out[i-1]+(out[[i-1]] * rate)
  }
  return(out)
}

calc_rate_of_increase <- function(input){
  out <- numeric()
  for (i in 2:length(input)) {
    out[i-1] <- ((input[i]-input[i-1])/input[i-1])
  }
  return(out)
}

calc_mean_rate_of_increase <- function(input){
  out <- numeric()
  for (i in 1:length(input)) {
    out[i] <- mean(input[1:i])
  }
  return(out)
}

calc_doubling_time <- function(observed_data){
  total_time <- (length(observed_data)-1)
  out <- ((total_time*log(2))/(log(observed_data[[total_time+1]]/observed_data[[1]])))
  return(out)
}

calc_expected_values_for_n_weeks <- function(data, number_weeks = 1){
  all_days <- na.omit(data[, "observed_census_ICU_p_acute_care"])
  # Numeric representation of all_days[[1]] as a day of the week
  first_day <- 2
  # TODO Use dates to verify no skipped days
  start_of_first_full_week_index <- (7 - first_day)+2
  max_number_of_weeks <- floor((length(all_days)-(start_of_first_full_week_index-1))/7)
  if (number_weeks > max_number_of_weeks){
    stop(paste("The requested number of weeks is above the maximum number of complete weeks. The last incomplete week starts at row number:", start_of_first_full_week_index+(7*max_number_of_weeks)))
  }
  
  start_of_calculation_week <- ifelse(number_weeks!=max_number_of_weeks, start_of_first_full_week_index + 7*(max_number_of_weeks - number_weeks),start_of_first_full_week_index)
  doubling_time <- list()
  for (week in seq(number_weeks)) {
    # Convert to number of weeks to add
    weeks_to_add <- week-1
    start_of_calculation_week <- start_of_calculation_week + 7*weeks_to_add
    
    observed_input_for_week <- all_days[start_of_calculation_week:(start_of_calculation_week+6)]
    rate_of <- calc_rate_of_increase(observed_input_for_week)
    data[(start_of_calculation_week+1):(start_of_calculation_week+6),"rate_of_increase_mar26_onward"] <-rate_of
    
    daily_mean_rate <- calc_mean_rate_of_increase(rate_of)
    data[(start_of_calculation_week+1):(start_of_calculation_week+6),"mean_daily_rate_of_increase_mar26_onward"] <- daily_mean_rate
    
    expected_value <- cal_expected_value(all_days[[start_of_calculation_week]], last(daily_mean_rate), length(daily_mean_rate))
    data[start_of_calculation_week:(start_of_calculation_week+6),"expected_val_mar26_onward"]
    
    doubling_time[[week]] <- calc_doubling_time(observed_input_for_week)
  }

  return(list(data,doubling_time))
}

# To use
#  tmp <- calc_expected_values_for_n_weeks(ott_observed, number_weeks = 2)
# doubling_time <- tmp[[2]]
# new_data <- tmp[[1]]

