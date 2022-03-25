#' Calculate expected values
#' 
#' Internal function for calculating expected values based on passed rate
#' 
#' @param default the value to start the expected calculations from
#' @param rate of change for that period
#' @param len length of the period
#' 
#' @return a numeric vector containing calculated expected values
calc_expected_value <- function(default, rate, len){
  out <- numeric()
  out[1] <- default
  for (i in 2:(len+1)) {
    out[i] <- out[i-1]+(out[[i-1]] * rate)
  }
  return(out)
}


#' Calculate doubling time for given range
#' 
#' @param observed_data range of observed data to calculate doubling time on
#' 
#' @return numeric representing doubling time for passed range
calc_doubling_time <- function(observed_data){
  total_time <- (length(observed_data)-1)
  out <- ((total_time*log(2))/(log(observed_data[[total_time+1]]/observed_data[[1]])))
  return(out)
}

calc_growth <- function(doubling_time){
  growth <- (2^(1/doubling_time)) - 1
  
  return(growth)
}


calc_expected_values_for_n_weeks <- function(data, number_weeks = 1, observed_columns_name = "observed_census_ICU_p_acute_care", first_day = 2, date_column = "date"){
  all_days <- na.omit(data[, observed_columns_name])
  # Numeric representation of all_days[[1]] as a day of the week
  first_day <- first_day
  # TODO Use dates to verify no skipped days
  start_of_first_full_week_index <- (7 - first_day)+2
  #Validate Start Week
  while(is.na(data[start_of_first_full_week_index, observed_columns_name])){
    start_of_first_full_week_index <- start_of_first_full_week_index+7
  }
  data <- data[start_of_first_full_week_index:nrow(data),]
  start_of_first_full_week_index <- 1
  all_days <- na.omit(data[, observed_columns_name])
  max_number_of_weeks <- floor(length(all_days)/7)
  if (number_weeks > max_number_of_weeks){
    stop(paste("The requested number of weeks is above the maximum number of complete weeks. The last incomplete week starts at row number:", start_of_first_full_week_index+(7*max_number_of_weeks)))
  }
  
  start_of_calculation_week_standard <- ifelse(number_weeks!=max_number_of_weeks, start_of_first_full_week_index + 7*(max_number_of_weeks - number_weeks),start_of_first_full_week_index)
  doubling_time <- list()
  expected_out <- list()
  weekly_growth <- list()
  for (week in seq(number_weeks)) {
    # Convert to number of weeks to add
    weeks_to_add <- week-1
    start_of_calculation_week <- start_of_calculation_week_standard + 7*weeks_to_add
    
    observed_input_for_week <- all_days[start_of_calculation_week:(start_of_calculation_week+6)]
    #rate_of <- calc_rate_of_increase(observed_input_for_week)
    #data[(start_of_calculation_week+1):(start_of_calculation_week+6),"rate_of_increase"] <-rate_of
    
    #daily_mean_rate <- calc_mean_rate_of_increase(rate_of)
    #data[(start_of_calculation_week+1):(start_of_calculation_week+6),"mean_daily_rate_of_increase"] <- daily_mean_rate
    
    doubling_time[[week]] <- calc_doubling_time(observed_input_for_week)
    weekly_growth[[week]] <- calc_growth(doubling_time[[week]])
    
    expected_value <- calc_expected_value(all_days[[start_of_calculation_week]], weekly_growth[[week]], length(observed_input_for_week)-1)
    #data[start_of_calculation_week:(start_of_calculation_week+6),"expected_val"] <- expected_value
    
    # This is done to preserve row numbers
    expected_sub_data <- as.data.frame(data[,0])
    expected_sub_data[,"expected_val"] <- NA
    expected_sub_data[,date_column] <- NA
    expected_sub_data[start_of_calculation_week:(start_of_calculation_week+6),"expected_val"] <- expected_value
    expected_sub_data[start_of_calculation_week:(start_of_calculation_week+6),date_column] <- 
      as.character(data[start_of_calculation_week:(start_of_calculation_week+6),date_column])
    expected_out[[week]] <- expected_sub_data
    #doubling_time[[week]] <- calc_doubling_time(observed_input_for_week)
  }

  return(list(data, doubling_time, expected_out, weekly_growth))
}

# To use
#  tmp <- calc_expected_values_for_n_weeks(ott_observed, number_weeks = 2)
# doubling_time <- tmp[[2]]
# new_data <- tmp[[1]]
# ---------- DEPRICATED ----------

#' #' Calculate Rate of Increase
#' #' 
#' #' Calculate rate of increase between two days in the given range
#' #' 
#' #' @param input range of observed data to calculate rate of increase on
#' #' 
#' #' @return daily rate of change for the passed range with len of input-1
#' calc_rate_of_increase <- function(input){
#'   out <- numeric()
#'   for (i in 2:length(input)) {
#'     out[i-1] <- ((input[i]-input[i-1])/input[i-1])
#'   }
#'   return(out)
#' }
#' 
#' #' Calculate mean rate of increase
#' #'
#' #' Calculates running average of the passed range
#' #' 
#' #' @param input daily rate of increase range
#' #' 
#' #' @return vector containing means
#' calc_mean_rate_of_increase <- function(input){
#'   out <- numeric()
#'   for (i in 1:length(input)) {
#'     out[i] <- mean(input[1:i])
#'   }
#'   return(out)
#' }
