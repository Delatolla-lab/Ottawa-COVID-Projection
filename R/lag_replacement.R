
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
  total_time <- length(observed_data)
  out <- ((total_time*log(2))/(log(observed_data[[total_time]]/observed_data[[1]])))
  return(out)
}

# Temporary dataset starting at March 26
tmp <- na.omit(ott_observed[10:nrow(ott_observed), "observed_census_ICU_p_acute_care"])

rate_of <- calc_rate_of_increase(tmp)

# Create rate of increase column
ott_observed$rate_of_increase_mar26_onward <- 0
ott_observed[11:(11+length(rate_of)-1),"rate_of_increase_mar26_onward"] <-rate_of 
daily_mean_rate <- calc_mean_rate_of_increase(rate_of)
ott_observed$mean_daily_rate_of_increase <- 0
ott_observed[11:(11+length(daily_mean_rate)-1),"mean_daily_rate_of_increase_mar26_onward"] <- daily_mean_rate

# Object for initial observed value at March 26
day1 <- ott_observed[10,"observed_census_ICU_p_acute_care"]
expected_value <- cal_expected_value(day1, last(daily_mean_rate), length(daily_mean_rate))

# Column for expected values
ott_observed$expected_val_mar26_onward <- NA
ott_observed[10:(10+length(expected_value)-1),"expected_val_mar26_onward"] <- expected_value

#Doubling Time
observed_date <- na.omit(ott_observed[10:nrow(ott_observed), "observed_census_ICU_p_acute_care"])
doubling_time <- calc_doubling_time(observed_date)

# INSERT INTO INDEX.RMD 
# for testing

# ```{r}
# ott_observed <- read.csv(file.path(getwd(), "../Data/Observed data/Ottawa_Observed_COVID_Hospital_Use.csv"))
# tmp <- na.omit(ott_observed[10:nrow(ott_observed), "observed_census_ICU_p_acute_care"])
# rate_of <- calc_rate_of_increase(tmp)
# ott_observed$rate_of_increase_mar26_onward <- NA
# ott_observed[11:(11+length(rate_of)-1),"rate_of_increase_mar26_onward"] <-rate_of 
# 
# daily_mean_rate <- calc_mean_rate_of_increase(rate_of)
# ott_observed$mean_daily_rate_of_increase <- NA
# ott_observed[11:(11+length(daily_mean_rate)-1),"mean_daily_rate_of_increase_mar26_onward"] <- daily_mean_rate
# 
# day1 <- ott_observed[10,"observed_census_ICU_p_acute_care"]
# expected_value <- cal_expected_value(day1, last(daily_mean_rate), length(daily_mean_rate))
# ott_observed$expected_val_mar26_onward <- NA
# ott_observed[10:(10+length(expected_value)-1),"expected_val_mar26_onward"] <- expected_value
# View(ott_observed[,c("expected_val_mar26_onward", "mean_daily_rate_of_increase_mar26_onward", "rate_of_increase_mar26_onward")])
# 
# ```


