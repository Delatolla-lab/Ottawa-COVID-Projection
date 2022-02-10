create_7_14_days_forecast <- function(data){
  median_7_vec = c()
  median_14_vec = c()
  observed_7_vec = c()
  observed_14_vec = c()
  for (x in length(data)){
    for(i in 1:nrow(data[[x]])) {  
      # for-loop over rows
        if (i == 7){
          median_7_vec <- c(median_7_vec, data[[x]][i, median])
          observed_7_vec <- c(observed_7_vec, data[[x]][i, observed_census_ICU_p_acute_care])
        }
        else if (i == 14){
          median_14_vec <- c(median_14_vec, data[[x]][i, median])
          observed_14_vec <- c(observed_14_vec, data[[x]][i, observed_census_ICU_p_acute_care])
        }
      }
  }
  forecast_7_14_data = data.frame("7_days_forecast" = median_7_vec, "14_days_forecast" = median_14_vec,
                                  "observed_7_days" = observed_7_vec, "observed_14_days" = observed_14_vec)
  return(forecast_7_14_data)
  
}