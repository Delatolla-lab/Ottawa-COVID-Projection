# tmp <- na.omit(ott_observed$observed_census_ICU_p_acute_care) 
# rate_change <- (tmp[[length(tmp)]]-tmp[[1]])/tmp[[1]]
# breh <- lag_didnt_wanna_work(ott_observed[10:nrow(ott_observed),0], tmp[[10]], "expected", rate_change)
# ott_observed[["expected"]] <- NA
# ott_observed[10:nrow(ott_observed), "expected"] <- breh
lag_didnt_wanna_work <- function(data, default, colname, rate){
  data[[colname]] <- default
  for (row_num in 2:nrow(data)) {
    data[row_num,colname] <- data[row_num-1,colname] * rate
  }
  return(data)
}