# This function creates a dataset that pulls data from historic projections
# and combines it with observed data. Function defaults to pull 7th and 14th day
# projections, but users can select other dates. Function defaults to median as
# projection variable of interest, but users can change to mean. 

create_historic_forecast_dataset <- function(data,
                                      days = c(7, 14),
                                      projection_variable = "median",
                                      ci = 90,
                                      obs_column){
  
  for (x in 1:length(data)){
    data[[x]] <- data[[x]] %>%
      select(date, projection_variable, obs_column, sd, paste0("lower_", ci),
             paste0("upper_", ci))
    daily_proj <- list()
    i <- 0
    for(day in unique(days)){
      i <- i + 1
      daily_proj[[i]] <- data[[x]][day,] %>%
        rename_with(~paste0(.,paste0("_day_", day)), .cols = -c("date", obs_column))
    }
    data[[x]] <- bind_rows(daily_proj)
  }
  forecast_dataset <- bind_rows(data) %>%
    group_by(date) %>%
    summarise_all(list(~ .[!is.na(.)][1])) %>%
    arrange(date) %>%
    relocate(date, obs_column)
  return(as.data.frame(forecast_dataset))
}