short_term_forecast <- function(data,
                                generation_time,
                                incubation_period,
                                reporting_delay){
  # Format dataset
  data_formatted <- data %>%
    select(date, observed_new_episodes) %>%
    rename(confirm = observed_new_episodes) %>%
    mutate(date = as.Date(date))
  # Split data into month blocks
  data_split <- split(data_formatted, format(data_formatted$date,"%Y-%m"))
  # Run epinow2 sim in month blocks
  projections <- vector(mode = "list", length = length(data_split))
  for(i in 1:length(data_split)){
    projections[[i]] <-
      epinow(reported_cases = data_split[[i]], 
             generation_time = generation_time,
             delays = list(incubation_period, reporting_delay))
    projections[[i]] <-
      projections[[i]][[1]][[2]] %>% # Obtain summarized estimates
      filter(variable == "infections") # Filter for infections by episode date
    if(i < length(data_split)){ # remove partial/forecast estimates from earlier data
      projections[[i]] <-
        filter(projections[[i]], type == "estimate") 
    }
  }
  # Bind list of datasets into one dataframe
  projections <- bind_rows(projections) 
  return(projections)
}