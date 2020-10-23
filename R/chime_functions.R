# Functions to pull hospitalization data from Open Ottawa's API to be used
# in the CHIME-SIMS simulation

## Import open Ottawa hospitalization data & export to Python folder for
## simulation
hosp_export <- function(json_address, dest){
  object <- jsonlite::fromJSON(json_address)
  data <- data_extract(object) %>%
    rename(
      date = "_Date",
      hosp = "Cases_Currently_in_Hospital"
    )
  data$vent <- NULL
  data$mort <- NULL
  write.csv(data, dest)
}