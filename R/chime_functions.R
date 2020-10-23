# Functions to pull hospitalization data from Open Ottawa's API to be used
# in the CHIME-SIMS simulation

## Import open Ottawa hospitalization data & export to Python folder for
## simulation
hosp_export <- function(json_address, starting_date, dest){
  object <- jsonlite::fromJSON(json_address)
  data <- data_extract(object) %>%
    rename(
      date = "_Date",
      hosp = "Cases_Currently_in_Hospital"
    ) %>%
    add_column(vent = "",
               mort = "") %>%
    filter(date >= as.character(starting_date))
  write.csv(data, dest, row.names = FALSE)
}

