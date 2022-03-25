# Functions to pull hospitalization data from Open Ottawa's API to be used
# in the CHIME-SIMS simulation.

## Import open Ottawa hospitalization data & export to Python folder for
## simulation.
hosp_export <- function(json_address, starting_date, dest){
  object <- jsonlite::fromJSON(json_address)
  data <- data_extract(object) %>%
    rename(
      date = "Date",
      hosp = "Cases_Currently_in_Hospital"
    ) %>%
    add_column(vent = "",
               mort = "") %>%
    filter(date >= as.character(starting_date))
  write.csv(data, dest, row.names = FALSE)
}

## Integrate output from CHIME simulation with existing hosp_projection file
hosp_integrate <- function(old_proj, new_proj,
                           starting_date){
  
  old_proj <- old_proj %>%
    mutate(date = as.Date(date)) %>%
    filter(date < as.Date(starting_date))
  
  new_proj <- new_proj %>%
    mutate(date = as.Date(date))
  
  proj_data <- old_proj %>%
    bind_rows(new_proj) %>%
    select(-X)
  return(proj_data)
}