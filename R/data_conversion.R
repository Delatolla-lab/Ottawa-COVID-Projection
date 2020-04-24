#' @param strata list(<header_name> = c(<vals>))
#' @example 
#' tmp <- figure_input_creation_line_listing(data = outbreak_LL_institutions, sort_date_column = "Accurate Episode Date", strata = list("Exposure Setting Type Desc" = c("RH","LTCH"), "Exposure Setting Comments" = c("FACILITY WIDE")))

figure_input_creation_line_listing <- function(data, sort_date_column, strata){
  # Remove NA dates
  data <- data[!is.na(data[[sort_date_column]]),]
  # Order by date
  data <- data[order(as.Date(data[[sort_date_column]], format = "%Y-%m-%d")),]
  
  # Remove rows not containing strata
  for (header in names(strata)) {
    data <- data[grepl(paste(strata[[header]], collapse = "|"), data[[header]]),]
  }
  
  # Output whats left
  return(data)
}

#' @example 
#' test <- data_column_standardisation(tmp, "Exposure Setting Type Desc","Accurate Episode Date","observed_new")
data_column_standardisation <- function(data, strata, date_column, new_columns_prefix){
  library(dplyr)
  # Create vector containing dates from start to finish
  all_dates <- seq.Date(from = min(data[[date_column]]), to = max(data[[date_column]]), by = "day")
  
  case_types <- unique(data[[strata]])
  new_out_data <- data.frame()
  # Match all rows containing date
  for (single_day in all_dates) {
    new_row <- c()
    new_row[["Date"]] <- as.Date(single_day, format =  "%Y-%m-%d", origin = "1970-01-01")
    events <- data[data[[date_column]] == single_day, ]
    for (value in case_types) {
      type_count <- nrow(events[events[[strata]]==value,])
      new_row[[paste(new_columns_prefix, value, sep = "_")]] <- type_count
    }
    new_out_data <- rbind(new_out_data, new_row)
  }
  
  new_out_data[["Date"]] <- as.Date(new_out_data[["Date"]], format =  "%Y-%m-%d", origin = "1970-01-01")
  
  return(new_out_data)
  # Count number of unique values
  
  # append the vector containing that info
  
  # Combine new dataframe columns
  
  #return
}
#all_dates <- complete(Date = seq.Date(tmp[1,date_column], tmp[nrow(data),tmp ], by = "day"))
#complete(Date = seq.Date(<start_date>, <end_date>, by=<date_unit>))