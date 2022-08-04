library(data.table)
clean_vacc_data <- function(data){
  #function to clean vaccine data:
  data <- data %>% mutate(Date = as.Date(Date))
  
  setnames(data, old = c('Date','Percent_of_Ottawa_residents_5_1', 'Percent_of_Ottawa_residents_12_', 
                              'Percent_of_Ottawa_residents_18_', 'Percent_of_Ottawa_residents_30_', 'Percent_of_Ottawa_residents_40_',
                              'Percent_of_Ottawa_residents_50_', 'Percent_of_Ottawa_residents_60_', 'Percent_of_Ottawa_residents_70_',
                              'Percent_of_Ottawa_residents_80_', 'Percent_of_Ottawa_residents_5_2', 'Percent_of_Ottawa_residents_121',
                              'Percent_of_Ottawa_residents_181', 'Percent_of_Ottawa_residents_301', 'Percent_of_Ottawa_residents_401',
                              'Percent_of_Ottawa_residents_501', 'Percent_of_Ottawa_residents_601', 'Percent_of_Ottawa_residents_701',
                              'Percent_of_Ottawa_residents_801'), new = c('date','Percent_of_Ottawa_residents_5_1_dose',
                                                                          'Percent_of_Ottawa_residents_12_1_dose',
                                                                          'Percent_of_Ottawa_residents_18_1_dose',
                                                                          'Percent_of_Ottawa_residents_30_1_dose',
                                                                          'Percent_of_Ottawa_residents_40_1_dose',
                                                                          'Percent_of_Ottawa_residents_50_1_dose',
                                                                          'Percent_of_Ottawa_residents_60_1_dose',
                                                                          'Percent_of_Ottawa_residents_70_1_dose',
                                                                          'Percent_of_Ottawa_residents_80_1_dose',
                                                                          'Percent_of_Ottawa_residents_5_2_dose',
                                                                          'Percent_of_Ottawa_residents_12_2_dose',
                                                                          'Percent_of_Ottawa_residents_18_2_dose',
                                                                          'Percent_of_Ottawa_residents_30_2_dose',
                                                                          'Percent_of_Ottawa_residents_40_2_dose',
                                                                          'Percent_of_Ottawa_residents_50_2_dose',
                                                                          'Percent_of_Ottawa_residents_60_2_dose',
                                                                          'Percent_of_Ottawa_residents_70_2_dose',
                                                                          'Percent_of_Ottawa_residents_80_2_dose'
                              ))
  cols <- colnames(data)
  cols <- cols[! cols %in% c('date')]
  for (col in cols){
  data[,`col`] <- gsub("%", "", data[,`col`])
  data[,`col`] <- as.numeric(data[,`col`])
  
  }
  
  return (data)

}