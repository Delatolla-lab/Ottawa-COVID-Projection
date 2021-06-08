variant_df_prep <- function(sheet){
  df <- pivot_wider(sheet, names_from = pangolin_lineage,
                    values_from = c(detect, consensus_subconsensus_NA,
                                    prop_voc_mutations, frac_voc_detected)) %>%
    arrange(date_collected)
  
  var_text <- data.frame(variable = character(),
                         details = character())
  
  variables <- c("date", "alphaDetect", "alphaLevel", "deltaDetect",
                 "deltaLevel", "otherVOCDetect", "otherVOCLevel",
                 "interpretiveNote", 
                 "ctN1Assay", "ctN2Assay")
  
  var_text[nrow(var_text)+length(variables),] <- NA
  
  x <- 0
  
  for(i in variables){
    x <- x + 1
    var_text[[1]][[x]] <- i
  }
  
  # Fill date
  var_text[[2]][[1]] <- last(df$date_collected)
  
  # Fill alphaDetect value
  var_text[[2]][[2]] <- ifelse(last(df$detect_B.1.1.7) == "detect",
                                 TRUE, FALSE)
  
  # Fill alphaLevel
  var_text[[2]][[3]] <- ifelse(last(df$consensus_subconsensus_NA_B.1.1.7) == 
                                 "consensus", "high",
                               ifelse(last(
                                 df$consensus_subconsensus_NA_B.1.1.7) == 
                                                               "sub-consensus",
                                 "low", NA))
  
  # Fill deltaDetect
  var_text[[2]][[4]] <- ifelse(last(df$`detect_B.1.617+`) == "detect",
                               TRUE, FALSE)
  
  # Fill deltaLevel
  var_text[[2]][[5]] <- ifelse(last(df$`consensus_subconsensus_NA_B.1.617+`) == 
                                 "consensus", "high",
                               ifelse(last(
                                 df$`consensus_subconsensus_NA_B.1.617+`) == 
                                   "sub-consensus",
                                 "low", NA))
  
  # Fill otherVOCDetect
  var_text[[2]][[6]] <- ifelse(last(df$`detect_other VOC/VOI`) == "detect",
                               TRUE, FALSE)
  
  # Fill otherVOCLevel
  var_text[[2]][[7]] <- ifelse(last(
    df$`consensus_subconsensus_NA_other VOC/VOI`) == 
                                 "consensus", "high",
    ifelse(last(df$`consensus_subconsensus_NA_other VOC/VOI`) == 
                                   "sub-consensus",
                                 "low", NA))
  
  # Interpretation text
  date <- format(as.Date(last(df$date_collected)), "%B %d, %Y")
  
  alpha_prev <- ifelse(is.na(var_text[[2]][[3]]), "no", var_text[[2]][[3]])
  
  delta_prev <- ifelse(is.na(var_text[[2]][[5]]), "no", var_text[[2]][[5]])
  
  alpha_prev_2_wk <- ifelse(df[nrow(df)-1, "consensus_subconsensus_NA_B.1.1.7"]
                            == "high", "high confidence",
                            ifelse(df[nrow(df)-1,
                                      "consensus_subconsensus_NA_B.1.1.7"] ==
                                     "low", "low confidence", "no"))
  
  voc_prev <- ifelse(var_text[[2]][[6]] == FALSE, "No other", "Other")
  
  var_text[[2]][[8]] <- paste(paste0(date, ":"), "Ottawa wastewater shows",
                              alpha_prev, "confidence detection of B.1.1.7 and",
                              delta_prev, "confidence of B.1.617+ lineages.",
                              voc_prev,
            "VOC/VOI were identified above the level of detection of the assay.")
  
  var_text[[2]][[9]] <- last(df$CDC_N1)
  
  var_text[[2]][[10]] <- last(df$CDC_N2)
  
  return(var_text)
}