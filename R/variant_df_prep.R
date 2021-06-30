variant_df_prep <- function(sheet){
  sheet$pangolin_lineage <- ifelse(
    grepl("^B.1.617", sheet$pangolin_lineage) == TRUE, "B.1.617+",
    sheet$pangolin_lineage)
  df <- pivot_wider(sheet, names_from = pangolin_lineage,
                    values_from = c(prop_voc_mutations,
                                    detection_quality, frac_voc_detected)) %>%
    arrange(date_collected)
  
  var_text <- data.frame(variable = character(),
                         details = character())
  
  variables <- c("date", "alphaDetect", "alphaLevel", "deltaDetect",
                 "deltaLevel", "otherVOCDetect", "otherVOCLevel",
                 "summaryDescription", 
                 "ctN1Assay", "ctN2Assay",
                 "detailedDescription1", "detailedDescription2",
                 "detailedDescription3", "detailedDescription4")
  
  var_text[nrow(var_text)+length(variables),] <- NA
  
  df$`detection_quality_B.1.617+` <- 
    ifelse(is.vector(df$`detection_quality_B.1.617+`),
           ifelse("high" %in% df$`detection_quality_B.1.617+`, "high", "low"),
           df$`detection_quality_B.1.617+`)
  
  x <- 0
  
  for(i in variables){
    x <- x + 1
    var_text[[1]][[x]] <- i
  }
  
  # Fill date
  var_text[[2]][[1]] <- as.character(last(df$date_collected))
  
  # Fill alphaDetect value
  var_text[[2]][[2]] <- ifelse(last(df$detection_quality_B.1.1.7) == "high" ||
                                 last(df$detection_quality_B.1.1.7) == "low",
                                 TRUE, FALSE)
  
  # Fill alphaLevel
  var_text[[2]][[3]] <- ifelse(last(df$detection_quality_B.1.1.7) ==
                                 "non-detect", NA,
                               last(df$detection_quality_B.1.1.7))
  
  # Fill deltaDetect
  var_text[[2]][[4]] <- ifelse(last(df$`detection_quality_B.1.617+`) == "high" ||
                                 last(df$`detection_quality_B.1.617+`) == "low",
                               TRUE, FALSE)
  
  # Fill deltaLevel
  var_text[[2]][[5]] <-
    ifelse(last(df$`detection_quality_B.1.617+`) == 
                                 "non-detect", NA,
           last(df$`detection_quality_B.1.617+`))
  
  # Fill otherVOCDetect
  var_text[[2]][[6]] <-
    ifelse(last(df$`detection_quality_other VOC/VOI`) == "high" || 
             last(df$`detection_quality_other VOC/VOI`) == "low", TRUE, FALSE)
  
  # Fill otherVOCLevel
  var_text[[2]][[7]] <- ifelse(last(df$`detection_quality_other VOC/VOI`) ==
                                 "non-detect" ||
                                 is.null(last(df$`detection_quality_other VOC/VOI`)),
                               NA, last(df$`detection_quality_other VOC/VOI`))
  
  # Interpretation text
  date <- format(as.Date(last(df$date_collected)), "%B %d, %Y")
  
  alpha_prev <- ifelse(is.na(var_text[[2]][[3]]), "no", var_text[[2]][[3]])
  
  delta_prev <- ifelse(is.na(var_text[[2]][[5]]), "no", var_text[[2]][[5]])
  
  delta_prev_2_wk <- ifelse(df[nrow(df)-1, "detection_quality_B.1.617+"]
                            == "high", "high confidence",
                            ifelse(df[nrow(df)-1,
                                      "detection_quality_B.1.617+"] ==
                                     "low", "low confidence", "no"))
  
  voc_prev <- ifelse(var_text[[2]][[6]] == FALSE, "No other", "Other")
  
  var_text[[2]][[8]] <-
    paste(paste0(date, ":"), "Ottawa wastewater shows", alpha_prev, "confidence detection of B.1.1.7 and", delta_prev, "confidence of B.1.617+ lineages. Two weeks prior,", delta_prev_2_wk, "detection of B.1.617+ lineages was observed.", voc_prev, "VOC/VOI were identified above the level of detection of the assay.")
  
  var_text[[2]][[9]] <- last(df$CDC_N1)
  
  var_text[[2]][[10]] <- last(df$CDC_N2)
  
  var_text[[2]][[11]] <-
  paste("The assembled consensus community genome (i.e., most frequent metagenome observed) was consistent with genomes of B.1.1.7 lineage (aka the UK variant) with", paste0(last(df$breadth_of_coverage_pct),"%"), "genome coverage at >5X read depth. The mean depth of coverage across the community genome was", paste0(round(last(df$mean_coverage_depth), 0), "X."))
  
  var_text[[2]][[12]] <-
  paste(last(df$frac_voc_detected_B.1.1.7), "mutations associated with B.1.1.7 genomes were present in the community genome.")
  
  var_text[[2]][[13]] <-
  paste("Of every 100 reads that overlapped sites of B.1.1.7 mutations, approx.", round(last(df$prop_voc_mutations_B.1.1.7), -1), "were found to harbour B.1.1.7 mutations. This is consistent with a sample where the major proportion of contributing viral RNA can be assigned to the B.1.1.7 lineage.")
  
  other_voc_sufficiency <-
    ifelse(var_text[[2]][[6]] == FALSE, "insufficient",
           "sufficient")
  
  var_text[[2]][[14]] <-
    paste("There was", other_voc_sufficiency, "evidence to support the presence of any other VOC/VOI above the level of detection of the assay.")
  
  return(var_text)
}