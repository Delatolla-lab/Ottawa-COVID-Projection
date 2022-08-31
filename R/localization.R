insert_translation <- function(data, lang, key, vars=c()){
  translation_row <- as.character(data[data[["key"]] == key, lang])
  Encoding(translation_row) <- "UTF-8"
  if (grepl("\\{\\{", translation_row)){
    # Insert vars
    for (var_index in 1:length(vars)){
      search_param <- as.character(paste0("\\{\\{", var_index,"\\}\\}"))
      translation_row <- sub(search_param, vars[[var_index]], translation_row)
    }
  }
  translation_row <- gsub(pattern = "\n", replacement = "  \n", x = translation_row)
  return(translation_row)
}