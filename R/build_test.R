langs <- c("en", "fr")

for (current_lang in langs) {
  filename <- paste0("_index",".html")
  rmarkdown::render(
    "content/_index.Rmd",
    output_format = "html_document",
    output_file = filename,
    output_dir = paste0("content/",current_lang),
    params = list(lang = current_lang)
  )
}