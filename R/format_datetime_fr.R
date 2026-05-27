#' Format datetime in French
#'
#' @param time POSIXct datetime object
#' @param date_only Logical. If TRUE, only the date is returned (no time).
#' @return Character string with French-formatted datetime (e.g., "11 mai 2026 à 07:43:37")
#'
#' @examples
#' format_datetime_fr(Sys.time())
#' format_datetime_fr(Sys.time(), date_only = TRUE)
format_datetime_fr <- function(time, date_only = FALSE) {
  locale_fr <- clock::clock_locale("fr")
  day <- clock::get_day(time)
  month <- clock::date_format(time, format = "%B", locale = locale_fr)
  year <- clock::get_year(time)
  if (date_only) {
    paste0(day, " ", month, " ", year)
  } else {
    time_str <- clock::date_format(time, format = "%H:%M:%S")
    paste0(day, " ", month, " ", year, " à ", time_str)
  }
}
