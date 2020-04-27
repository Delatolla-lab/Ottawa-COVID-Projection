# Write function for list generation
#' @param yaxis <- list(list(type = <preset>, y_column = <header>, ?x_column = <header>, ?name = <name>),....)
# yaxis <- list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH"))
# reworked_figure(xaxis = "Date", yaxis = list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH")), titles = c(y = "Daily Observed cases", x = "Date"))
reworked_figure <-
  function(xaxis,
           yaxis,
           titles,
           data#,
           #doubling_time,
           #expected_values
           ) {
    # ---------- PRESETS ----------
    trace_presets <- list(
      doubling_time =
        list(
          mode = "line",
          type = "scatter",
          showlegend = FALSE,
          line = list(
            dash = "dash",
            color = "rgb(4, 157, 53)",
            width = 5
          )
        ),
      observed_data = list(
        type = "bar",
        showlegend = TRUE
      )
    )
    
    library(plotly)
    p <- plot_ly()
    
    for (var_to_map in yaxis) {
      p <- add_trace(
        p,
        mode = trace_presets[[var_to_map$type]]$mode,
        name = var_to_map$name,
        type = trace_presets[[var_to_map$type]]$type,
        showlegend = trace_presets[[var_to_map$type]]$showlegend,
        line = trace_presets[[var_to_map$type]]$line,
        x = data[,xaxis],
        y = data[,var_to_map$y_column]
      )
    }
    p <-
      layout(
        p,
        title = list(text = titles[["title"]], x = 0.5),
        xaxis = list(type = "date",
                     title = list(text = as.character(titles[["x"]]))),
        yaxis = list(title = list(text = as.character(titles[["y"]]))),
        barmode =  "relative",
        autosize = TRUE,
        legend = list(x = 0.05, y = 1)
      )
    
    return(p)
    
  }

observed_figure <- function(param_list,
                            title,
                            y_label) {
  data <- param_list[[1]]
  doubling_time <- param_list[[2]]
  expected_values <- param_list[[3]]
  trace <- list()
  type <- c("bar", "bar", "scatter", "scatter", "scatter")
  mode <- c("", "", "line", "line", "line")
  line <- list(
    FALSE,
    FALSE,
    list(
      dash = "dash",
      color = "rgb(4, 157, 53)",
      width = 5
    ),
    list(
      dash = "dash",
      color = "rgb(4, 157, 53)",
      width = 5
    ),
    list(
      dash = "dash",
      color = "rgb(4, 157, 53)",
      width = 5
    )
  )
  name <-
    c("ICU census",
      "Acute care census")
  x <- list(data$date, data$date, data$date, data$date, data$date)
  y <- list(
    data$observed_census_ICU,
    data$observed_census_acute_care,
    expected_values[[1]]$expected_val,
    expected_values[[2]]$expected_val,
    expected_values[[3]]$expected_val
  )
  showlegend <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  library(plotly)
  p <- plot_ly()
  for (trace_index in 1:5) {
    p <-
      add_trace(
        p,
        mode = mode[trace_index],
        name = name[trace_index],
        type = type[trace_index],
        showlegend = showlegend[[trace_index]],
        line = line[[trace_index]],
        x = x[[trace_index]],
        y = y[[trace_index]]
      )
    
  }
  for (i in 1:3) {
    p <-
      add_annotations(
        p,
        x = ifelse(
          doubling_time[[i]] > 0,
          min(expected_values[[i]]$date, na.rm = TRUE),
          max(expected_values[[i]]$date, na.rm = TRUE)
        ),
        y = median(expected_values[[i]]$expected_val, na.rm = TRUE),
        text = ifelse(doubling_time[[i]] > 0, (
          paste("Doubling time:", "\n", as.character(round(doubling_time[[i]], 1)), "days", sep = " ")
        ), (
          paste("Halving time:", "\n", as.character(round(
            abs(doubling_time[[i]]), 1
          )), "days", sep = " ")
        )),
        xref = "x",
        yref = "y",
        showarrow = FALSE
      )
  }
  p <-
    layout(
      p,
      title = list(text = title, x = 0.5),
      xaxis = list(type = "date",
                   title = list(text = "Date")),
      yaxis = list(title = list(text = as.character(y_label))),
      barmode =  "relative",
      autosize = TRUE,
      legend = list(x = 0.05, y = 1)
    )
  
  return(p)
}
