# Write function for list generation
#' @param yaxis <- list(list(type = <preset>, y_column = <header>, ?x_column = <header>, ?name = <name>),....)
# yaxis <- list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH"))
# reworked_figure(xaxis = "Date", yaxis = list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH")), titles = c(y = "Daily Observed cases", x = "Date"))
reworked_figure <-
  function(xaxis,
           yaxis,
           yaxis2 = NULL,
           titles,
           data) {
    # ---------- PRESETS ----------
    trace_presets <- list(
      doubling_time =
        list(
          mode = "line",
          type = "scatter",
          showlegend = FALSE,
          line = list(
            dash = "dash",
            color = "rgb(39, 62, 71)",
            width = 5
          )
        ),
      avg_data = list(
        type = "scatter",
        mode = "line",
        showlegend = TRUE,
        line = list(
          width = 4
        )
      ),
      observed_data = list(
        type = "bar",
        showlegend = TRUE,
        line = NULL,
        mode = NULL
        #marker = list(color = "rgb(204, 102, 255)")
      ),
      signal_data = list(
        type = "scatter",
        showlegend = TRUE
      )
    )
    attr(trace_presets$doubling_time, "class") <-
      "doubling_time"
    attr(trace_presets$avg_data, "class") <-
      "avg_data"
    attr(trace_presets$observed_data, "class") <-
      "observed_data"
    attr(trace_presets$signal_data, "class") <-
      "signal_data"
    
    library(plotly)
    p <- plot_ly()
    
    for (var_to_map in yaxis) {
      curr_temp <- trace_presets[[var_to_map$type]]
      if (!is_null(var_to_map$color)) {
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]], color = var_to_map$color)
      }
      p <-
        do.call(add_trace, c(
          list(p = p, name = var_to_map$name),
          curr_temp,
          list(x = data[, xaxis],
               y = data[, var_to_map$y_column])
        ))
      # p <- add_trace(
      #   p,
      #   name = var_to_map$name,
      #   unlist(curr_temp),
      #   x = data[, xaxis],
      #   y = data[, var_to_map$y_column]
      # )
    }
    
    for (var_to_map in yaxis2) {
      curr_temp <- trace_presets[[var_to_map$type]]
      if (!is_null(var_to_map$color)) {
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]], color = var_to_map$color)
      }
      p <-
        do.call(add_trace, c(
          list(p = p, name = var_to_map$name),
          curr_temp,
          list(x = data[, xaxis],
               y = data[, var_to_map$y_column],
               yaxis = "y2"),
          opacity = var_to_map$opacity
        ))
      # p <- add_trace(
      #   p,
      #   name = var_to_map$name,
      #   unlist(curr_temp),
      #   x = data[, xaxis],
      #   y = data[, var_to_map$y_column]
      # )
    }
    
    if(is.null(yaxis2)){
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            legend = list(x = 0.05, y = 1)
          )
    }
    else{
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2"),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE,
              zeroline = FALSE
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            legend = list(x = 0.05, y = 0.9)
          )
    }
    return(p)
    
  }
# ---------- COLOR CHANGE FUNCTIONS ---------
change_color <- function(template, ...) {
  UseMethod("change_color", template)
}
change_color.default <- function(template, color) {
  stop("Unspecified template passed")
}

change_color.doubling_time <- function(template, color) {
  return(list(
    mode = "line",
    type = "scatter",
    showlegend = FALSE,
    line = list(
      dash = "dash",
      color = color,
      width = 5
    )
  ))
}

change_color.avg_data <- function(template, color){
  return(list(
    type = "scatter",
    mode = "line",
    showlegend = TRUE,
    line = list(
      color = color,
      width = 4
    )
  ))
}

change_color.observed_data <- function(template, color) {
  return(list(
    type = "bar",
    showlegend = TRUE,
    marker = list(color = color)
  ))
}

change_color.signal_data <- function(template, color) {
  return(list(
    type = "scatter",
    showlegend = TRUE,
    marker = list(color = color)
  ))
}


# observed_figure <- function(param_list,
#                             title,
#                             y_label) {
#   data <- param_list[[1]]
#   doubling_time <- param_list[[2]]
#   expected_values <- param_list[[3]]
#   trace <- list()
#   type <- c("bar", "bar", "scatter", "scatter", "scatter")
#   mode <- c("", "", "line", "line", "line")
#   line <- list(
#     FALSE,
#     FALSE,
#     list(
#       dash = "dash",
#       color = "rgb(4, 157, 53)",
#       width = 5
#     ),
#     list(
#       dash = "dash",
#       color = "rgb(4, 157, 53)",
#       width = 5
#     ),
#     list(
#       dash = "dash",
#       color = "rgb(4, 157, 53)",
#       width = 5
#     )
#   )
#   name <-
#     c("ICU census",
#       "Acute care census")
#   x <- list(data$date, data$date, data$date, data$date, data$date)
#   y <- list(
#     data$observed_census_ICU,
#     data$observed_census_acute_care,
#     expected_values[[1]]$expected_val,
#     expected_values[[2]]$expected_val,
#     expected_values[[3]]$expected_val
#   )
#   showlegend <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
#   library(plotly)
#   p <- plot_ly()
#   for (trace_index in 1:5) {
#     p <-
#       add_trace(
#         p,
#         mode = mode[trace_index],
#         name = name[trace_index],
#         type = type[trace_index],
#         showlegend = showlegend[[trace_index]],
#         line = line[[trace_index]],
#         x = x[[trace_index]],
#         y = y[[trace_index]]
#       )
#
#   }
#   for (i in 1:3) {
#     p <-
#       add_annotations(
#         p,
#         x = ifelse(
#           doubling_time[[i]] > 0,
#           min(expected_values[[i]]$date, na.rm = TRUE),
#           max(expected_values[[i]]$date, na.rm = TRUE)
#         ),
#         y = median(expected_values[[i]]$expected_val, na.rm = TRUE),
#         text = ifelse(doubling_time[[i]] > 0, (
#           paste("Doubling time:", "\n", as.character(round(doubling_time[[i]], 0)), "days", sep = " ")
#         ), (
#           paste("Halving time:", "\n", as.character(round(
#             abs(doubling_time[[i]]), 1
#           )), "days", sep = " ")
#         )),
#         xref = "x",
#         yref = "y",
#         showarrow = FALSE
#       )
#   }
#   p <-
#     layout(
#       p,
#       title = list(text = title, x = 0.5),
#       xaxis = list(type = "date",
#                    title = list(text = "Date")),
#       yaxis = list(title = list(text = as.character(y_label))),
#       barmode =  "relative",
#       autosize = TRUE,
#       legend = list(x = 0.05, y = 1)
#     )
#
#   return(p)
# }
