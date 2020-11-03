# Write function for list generation
#' @param yaxis <- list(list(type = <preset>, y_column = <header>, ?x_column = <header>, ?name = <name>),....)
# yaxis <- list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH"))
# reworked_figure(xaxis = "Date", yaxis = list(list(type = "observed_data", y_column = "observed_new_RH"), list(type = "observed_data", y_column = "observed_new_LTCH")), titles = c(y = "Daily Observed cases", x = "Date"))
reworked_figure <-
  function(xaxis,
           yaxis,
           yaxis2 = NULL,
           yaxis_button = FALSE,
           yaxis2_button = FALSE,
           y_button_name = "",
           y2_button_name = "",
           titles,
           data) {
    # ---------- PRESETS ----------
    tickvals <- floor_date(as_date(data$date), "month")
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
    
    # base parameters for buttons
    base_params <- 'list(
  list(
  active = 0,
  x = -0.2,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 0, "t"= -25, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    base_params_y2 <- 'list(
  list(
  active = 0,
  x = 1.20,
  y = 0.88,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 5, "t"= 0, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    updated <- NULL
    menu <- ""
    updated_y2 <- NULL
    menu_y2 <- ""
    
    for (i in 1:length(yaxis)) {
      var_to_map <- yaxis[[i]]
      curr_temp <- trace_presets[[var_to_map$type]]
      if (!is_null(var_to_map$color)) {
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]], color = var_to_map$color)
      }
      if (isTRUE(yaxis_button)){
          vis_logical <- c(rep(NA, length(yaxis)), rep(T, length(yaxis2)))
          vis_logical[i] <- T
          vis_logical[is.na(vis_logical)] <- F
          vis_logical <- paste0("c(",stringr::str_flatten(vis_logical, ","),")")
          menu_item <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s")))',
                               yaxis[[i]][["short_name"]],
                               vis_logical,
                               titles[["title"]])
          
          if (i < length(yaxis)){
            menu <- stringr::str_glue(stringr::str_glue(menu,menu_item),",")
          } else {
            menu <- stringr::str_glue(menu,menu_item)
          }
          if(i == 1){
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column]),
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}')
              ))
          }else{
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column]),
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}'),
                visible = FALSE
              ))
          }
      }
      else{
        p <-
          do.call(add_trace, c(
            list(p = p, name = var_to_map$name),
            curr_temp,
            list(x = data[, xaxis],
                 y = data[, var_to_map$y_column]),
            opacity = var_to_map$opacity,
            hovertemplate = paste('%{x|%b %d, %Y}:',
                                  '%{y}')
          ))
      }
    }
    
    updated <- sprintf(base_params, menu)
    updated <- eval(parse(text = updated))
    
    if(!is.null(yaxis2)){
      for (i in 1:length(yaxis2)) {
        var_to_map <- yaxis2[[i]]
        curr_temp <- trace_presets[[var_to_map$type]]
        if (!is_null(var_to_map$color)) {
          curr_temp <-
            change_color(template = trace_presets[[var_to_map$type]], color = var_to_map$color)
        }
        
        if (isTRUE(yaxis2_button)){
          vis_logical <- c(rep(T, length(yaxis)))
          vis_logical2 <- c(rep(NA, length(yaxis2)))
          vis_logical2[i] <- T
          vis_logical2[is.na(vis_logical2)] <- F
          vis_logical2 <- c(vis_logical, vis_logical2)
          vis_logical2 <- paste0("c(",stringr::str_flatten(vis_logical2, ","),")")
          menu_item2 <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s",
                          yaxis2.range = c(0,"%s"))))',
                                yaxis2[[i]][["short_name"]],
                                vis_logical2,
                                titles[["title"]],
                                2*max(data[,yaxis2[[i]][["y_column"]]],
                                      na.rm = TRUE))
          
          if (i < length(yaxis2)){
            menu_y2 <- stringr::str_glue(stringr::str_glue(menu_y2,menu_item2),",")
          } else {
            menu_y2 <- stringr::str_glue(menu_y2,menu_item2)
          }
          if(i == 1){
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column],
                     yaxis = "y2"),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}')
              ))
          }else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column],
                   yaxis = "y2"),
              opacity = var_to_map$opacity,
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}'),
              visible = FALSE
            ))
          }
          
        }
        else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column],
                   yaxis = "y2"),
              opacity = var_to_map$opacity,
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }
      }
    }
    
    updated_y2 <- sprintf(base_params_y2, menu_y2)
    updated_y2 <- eval(parse(text = updated_y2))
    
    if(is.null(yaxis2)){
      if(!isTRUE(yaxis_button)){
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
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
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            legend = list(x = 0.05, y = 1),
            updatemenus = updated
          )
      }
    }
    else{
      if(isTRUE(yaxis_button)){
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            legend = list(x = 0.05, y = 0.9),
            updatemenus = updated
          )
      }
      else if(isTRUE(yaxis2_button)){
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE,
              range = c(0, tmp)
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            annotations = list(
              x = 1.26, y = 0.95, text = y2_button_name, 
              showarrow = F, xref='paper', yref='paper',
              font=list(size=15)
            ),
            legend = list(x = 0.05, y = 0.9),
            updatemenus = updated_y2
          )
      }
      else{
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE,
              range = c(0, tmp)
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = TRUE,
            legend = list(x = 0.05, y = 0.9)
          )
      }  
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
