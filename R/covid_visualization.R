param_append <- function(parameter, append) {
  paste(parameter, append, sep = "_")
}
data_y_creation <-
  function(data,
           prefix,
           suffix,
           pre_suffix_suffix,
           parameter) {
    full_search <-
      paste(prefix, parameter, pre_suffix_suffix, suffix, sep = "")
    data[, grepl(full_search, names(data))]
  }
hosp_visualization <-
  function(data1,
           data2,
           parameter,
           title,
           y_label,
           annote = FALSE) {
    trace <- list()
    fill <- c("tozeroy", "tozeroy", "tozeroy", NULL, NULL)
    meta <-
      c(
        param_append(parameter, 50),
        param_append(parameter, 60),
        param_append(parameter, 70),
        "observed_data",
        "hospital_capacity"
      )
    mode <- c("lines", "lines", "lines", "markers+lines", "lines")
    name <-
      c(
        "Current distancing effectiveness",
        "60% physical distancing",
        "70% physical distancing",
        "Reported # of patients",
        "Hospital capacity"
      )
    type <- c("scatter", "scatter", "scatter", "scatter", "scatter")
    x <- list(data1$date, data1$date, data1$date, data2$date, data2$date)
    ydata <- list(data1, data1, data1, data2, data2)
    yprefix <- c("^", "^", "^", "^observed_", "^capacity_")
    ysuffix <- c("_50", "_60", "_70", "", "")
    ypre_suffix <- c("", "", "", "$", "$")
    
    for (trace_index in 1:5) {
      trace[[trace_index]] <- list(
        fill = fill[trace_index],
        meta = list(columnNames = list(x = "date",
                                       y = meta[trace_index])),
        mode = mode[trace_index],
        name = name[trace_index],
        type = type[trace_index],
        x = x[[trace_index]],
        y = data_y_creation(ydata[[trace_index]], yprefix[trace_index], ysuffix[trace_index], ypre_suffix[trace_index], parameter)
      )
    }
    library(plotly)
    data <- trace
    layout <- list(
      title = list(text = paste(as.character(title)), x = 0.5), 
      xaxis = list(
        type = "date", 
        range = c("2020-03-05 14:13:30.5106", "2020-10-16"), 
        title = list(text = "Date"), 
        autorange = TRUE
      ), 
      yaxis = list(
        type = "linear", 
        range = c(0, 688.421052631579), 
        title = list(text = paste(as.character(y_label))), 
        autorange = TRUE
      ), 
      autosize = TRUE,
      annotations =
        ifelse(annote == TRUE,
               list(
                 list(
                   x = Sys.Date(), 
                   y = max(trace[[4]]$y, na.rm = TRUE), 
                   ax = 0, 
                   ay = -50, 
                   font = list(color = "rgb(214, 39, 40)"), 
                   text = "Current use (drag to zoom,
            double click to zoom out)", 
                   arrowcolor = "rgb(214, 39, 40)"
                 )
               ),
               list())
    )
    p <-
      plot_ly() %>% config(modeBarButtonsToRemove = c("toggleSpikelines", "lasso2d", "select2d"))
    for (trace_index in 1:5) {
      p <-
        add_trace(
          p,
          fill = trace[[trace_index]]$fill,
          meta = trace[[trace_index]]$meta,
          mode = trace[[trace_index]]$mode,
          name = trace[[trace_index]]$name,
          type = trace[[trace_index]]$type,
          x = trace[[trace_index]]$x,
          y = trace[[trace_index]]$y
        )
      
    }
 
    p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, 
                annotations=layout$annotations, 
                autosize = FALSE, width = 700, height = 500,
                legend = list(x = 0.05, y = 0.9))
    return(p)
  }

death_visualization <- function(data1,
                                data2,
                                parameter,
                                title,
                                y_label){
  trace <- list()
  fill <- c("tozeroy", "tozeroy", "tozeroy", NULL)
  meta <-
    c(
      param_append(parameter, 50),
      param_append(parameter, 60),
      param_append(parameter, 70),
      "observed_data"
    )
  mode <- c("lines", "lines", "lines", "markers+lines")
  name <-
    c(
      "Current distancing effectiveness",
      "60% physical distancing",
      "70% physical distancing",
      "Reported # of deaths"
    )
  type <- c("scatter", "scatter", "scatter", "scatter")
  x <- list(data1$date, data1$date, data1$date, data2$date)
  ydata <- list(data1, data1, data1, data2, data2)
  yprefix <- c("^", "^", "^", "^observed_")
  ysuffix <- c("_50", "_60", "_70", "")
  ypre_suffix <- c("", "", "", "$")
  for (trace_index in 1:4) {
    trace[[trace_index]] <- list(
      fill = fill[trace_index],
      meta = list(columnNames = list(x = "date",
                                     y = meta[trace_index])),
      mode = mode[trace_index],
      name = name[trace_index],
      type = type[trace_index],
      x = x[[trace_index]],
      y = data_y_creation(ydata[[trace_index]], yprefix[trace_index], ysuffix[trace_index], ypre_suffix[trace_index], parameter)
    )
  }
  library(plotly)
  data <- trace
  layout <- list(
    title = list(text = paste(as.character(title)), x = 0.5), 
    xaxis = list(
      type = "date", 
      range = c("2020-03-05 14:13:30.5106", "2020-10-16"), 
      title = list(text = "Date"), 
      autorange = TRUE
    ), 
    yaxis = list(
      type = "linear", 
      range = c(0, 688.421052631579), 
      title = list(text = paste(as.character(y_label))), 
      autorange = TRUE
    ), 
    autosize = TRUE
  )
  
  p <- 
    plot_ly() %>% config(modeBarButtonsToRemove = c("toggleSpikelines", "lasso2d", "select2d"))
  for (trace_index in 1:4) {
    p <-
      add_trace(
        p,
        fill = trace[[trace_index]]$fill,
        meta = trace[[trace_index]]$meta,
        mode = trace[[trace_index]]$mode,
        name = trace[[trace_index]]$name,
        type = trace[[trace_index]]$type,
        x = trace[[trace_index]]$x,
        y = trace[[trace_index]]$y
      )
    
  }
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=TRUE,
              width = 700, height = 500,
              legend = list(x = 0.05, y = 0.9))
  return(p)
}