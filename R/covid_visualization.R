hosp_visualization <- function(data1, data2, parameter, title, y_label, annote = FALSE){
  library(plotly)
  trace1 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(parameter, 50, sep = "_")
    )), 
    mode = "lines", 
    name = "50% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:d75a98", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), 50, sep = "_"), names(data1))]
  )
  trace2 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(as.character(parameter), 60, sep = "_")
    )), 
    mode = "lines", 
    name = "60% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:768815", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), 60, sep = "_"), names(data1))], 
    stackgroup = NULL
  )
  trace3 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(as.character(parameter), 70, sep = "_")
    )), 
    mode = "lines", 
    name = "70% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:3dc4e1", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), 70, sep = "_"), names(data1))], 
    stackgroup = NULL
  )
  trace4 <- list(
    meta = list(columnNames = list(
      x = "date", 
      y = "observed_data"
    )), 
    mode = "markers+lines", 
    name = "Reported # of patients", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data2$date, 
    ysrc = "wyusuf:1:420b6f", 
    y = data2[,grepl(paste("^observed",as.character(parameter), sep = "_"), names(data2))]
  )
  trace5 <- list(
    line = list(
      dash = "dash", 
      color = "rgb(24, 24, 24)"
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hospital_capacity"
    )), 
    mode = "lines", 
    name = "Hospital capacity", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data2$date, 
    ysrc = "wyusuf:1:a5d486", 
    y = data2[,grepl(paste("^capacity", as.character(parameter), sep = "_"), names(data2))]
  )
  data <- list(trace1, trace2, trace3, trace4, trace5)
  layout <- list(
    title = list(text = paste(as.character(title))), 
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
            y = max(trace4$y), 
            ax = 0, 
            ay = -50, 
            font = list(color = "rgb(214, 39, 40)"), 
            text = "Current use (drag to zoom)", 
            arrowcolor = "rgb(214, 39, 40)"
          )
        ),
        list())
  )
  p <- plot_ly()
  p <- add_trace(p, fill=trace1$fill, meta=trace1$meta, mode=trace1$mode, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y)
  p <- add_trace(p, fill=trace2$fill, meta=trace2$meta, mode=trace2$mode, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, stackgroup=trace2$stackgroup)
  p <- add_trace(p, fill=trace3$fill, meta=trace3$meta, mode=trace3$mode, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, stackgroup=trace3$stackgroup)
  p <- add_trace(p, meta=trace4$meta, mode=trace4$mode, name=trace4$name, type=trace4$type, xsrc=trace4$xsrc, x=trace4$x, ysrc=trace4$ysrc, y=trace4$y)
  p <- add_trace(p, line=trace5$line, meta=trace5$meta, mode=trace5$mode, name=trace5$name, type=trace5$type, xsrc=trace5$xsrc, x=trace5$x, ysrc=trace5$ysrc, y=trace5$y)
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, 
              annotations=layout$annotations, 
              autosize=FALSE, width = 1000, height = 500)
  p
}

death_visualization <- function(data1, data2, parameter, title, y_label){
  library(plotly)
  trace1 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(parameter, 50, sep = "_")
    )), 
    mode = "lines", 
    name = "50% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:d75a98", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), "50$", sep = "_"), names(data1))]
  )
  trace2 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(as.character(parameter), 60, sep = "_")
    )), 
    mode = "lines", 
    name = "60% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:768815", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), "60$", sep = "_"), names(data1))], 
    stackgroup = NULL
  )
  trace3 <- list(
    fill = "tozeroy", 
    meta = list(columnNames = list(
      x = "date", 
      y = paste(as.character(parameter), 70, sep = "_")
    )), 
    mode = "lines", 
    name = "70% physical distancing", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data1$date, 
    ysrc = "wyusuf:1:3dc4e1", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""), "70$", sep = "_"), names(data1))], 
    stackgroup = NULL
  )
  trace4 <- list(
    meta = list(columnNames = list(
      x = "date", 
      y = "observed_data"
    )), 
    mode = "markers+lines", 
    name = "Reported # of deaths", 
    type = "scatter", 
    xsrc = "wyusuf:1:9c770a", 
    x = data2$date, 
    ysrc = "wyusuf:1:420b6f", 
    y = data2[,grepl(paste("^observed",as.character(parameter), sep = "_"), names(data2))]
  )
  data <- list(trace1, trace2, trace3, trace4)
  layout <- list(
    title = list(text = paste(as.character(title))), 
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
  p <- plot_ly()
  p <- add_trace(p, fill=trace1$fill, meta=trace1$meta, mode=trace1$mode, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y)
  p <- add_trace(p, fill=trace2$fill, meta=trace2$meta, mode=trace2$mode, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, stackgroup=trace2$stackgroup)
  p <- add_trace(p, fill=trace3$fill, meta=trace3$meta, mode=trace3$mode, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, stackgroup=trace3$stackgroup)
  p <- add_trace(p, meta=trace4$meta, mode=trace4$mode, name=trace4$name, type=trace4$type, xsrc=trace4$xsrc, x=trace4$x, ysrc=trace4$ysrc, y=trace4$y)
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=FALSE,
              width = 1000, height = 500)
  p
}