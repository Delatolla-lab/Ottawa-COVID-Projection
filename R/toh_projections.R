library(plotly)
TOH_fun <- function(data1, parameter, title, y) {
  tmp <- max(data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                                 "95_reduction_20", sep = "_"), names(data1))])
  tmp <- tmp*0.55
  trace1 <- list(
    fill = "none",
    line = list(color = "rgb(57, 157, 195)",
                width = 0.5),
    mode = "lines",
    name = "Current distancing upper bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "95_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace2 <- list(
    fill = "tonexty",
    fillcolor = "rgba(57, 157, 195)",
    line = list(color = "rgb(57, 157, 195)",
                width = 0.5),
    mode = "lines",
    name = "Current distancing lower bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "5_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace3 <- list(
    line = list(color = "rgb(57, 157, 195)",
                width = 3),
    mode = "lines",
    name = "Current distancing median estimate",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "median_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace4 <- list(
    line = list(color = "rgb(214, 39, 40)",
                width = 0.5),
    mode = "lines",
    name = "20% reduction upper bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "95_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL
  )
  trace5 <- list(
    fill = "tonexty",
    fillcolor = "rgba(214, 39, 40)",
    line = list(color = "rgb(214, 39, 40)",
                width = 0.5),
    mode = "lines",
    name = "20% reduction lower bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "5_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL
  )
  trace6 <- list(
    line = list(color = "rgb(214, 39, 40)",
                width = 3),
    mode = "lines",
    name = "20% reduction median estimate",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(
      paste("^", as.character(parameter), sep = ""),
      "median_reduction_20",
      sep = "_"
    ), names(data1))],
    visible = TRUE
  )
  trace7 <- list(
    marker = list(color = 'rgb(254, 203, 82)',
                  line = list(color = 'rgb(8,48,107)', width = 0)),
    mode = "lines",
    name = "Observed hospital census",
    type = "bar",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "observed", sep = "_"), names(data1))],
    orientation = "v"
  )
  layout <- list(
    title = list(text = as.character(title)),
    xaxis = list(type = "date",
                 title = list(text = "Date")),
    yaxis = list(title = list(text = as.character(y)), range = c(0,tmp)),
    hovermode = "closest",
    width = 700,
    height = 500,
    showlegend = TRUE
  )
  p <- plot_ly()
  p <-
    add_trace(
      p,
      fill = trace1$fill,
      line = trace1$line,
      mode = trace1$mode,
      name = trace1$name,
      type = trace1$type,
      x = trace1$x,
      y = trace1$y,
      visible = trace1$visible
    )
  p <-
    add_trace(
      p,
      fill = trace2$fill,
      fillcolor = trace2$fillcolor,
      line = trace2$line,
      mode = trace2$mode,
      name = trace2$name,
      type = trace2$type,
      x = trace2$x,
      y = trace2$y,
      visible = trace2$visible
    )
  p <-
    add_trace(
      p,
      line = trace3$line,
      mode = trace3$mode,
      name = trace3$name,
      type = trace3$type,
      x = trace3$x,
      y = trace3$y,
      visible = trace3$visible
    )
  p <-
    add_trace(
      p,
      line = trace4$line,
      mode = trace4$mode,
      name = trace4$name,
      type = trace4$type,
      x = trace4$x,
      y = trace4$y
    )
  p <-
    add_trace(
      p,
      fill = trace5$fill,
      fillcolor = trace5$fillcolor,
      line = trace5$line,
      mode = trace5$mode,
      name = trace5$name,
      type = trace5$type,
      x = trace5$x,
      y = trace5$y
    )
  p <-
    add_trace(
      p,
      line = trace6$line,
      meta = trace6$meta,
      mode = trace6$mode,
      name = trace6$name,
      type = trace6$type,
      x = trace6$x,
      y = trace6$y,
      visible = trace6$visible
    )
  p <-
    add_trace(
      p,
      marker = trace7$marker,
      mode = trace7$mode,
      name = trace7$name,
      type = trace7$type,
      x = trace7$x,
      y = trace7$y
    )
  p <-
    layout(
      p,
      title = layout$title,
      xaxis = layout$xaxis,
      yaxis = layout$yaxis,
      hovermode = layout$hovermode,
      width = layout$width,
      height = layout$height,
      showlegend = layout$showlegend
    )
  p
}
