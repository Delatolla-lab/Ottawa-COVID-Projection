library(plotly)
hosp_fun <- function(data1, parameter, title, current_color, current_shade,
                     reduction_color, reduction_shade, y) {
  tmp <- max(data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                                 "95_reduction_20", sep = "_"), names(data1))])
  tmp <- tmp*0.55
  trace1 <- list(
    fill = "none",
    line = list(color = as.character(current_color),
                width = 0.05),
    mode = "lines",
    name = "Current distancing upper bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "95_current", sep = "_"), names(data1))],
    visible = TRUE,
    showlegend = FALSE
  )
  trace2 <- list(
    fill = "tonexty",
    fillcolor = as.character(current_shade),
    line = list(color = as.character(current_color),
                width = 0.05),
    mode = "lines",
    name = "Current distancing lower bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "5_current", sep = "_"), names(data1))],
    visible = TRUE,
    showlegend = FALSE
  )
  trace3 <- list(
    line = list(color = as.character(current_color),
                width = 3),
    mode = "lines",
    name = "Current transmission",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "median_current", sep = "_"), names(data1))],
    visible = TRUE,
    showlegend = TRUE
  )
  trace4 <- list(
    line = list(color = as.character(reduction_color),
                width = 0.05),
    mode = "lines",
    name = "20% reduction upper bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "95_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL,
    showlegend = FALSE
  )
  trace5 <- list(
    fill = "tonexty",
    fillcolor = as.character(reduction_shade),
    line = list(color = as.character(reduction_color),
                width = 0.05),
    mode = "lines",
    name = "20% reduction lower bound",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "5_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL,
    showlegend = FALSE
  )
  trace6 <- list(
    line = list(color = as.character(reduction_color),
                width = 3),
    mode = "lines",
    name = "20% transmission increase \n(beginning 2 weeks from today)",
    type = "scatter",
    x = data1$date,
    y = data1[, grepl(paste(
      paste("^", as.character(parameter), sep = ""),
      "median_reduction_20",
      sep = "_"
    ), names(data1))],
    visible = TRUE,
    showlegend = TRUE
  )
  trace7 <- list(
    marker = list(color = as.character(current_color),
                  line = list(color = paste("rgb", as.character(current_color),
                                            sep = ""), width = 0)),
    mode = "lines",
    name = "Observed hospital census",
    type = "bar",
    x = data1$date,
    y = data1[, grepl(paste(paste("^", as.character(parameter), sep = ""),
                            "observed", sep = "_"), names(data1))],
    orientation = "v",
    showlegend = TRUE
  )
  layout <- list(
    title = list(text = as.character(title)),
    xaxis = list(type = "date",
                 title = list(text = "Date")),
    yaxis = list(title = list(text = as.character(y)), range = c(0,tmp)),
    hovermode = "closest",
    width = 700,
    height = 500,
    legend = list(x = 0.05, y = 0.9),
    annotations = list(
      x = 1, y = -0.09, text = "*Shaded area represents the 90% credible region", 
      showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto', xshift=0, yshift=0,
      font=list(size=10)
    )
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
      visible = trace1$visible,
      showlegend = trace1$showlegend
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
      visible = trace2$visible,
      showlegend = trace2$showlegend
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
      visible = trace3$visible,
      showlegend = trace3$showlegend
    )
  p <-
    add_trace(
      p,
      line = trace4$line,
      mode = trace4$mode,
      name = trace4$name,
      type = trace4$type,
      x = trace4$x,
      y = trace4$y,
      showlegend = trace4$showlegend
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
      y = trace5$y,
      showlegend = trace5$showlegend
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
      visible = trace6$visible,
      showlegend = trace6$showlegend
    )
  p <-
    add_trace(
      p,
      marker = trace7$marker,
      mode = trace7$mode,
      name = trace7$name,
      type = trace7$type,
      x = trace7$x,
      y = trace7$y,
      showlegend = trace7$showlegend
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
      legend = layout$legend,
      annotations = layout$annotations
    )
  p
}
