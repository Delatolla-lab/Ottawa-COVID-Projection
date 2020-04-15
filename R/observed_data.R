observed_figure <- function(param_list, title, y_label) {
  data <- param_list[[1]]
  doubling_time <- param_list[[2]]
  expected_values <- param_list[[3]]
  library(plotly)
  trace1 <- list(
    meta = list(columnNames = list(
      x = "date", 
      y = "observed_census_ICU"
    )), 
    name = "ICU census", 
    type = "bar", 
    xsrc = "wyusuf:11:3cac68", 
    x = data$date,
    ysrc = "wyusuf:11:5f8f8c", 
    y = data$observed_census_ICU,
    visible = TRUE,
    showlegend = TRUE,
    orientation = "v"
  )
  trace2 <- list(
    meta = list(columnNames = list(
      x = "date", 
      y = "observed_census_acute_care", 
      marker = list(color = "observed_census_acute_care")
    )), 
    mode = "markers", 
    name = "Acute care census", 
    type = "bar", 
    xsrc = "wyusuf:11:3cac68", 
    x = data$date,
    ysrc = "wyusuf:11:5f8f8c", 
    y = data$observed_census_acute_care,
    marker = list(
      meta = list(columnNames = list(color = "observed_census_acute_care")), 
      color = "#636efa"
    ), 
    visible = TRUE, 
    showlegend = TRUE,
    orientation = "v"
  )
  trace3 <- list(
    line = list(
      dash = "dash", 
      color = "rgb(4, 157, 53)", 
      width = 5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "Rate of growth"
    )), 
    mode = "lines", 
    name = paste("Expected values for 3 weeks ago,\nbased on an observed doubling time\nof", round(doubling_time[[1]], 2), "days)"), 
    type = "scatter", 
    xsrc = "wyusuf:11:3cac68", 
    x = data$date,
    ysrc = "wyusuf:11:c15d89", 
    y = expected_values[[1]]$expected_val,
    visible = TRUE,
    showlegend = FALSE,
    orientation = "v"
  )
  trace4 <- list(
    line = list(
      dash = "dash", 
      color = "rgb(4, 157, 53)", 
      width = 5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "previous 7 day"
    )), 
    mode = "lines", 
    name = paste("Expected values for 2 weeks ago,\nbased on an observed doubling time\nof", round(doubling_time[[2]], 2), "days)"),
    type = "scatter", 
    xsrc = "wyusuf:11:3cac68", 
    x = data$date,
    ysrc = "wyusuf:11:3ea644", 
    y = expected_values[[2]]$expected_val,
    visible = TRUE,
    showlegend = FALSE,
    orientation = "v"
  )
  trace5 <- list(
    line = list(
      dash = "dash", 
      color = "rgb(4, 157, 53)", 
      width = 5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "previous previous 7 day"
    )), 
    mode = "lines", 
    type = "scatter",
    name = paste("Expected values for last week,\nbased on an observed doubling time\nof", round(doubling_time[[3]], 2), "days)"),
    xsrc = "wyusuf:11:3cac68", 
    x = data$date,
    ysrc = "wyusuf:11:51678f", 
    y = expected_values[[3]]$expected_val,
    visible = TRUE,
    showlegend = FALSE,
    orientation = "v"
  )
  data <- list(trace1, trace2, trace3, trace4, trace5)
  layout <- list(
    title = list(text = as.character(title)), 
    xaxis = list(
      type = "date", 
      range = c("2020-03-16 12:00", "2020-04-07 12:00"), 
      title = list(text = "Date"), 
      autorange = TRUE
    ), 
    yaxis = list(
      type = "linear", 
      range = c(0, 37.89473684210526), 
      title = list(text = as.character(y_label)), 
      autorange = TRUE
    ), 
    barmode = "relative", 
    autosize = TRUE, 
    template = list(
      data = list(
        bar = list(
          list(
            type = "bar", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        table = list(
          list(
            type = "table", 
            cells = list(
              fill = list(color = "#EBF0F8"), 
              line = list(color = "white")
            ), 
            header = list(
              fill = list(color = "#C8D4E3"), 
              line = list(color = "white")
            )
          )
        ), 
        carpet = list(
          list(
            type = "carpet", 
            aaxis = list(
              gridcolor = "#C8D4E3", 
              linecolor = "#C8D4E3", 
              endlinecolor = "#2a3f5f", 
              minorgridcolor = "#C8D4E3", 
              startlinecolor = "#2a3f5f"
            ), 
            baxis = list(
              gridcolor = "#C8D4E3", 
              linecolor = "#C8D4E3", 
              endlinecolor = "#2a3f5f", 
              minorgridcolor = "#C8D4E3", 
              startlinecolor = "#2a3f5f"
            )
          )
        ), 
        mesh3d = list(
          list(
            type = "mesh3d", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )
          )
        ), 
        contour = list(
          list(
            type = "contour", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ), 
            autocolorscale = TRUE
          )
        ), 
        heatmap = list(
          list(
            type = "heatmap", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ), 
            autocolorscale = TRUE
          )
        ), 
        scatter = list(
          list(
            type = "scatter", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        surface = list(
          list(
            type = "surface", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )
          )
        ), 
        heatmapgl = list(
          list(
            type = "heatmapgl", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )
          )
        ), 
        histogram = list(
          list(
            type = "histogram", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        parcoords = list(
          list(
            line = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )), 
            type = "parcoords"
          )
        ), 
        scatter3d = list(
          list(
            type = "scatter3d", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        scattergl = list(
          list(
            type = "scattergl", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        choropleth = list(
          list(
            type = "choropleth", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )
          )
        ), 
        scattergeo = list(
          list(
            type = "scattergeo", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        histogram2d = list(
          list(
            type = "histogram2d", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ), 
            autocolorscale = TRUE
          )
        ), 
        scatterpolar = list(
          list(
            type = "scatterpolar", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        contourcarpet = list(
          list(
            type = "contourcarpet", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            )
          )
        ), 
        scattercarpet = list(
          list(
            type = "scattercarpet", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        scattermapbox = list(
          list(
            type = "scattermapbox", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        scatterpolargl = list(
          list(
            type = "scatterpolargl", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        scatterternary = list(
          list(
            type = "scatterternary", 
            marker = list(colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ))
          )
        ), 
        histogram2dcontour = list(
          list(
            type = "histogram2dcontour", 
            colorbar = list(
              ticks = "", 
              outlinewidth = 0
            ), 
            autocolorscale = TRUE
          )
        )
      ), 
      layout = list(
        geo = list(
          bgcolor = "white", 
          showland = TRUE, 
          lakecolor = "white", 
          landcolor = "white", 
          showlakes = TRUE, 
          subunitcolor = "#C8D4E3"
        ), 
        font = list(color = "#2a3f5f"), 
        polar = list(
          bgcolor = "white", 
          radialaxis = list(
            ticks = "", 
            gridcolor = "#EBF0F8", 
            linecolor = "#EBF0F8"
          ), 
          angularaxis = list(
            ticks = "", 
            gridcolor = "#EBF0F8", 
            linecolor = "#EBF0F8"
          )
        ), 
        scene = list(
          xaxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            gridwidth = 2, 
            linecolor = "#EBF0F8", 
            zerolinecolor = "#EBF0F8", 
            showbackground = TRUE, 
            backgroundcolor = "white"
          ), 
          yaxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            gridwidth = 2, 
            linecolor = "#EBF0F8", 
            zerolinecolor = "#EBF0F8", 
            showbackground = TRUE, 
            backgroundcolor = "white"
          ), 
          zaxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            gridwidth = 2, 
            linecolor = "#EBF0F8", 
            zerolinecolor = "#EBF0F8", 
            showbackground = TRUE, 
            backgroundcolor = "white"
          )
        ), 
        title = list(x = 0.05), 
        xaxis = list(
          ticks = "", 
          gridcolor = "#EBF0F8", 
          linecolor = "#EBF0F8", 
          automargin = TRUE, 
          zerolinecolor = "#EBF0F8", 
          zerolinewidth = 2
        ), 
        yaxis = list(
          ticks = "", 
          gridcolor = "#EBF0F8", 
          linecolor = "#EBF0F8", 
          automargin = TRUE, 
          zerolinecolor = "#EBF0F8", 
          zerolinewidth = 2
        ), 
        ternary = list(
          aaxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            linecolor = "#A2B1C6"
          ), 
          baxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            linecolor = "#A2B1C6"
          ), 
          caxis = list(
            ticks = "", 
            gridcolor = "#DFE8F3", 
            linecolor = "#A2B1C6"
          ), 
          bgcolor = "white"
        ), 
        colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#19d3f3", "#e763fa", "#fecb52", "#ffa15a", "#ff6692", "#b6e880"), 
        hovermode = "closest", 
        colorscale = list(
          diverging = list(c(0, "#8e0152"),list(0.1, "#c51b7d"),list(0.2, "#de77ae"),list(0.3, "#f1b6da"),list(0.4, "#fde0ef"),list(0.5, "#f7f7f7"),list(0.6, "#e6f5d0"),list(0.7, "#b8e186"),list(0.8, "#7fbc41"),list(0.9, "#4d9221"),list(1, "#276419")), 
          sequential = list(c(0, "#0508b8"),list(0.0893854748603352, "#1910d8"),list(0.1787709497206704, "#3c19f0"),list(0.2681564245810056, "#6b1cfb"),list(0.3575418994413408, "#981cfd"),list(0.44692737430167595, "#bf1cfd"),list(0.5363128491620112, "#dd2bfd"),list(0.6256983240223464, "#f246fe"),list(0.7150837988826816, "#fc67fd"),list(0.8044692737430168, "#fe88fc"),list(0.8938547486033519, "#fea5fd"),list(0.9832402234636871, "#febefe"),list(1, "#fec3fe")), 
          sequentialminus = list(c(0, "#0508b8"),list(0.0893854748603352, "#1910d8"),list(0.1787709497206704, "#3c19f0"),list(0.2681564245810056, "#6b1cfb"),list(0.3575418994413408, "#981cfd"),list(0.44692737430167595, "#bf1cfd"),list(0.5363128491620112, "#dd2bfd"),list(0.6256983240223464, "#f246fe"),list(0.7150837988826816, "#fc67fd"),list(0.8044692737430168, "#fe88fc"),list(0.8938547486033519, "#fea5fd"),list(0.9832402234636871, "#febefe"),list(1, "#fec3fe"))
        ), 
        plot_bgcolor = "white", 
        paper_bgcolor = "white", 
        shapedefaults = list(
          line = list(width = 0), 
          opacity = 0.4, 
          fillcolor = "#506784"
        ), 
        annotationdefaults = list(
          arrowhead = 0, 
          arrowcolor = "#506784", 
          arrowwidth = 1
        )
      ), 
      themeRef = "PLOTLY_WHITE"
    )
  )
  p <- plot_ly()
  p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, marker=trace1$marker, visible=trace1$visible, showlegend = trace1$showlegend, orientation=trace1$orientation)
  p <- add_trace(p, meta=trace2$meta, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, visible=trace2$visible, showlegend = trace2$showlegend, orientation=trace2$orientation)
  p <- add_trace(p, line=trace3$line, meta=trace3$meta, mode=trace3$mode, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, visible=trace3$visible, showlegend = trace3$showlegend, orientation=trace3$orientation)
  p <- add_trace(p, line=trace4$line, meta=trace4$meta, mode=trace4$mode, name=trace4$name, type=trace4$type, xsrc=trace4$xsrc, x=trace4$x, ysrc=trace4$ysrc, y=trace4$y, y=trace4$y, visible=trace4$visible, showlegend = trace4$showlegend, orientation=trace4$orientation)
  p <- add_trace(p, line=trace5$line, meta=trace5$meta, mode=trace5$mode, name=trace5$name, type=trace5$type, xsrc=trace5$xsrc, x=trace5$x, ysrc=trace5$ysrc, y=trace5$y, y=trace5$y, visible=trace5$visible, showlegend = trace5$showlegend, orientation=trace5$orientation)
  p <- add_annotations(p, x = min(expected_values[[1]]$date, na.rm = TRUE), y = median(expected_values[[1]]$expected_val, na.rm = TRUE), text = paste("Doubling time:", "\n",as.character(round(doubling_time[[1]], 2)), "days", sep = " "), xref = "x", yref = "y", showarrow = FALSE)
  p <- add_annotations(p, x = min(expected_values[[2]]$date, na.rm = TRUE), y = median(expected_values[[2]]$expected_val, na.rm = TRUE), text = paste("Doubling time:", "\n",as.character(round(doubling_time[[2]], 2)), "days", sep = " "), xref = "x", yref = "y", showarrow = FALSE)
  p <- add_annotations(p, x = min(expected_values[[3]]$date, na.rm = TRUE), y = median(expected_values[[3]]$expected_val, na.rm = TRUE), text = paste("Doubling time:", "\n", as.character(round(doubling_time[[3]], 2)), "days", sep = " "), xref = "x", yref = "y", showarrow = FALSE)
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, barmode=layout$barmode, autosize=TRUE, template=layout$template, legend = list(x = 0.05, y = 1))
  p
}
