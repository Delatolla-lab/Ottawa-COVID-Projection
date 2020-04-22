observed_figure <- function(param_list,
                            title,
                            y_label) {
  data <- param_list[[1]]
  doubling_time <- param_list[[2]]
  expected_values <- param_list[[3]]
  trace <- list()
  type <- c("bar", "bar", "scatter", "scatter", "scatter")
  mode <- c("", "", "line", "line", "line")
  name <-
    c(
      "ICU census",
      "Acute care census"
    )
  x <- list(data$date, data$date, data$date, data$date, data$date)
  y <- list(
    data$observed_census_ICU,
    data$observed_census_acute_care,
    expected_values[[1]]$expected_val,
    expected_values[[2]]$expected_val,
    expected_values[[3]]$expected_val
    )
  for (trace_index in 1:5) {
    trace[[trace_index]] <- list(
      mode = mode[trace_index],
      name = name[trace_index],
      type = type[trace_index],
      x = x[[trace_index]],
      y = y[[trace_index]]
    )
  }
  library(plotly)
  data <- trace 
  layout <- list(
    title = list(text = title, x=0.5), 
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
  p <- plot_ly() %>%
    for (trace_index in 1:5) {
      p <-
        add_trace(
          p,
          mode = trace[[trace_index]]$mode,
          name = trace[[trace_index]]$name,
          type = trace[[trace_index]]$type,
          x = trace[[trace_index]]$x,
          y = trace[[trace_index]]$y
        )
      
    }
  p <- add_annotations(p, x = ifelse(doubling_time[[1]] > 0, min(expected_values[[1]]$date, na.rm = TRUE), max(expected_values[[1]]$date, na.rm = TRUE)), y = median(expected_values[[1]]$expected_val, na.rm = TRUE), text = ifelse(doubling_time[[1]] > 0, (paste("Doubling time:", "\n",as.character(round(doubling_time[[1]], 1)), "days", sep = " ")), (paste("Halving time:", "\n",as.character(round(abs(doubling_time[[1]]), 1)), "days", sep = " "))), xref = "x", yref = "y", showarrow = FALSE)
  p <- add_annotations(p, x = ifelse(doubling_time[[2]] > 0, min(expected_values[[2]]$date, na.rm = TRUE), max(expected_values[[2]]$date, na.rm = TRUE)), y = median(expected_values[[2]]$expected_val, na.rm = TRUE), text = ifelse(doubling_time[[2]] > 0, (paste("Doubling time:", "\n",as.character(round(doubling_time[[2]], 1)), "days", sep = " ")), (paste("Halving time:", "\n",as.character(round(abs(doubling_time[[2]]), 1)), "days", sep = " "))), xref = "x", yref = "y", showarrow = FALSE)
  p <- add_annotations(p, x = ifelse(doubling_time[[3]] > 0, min(expected_values[[3]]$date, na.rm = TRUE), max(expected_values[[3]]$date, na.rm = TRUE)), y = median(expected_values[[3]]$expected_val, na.rm = TRUE), text = ifelse(doubling_time[[3]] > 0, (paste("Doubling time:", "\n",as.character(round(doubling_time[[3]], 1)), "days", sep = " ")), (paste("Halving time:", "\n",as.character(round(abs(doubling_time[[3]]), 1)), "days", sep = " "))), xref = "x", yref = "y", showarrow = FALSE)
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, barmode=layout$barmode, autosize=TRUE, template=layout$template, legend = list(x = 0.05, y = 1))
  
  return(p)
}
