TOH_projections <- read.csv("Data/TOH_projection_estimates.csv")
TOH_observed <- read.csv("Data/Observed data/TOH_Observed_Hospital_Use.csv")

library(plotly)
TOH_fun <- function(data1, data2, parameter) {
  trace1 <- list(
    fill = "none", 
    line = list(
      color = "rgb(57, 157, 195)", 
      width = 0.5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_95_current"
    )), 
    mode = "lines", 
    name = "Current distancing upper bound", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:be9762", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "95_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace2 <- list(
    fill = "tonexty", 
    line = list(
      color = "rgb(57, 157, 195)", 
      width = 0.5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_5_current"
    )), 
    mode = "lines", 
    name = "Current distancing lower bound", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:b983d3", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "5_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace3 <- list(
    line = list(
      color = "rgb(57, 157, 195)", 
      width = 3
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_median_current"
    )), 
    mode = "lines", 
    name = "Current distancing median estimate", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:e90755", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "median_current", sep = "_"), names(data1))],
    visible = TRUE
  )
  trace4 <- list(
    line = list(
      color = "rgb(214, 39, 40)", 
      width = 0.5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_95_reduction_20"
    )), 
    mode = "lines", 
    name = "20% reduction upper bound", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:de84a7", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "95_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL
  )
  trace5 <- list(
    fill = "tonexty", 
    line = list(
      color = "rgb(214, 39, 40)", 
      width = 0.5
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_5_reduction_20"
    )), 
    mode = "lines", 
    name = "20% reduction lower bound", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:09ceed", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "5_reduction_20", sep = "_"), names(data1))],
    stackgroup = NULL
  )
  trace6 <- list(
    line = list(
      color = "rgb(214, 39, 40)", 
      width = 3
    ), 
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_median_reduction_20"
    )), 
    mode = "lines", 
    name = "20% reduction median estimate", 
    type = "scatter", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data1$date,
    ysrc = "wyusuf:13:0ae7f1", 
    y = data1[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "median_reduction_20", sep = "_"), names(data1))],
    visible = TRUE, 
    stackgroup = NULL
  )
  trace7 <- list(
    meta = list(columnNames = list(
      x = "date", 
      y = "hosp_census_observed"
    )), 
    mode = "lines", 
    name = "Observed hospital census", 
    type = "bar", 
    xsrc = "wyusuf:13:d5d66b", 
    x = data2$date,
    ysrc = "wyusuf:13:5e5262", 
    y = data2[,grepl(paste(paste("^",as.character(parameter),sep = ""),
                           "observed", sep = "_"), names(data2))],
    stackgroup = NULL, 
    orientation = "v"
  )
  data <- list(trace1, trace2, trace3, trace4, trace5, trace6, trace7)
  layout <- list(
    title = list(text = "Projected census of COVID-19 hospitalizations at The Ottawa Hospital"), 
    xaxis = list(
      type = "date", 
      range = c("2020-03-23 12:00", "2020-10-09"), 
      title = list(text = "Date"), 
      autorange = TRUE
    ), 
    yaxis = list(
      type = "linear", 
      range = c(-75.35262219872224, 1438.0468001157224), 
      title = list(text = "Census (# of patients)"), 
      autorange = TRUE
    ), 
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
        colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#19d3f3", "#e763fa", "#fecb52", "#ffa15a", "#ff6692", "#FECB52"), 
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
    ), 
    hovermode = "closest", 
    showlegend = TRUE
  )
  p <- plot_ly()
  p <- add_trace(p, fill=trace1$fill, line=trace1$line, meta=trace1$meta, mode=trace1$mode, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, visible=trace1$visible)
  p <- add_trace(p, fill=trace2$fill, line=trace2$line, meta=trace2$meta, mode=trace2$mode, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, visible=trace2$visible)
  p <- add_trace(p, line=trace3$line, meta=trace3$meta, mode=trace3$mode, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, visible=trace3$visible)
  p <- add_trace(p, line=trace4$line, meta=trace4$meta, mode=trace4$mode, name=trace4$name, type=trace4$type, xsrc=trace4$xsrc, x=trace4$x, ysrc=trace4$ysrc, y=trace4$y, stackgroup=trace4$stackgroup)
  p <- add_trace(p, fill=trace5$fill, line=trace5$line, meta=trace5$meta, mode=trace5$mode, name=trace5$name, type=trace5$type, xsrc=trace5$xsrc, x=trace5$x, ysrc=trace5$ysrc, y=trace5$y, stackgroup=trace5$stackgroup)
  p <- add_trace(p, line=trace6$line, meta=trace6$meta, mode=trace6$mode, name=trace6$name, type=trace6$type, xsrc=trace6$xsrc, x=trace6$x, ysrc=trace6$ysrc, y=trace6$y, visible=trace6$visible, stackgroup=trace6$stackgroup)
  p <- add_trace(p, meta=trace7$meta, mode=trace7$mode, name=trace7$name, type=trace7$type, xsrc=trace7$xsrc, x=trace7$x, ysrc=trace7$ysrc, y=trace7$y, stackgroup=trace7$stackgroup, orientation=trace7$orientation)
  p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template, hovermode=layout$hovermode, showlegend=layout$showlegend)
  p
}
