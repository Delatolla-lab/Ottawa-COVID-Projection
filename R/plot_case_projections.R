case_projection <- function(pred_dat, obs_dat, col = "#488AC2",
                            value_column = "value", date_column = "day",
                            xlab = "Date",
                            ylab = "Daily cases", 
                            title) {
  if (!value_column %in% names(obs_dat)) {
    stop(glue("`obs_dat` must contain a column `{value_column}` that contains the reported case counts."), call. = FALSE)
  }
  if (!date_column %in% names(obs_dat)) {
    stop(glue("`obs_dat` must contain a column named `{date_column}` that contains the numeric day (or date)."), call. = FALSE)
  }
  if (!date_column %in% names(pred_dat)) {
    stop(glue("`pred_dat` must contain a column named `{date_column}` that contains the numeric day (or date)."), call. = FALSE)
  }
  g <- ggplot(pred_dat, aes_string(x = date_column)) +
    geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95"),
                alpha = 0.2, fill = col
    ) +
    geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75"),
                alpha = 0.2, fill = col
    ) +
    geom_line(aes_string(y = "mu_0.50"), lwd = 0.9, col = col) +
    coord_cartesian(expand = FALSE, xlim = range(pred_dat[[date_column]])) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey")
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y")
    
  g <- g +
    geom_line(
      data = obs_dat,
      col = "black", inherit.aes = FALSE,
      aes_string(x = date_column, y = value_column),
      lwd = 0.35, alpha = 0.9
    ) +
    geom_point(
      data = obs_dat,
      col = "grey30", inherit.aes = FALSE,
      aes_string(x = date_column, y = value_column),
      pch = 21, fill = "grey95", size = 1.25
    )
  
  if (max(pred_dat[["data_type"]]) > 1) g <- g + facet_wrap(~data_type)
  g_plot <- ggplotly(g)
  return(g_plot)
}