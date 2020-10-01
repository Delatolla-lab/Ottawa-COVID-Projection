# Generate case projections
proj_generation <- function(fit, lut, days_project, day_start_reduction,
                            pct_change){
  current <- 1
  reduction <- 1 - (pct_change/100)
  increase <- 1 + (pct_change/100)
  
  # Run projection for current transmission
  proj_current <- project_seir(
    fit,
    iter = 1:50,
    forecast_days = days_project,
    f_fixed_start = max(fit$days) + day_start_reduction,
    f_multi = rep(current, days_project - day_start_reduction + 1), 
    # Change from current transmission (1.1 = 10% increase in transmission, 0.9 = 10% decrease in transmission)
    f_multi_seg = 3 # which f segment to use
  )
  tidy_proj_current <- tidy_seir(proj_current, resample_y_rep = 30)
  tidy_proj_current <- left_join(tidy_proj_current, lut, by = "day")
  
  # Run projection for reduced transmission
  proj_reduction <- project_seir(
    fit,
    iter = 1:50,
    forecast_days = days_project,
    f_fixed_start = max(fit$days) + day_start_reduction,
    f_multi = rep(reduction, days_project - day_start_reduction + 1), 
    # Change from current transmission (1.1 = 10% increase in transmission, 0.9 = 10% decrease in transmission)
    f_multi_seg = 3 # which f segment to use
  )
  tidy_proj_reduction <- tidy_seir(proj_reduction, resample_y_rep = 30)
  tidy_proj_reduction <- left_join(tidy_proj_reduction, lut, by = "day")
  
  # Run projection for increase transmission
  proj_increase <- project_seir(
    fit,
    iter = 1:50,
    forecast_days = days_project,
    f_fixed_start = max(fit$days) + day_start_reduction,
    f_multi = rep(increase, days_project - day_start_reduction + 1), 
    # Change from current transmission (1.1 = 10% increase in transmission, 0.9 = 10% decrease in transmission)
    f_multi_seg = 3 # which f segment to use
  )
  tidy_proj_increase <- tidy_seir(proj_increase, resample_y_rep = 30)
  tidy_proj_increase <- left_join(tidy_proj_increase, lut, by = "day")
  
  # Generate list of projections
  proj_list <- list(tidy_proj_current,
                    tidy_proj_reduction,
                    tidy_proj_increase)
  return(proj_list)
}

# Prepare projection data for visualization
case_projection_data <- function(proj_list){
  proj_current <- proj_list[[1]] %>%
    rename_at(vars(-date), function(x) paste0(x, "_current"))
  proj_reduction <- proj_list[[2]] %>%
    rename_at(vars(-date), function(x) paste0(x, "_reduction"))
  proj_increase <- proj_list[[3]] %>%
    rename_at(vars(-date), function(x) paste0(x, "_increase"))
  proj_all <- proj_current %>%
    full_join(proj_increase, by = "date") %>%
    full_join(proj_reduction, by = "date") %>%
    select(date, everything())
  return(as.data.frame(proj_all))
}

# Visualize projections
case_projection_plot <- function(pred_dat, obs_dat, current_col, 
                                 reduction_col = NULL, increase_col = NULL,
                                 pct_change = NULL,
                                 value_column = "value",
                                 date_column = "day",
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
  if(!is.null(reduction_col) & !is.null(increase_col) & !is.null(pct_change)){
    g <- ggplot(pred_dat, aes_string(x = date_column)) +
      geom_ribbon(aes_string(ymin = "y_rep_0.05_current", ymax = "y_rep_0.95_current"),
                  alpha = 0.2, fill = current_col
      ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.25_current", ymax = "y_rep_0.75_current"),
                  alpha = 0.2, fill = current_col
      ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.05_reduction", ymax = "y_rep_0.95_reduction"),
                  alpha = 0.2, fill = reduction_col
      ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.25_reduction", ymax = "y_rep_0.75_reduction"),
                  alpha = 0.2, fill = reduction_col
      ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.05_increase", ymax = "y_rep_0.95_increase"),
                  alpha = 0.2, fill = increase_col
      ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.25_increase", ymax = "y_rep_0.75_increase"),
                  alpha = 0.2, fill = increase_col
      ) +
      geom_line(aes_string(y = "mu_0.50_current"),
                lwd = 0.9,
                color = current_col) +
      geom_line(aes_string(y = "mu_0.50_reduction"),
                lwd = 0.9,
                color = reduction_col) +
      geom_line(aes_string(y = "mu_0.50_increase"),
                lwd = 0.9,
                color = increase_col) +
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
        date_labels = "%b %Y")  +
      scale_color_manual(values = c(current_col = "Current transmission",
                                    reduction_col = paste("What if transmission decreased by", paste(pct_change, "%?", sep = "")),
                                    increase_col = paste("What if transmission increased by", paste(pct_change, "%?", sep = "")))
      )
  }
  else{
    g <- ggplot(pred_dat, aes_string(x = date_column)) +
      geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95"),
                alpha = 0.2, fill = current_col,
                ) +
      geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75"),
                  alpha = 0.2, fill = current_col,
                ) +
      geom_line(aes_string(y = "mu_0.50"), lwd = 0.9, col = current_col) +
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
  }
    
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