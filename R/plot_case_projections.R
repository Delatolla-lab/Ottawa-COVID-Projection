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
case_projection_data <- function(proj_list, obs_data){
  for(i in 1:length(proj_list)){
    proj_list[[i]] <- proj_list[[i]][c("mu_0.05", "mu_mean", "mu_0.95", "date")]
  }
  proj_current <- proj_list[[1]] %>%
    rename(new_cases_5_current = "mu_0.05",
           new_cases_95_current = "mu_0.95",
           new_cases_median_current = "mu_mean"
           ) %>%
    relocate(new_cases_median_current, .after = new_cases_95_current)
  proj_reduction <- proj_list[[2]] %>%
    rename(
      new_cases_5_reduction_10 = "mu_0.05",
      new_cases_median_reduction_10 = "mu_mean",
      new_cases_95_reduction_10 = "mu_0.95"
      )%>%
    relocate(new_cases_median_reduction_10, .after = new_cases_95_reduction_10)
  proj_increase <- proj_list[[3]] %>%
    rename(
      new_cases_5_increase_10 = "mu_0.05",
      new_cases_median_increase_10 = "mu_mean",
      new_cases_95_increase_10 = "mu_0.95"
    )%>%
    relocate(new_cases_median_increase_10, .after = new_cases_95_increase_10)
  obs_data <- obs_data[c("date", "observed_new_episodes")] %>%
    rename(new_cases_observed = "observed_new_episodes")
  proj_all <- proj_current %>%
    full_join(proj_increase, by = "date") %>%
    full_join(proj_reduction, by = "date") %>%
    full_join(obs_data, by = "date") %>%
    select(date, everything())
  return(as.data.frame(proj_all))
}

case_projection_plot <- function(pred_dat, obs_dat, col = "#488AC2",
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