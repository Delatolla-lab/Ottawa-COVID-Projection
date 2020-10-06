# Generate case projections based on model fit, days to project from last observation,
# days from last observation where transmission changes, and percentage change in transmission
proj_generation <- function(fit, lut, episode_data, project_to,
                            day_start_reduction,
                            pct_change){
  
  # Change from current transmission (1.1 = 10% increase in transmission,
  # 0.9 = 10% decrease in transmission)
  current <- 1
  reduction <- 1 - (pct_change/100)
  increase <- 1 + (pct_change/100)
  
  # Set days to project based on episode data & projection end date
  days_project <- as.numeric(as.Date(project_to) - last(episode_data$date))
  
  # Run projection for current transmission
  proj_current <- project_seir(
    fit,
    iter = 1:50,
    forecast_days = days_project,
    f_fixed_start = max(fit$days) + day_start_reduction,
    f_multi = rep(current, days_project - day_start_reduction + 1), 
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
case_projection_data <- function(proj_list, pct_change){
  # Create transmission column to specify if it's current transmission,
  # increase, or reduction in transmission
  proj_current <- proj_list[[1]] %>%
    mutate(Transmission = "Current transmission")
  proj_reduction <- proj_list[[2]] %>%
    mutate(Transmission = 
             paste("What if transmission decreased by", paste(pct_change, "%?", sep = ""), sep = " "))
  proj_increase <- proj_list[[3]] %>%
    mutate(Transmission = 
             paste("What if transmission increased by", paste(pct_change, "%?", sep = ""), sep = " "))
  proj_all <- proj_current %>%
    bind_rows(proj_increase) %>%
    bind_rows(proj_reduction) %>%
    select(date, everything() )%>%
    rename(
      estimate_0.05 = "y_rep_0.05",
      estimate_0.25 = "y_rep_0.25",
      estimate_0.75 = "y_rep_0.75",
      estimate_0.95 = "y_rep_0.95",
      median_estimate = "mu_0.50"
    )
  proj_all <- as.data.frame(proj_all)
  proj_all$Transmission <-
    factor(proj_all$Transmission,
           levels = c(
             "Current transmission",
             paste("What if transmission decreased by", paste(pct_change, "%?", sep = ""), sep = " "),
             paste("What if transmission increased by", paste(pct_change, "%?", sep = ""), sep = " ")
           ))
  return(proj_all)
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
  
  # Create error messages for incorrect input parameters
  if (!value_column %in% names(obs_dat)) {
    stop(glue("`obs_dat` must contain a column `{value_column}` that contains the reported case counts."), call. = FALSE)
  }
  if (!date_column %in% names(obs_dat)) {
    stop(glue("`obs_dat` must contain a column named `{date_column}` that contains the numeric day (or date)."), call. = FALSE)
  }
  if (!date_column %in% names(pred_dat)) {
    stop(glue("`pred_dat` must contain a column named `{date_column}` that contains the numeric day (or date)."), call. = FALSE)
  }
  
  # Create temporary range limit object
  tmp_data <- pred_dat[pred_dat$Transmission == "Current transmission", ]
  tmp <- 0.95*max(tmp_data$median_estimate)
  
  g <- ggplot(data = pred_dat, aes_string(x = date_column)) +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == "Current transmission", ],
                aes(ymin = estimate_0.05, ymax = estimate_0.95, text = "Current transmission"),
                alpha = 0.2, fill = current_col
    )  +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == "Current transmission", ],
                aes(ymin = estimate_0.25, ymax = estimate_0.75, text = "Current transmission"),
                alpha = 0.2, fill = current_col
    ) +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == paste("What if transmission decreased by", paste(pct_change, "%?", sep = ""), sep = " "), ],
                aes(ymin = estimate_0.05, ymax = estimate_0.95, text = paste(paste(pct_change, "%", sep = ""),"reduction",
                                                                       sep = " ")),
                alpha = 0.2, fill = reduction_col
    ) +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == paste("What if transmission decreased by", paste(pct_change, "%?", sep = ""), sep = " "), ],
                aes(ymin = estimate_0.25, ymax = estimate_0.75, text = paste(paste(pct_change, "%", sep = ""),"reduction",
                                                                       sep = " ")),
                alpha = 0.2, fill = reduction_col
    ) +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == paste("What if transmission increased by", paste(pct_change, "%?", sep = ""), sep = " "), ],
                aes(ymin = estimate_0.05, ymax = estimate_0.95, text = paste(paste(pct_change, "%", sep = ""),"increase",
                                                                       sep = " ")),
                alpha = 0.2, fill = increase_col
    ) +
    geom_ribbon(data = pred_dat[pred_dat$Transmission == paste("What if transmission increased by", paste(pct_change, "%?", sep = ""), sep = " "), ],
                aes(ymin = estimate_0.25, ymax = estimate_0.75, text = paste(paste(pct_change, "%", sep = ""),"increase",
                                                                       sep = " ")),
                alpha = 0.2, fill = increase_col
    )
  g <- g +
    geom_line(
      data = pred_dat,
      aes(y = median_estimate,
          color = Transmission),
      lwd = 0.9) +
    coord_cartesian(expand = FALSE, xlim = range(pred_dat[[date_column]]),
                    ylim = range(c(0,tmp))) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey"),
      legend.title = element_blank()
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y") +
    scale_color_manual(
      name = "Transmission",
      values = c(current_col,
                 reduction_col,
                 increase_col)
    ) +
    scale_y_continuous(
      breaks = seq(0, tmp, by = 100)
    )
  g <- g +
    geom_col(
      data = obs_dat,
      fill = current_col, inherit.aes = FALSE,
      aes_string(x = date_column, y = value_column),
      width = 1
    )
  
  if (max(pred_dat[["data_type"]]) > 1) g <- g + facet_wrap(~data_type)
  g_plot <- ggplotly(g) %>%
    layout(legend = list(x = 0.02, y = 1),
           annotations = list(
             x = 1, y = -0.12, text = "*Shaded area represents the 95% credible region", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=10)
           ))
  return(g_plot)
}