short_term_forecast <- function(data,
                                start_date,
                                generation_time,
                                incubation_period,
                                reporting_delay){
  # Format dataset
  data_formatted <- data %>%
    filter(as.Date(date) >= as.Date(start_date)) %>%
    select(date, observed_new_episodes) %>%
    rename(confirm = observed_new_episodes) %>%
    mutate(date = as.Date(date))
  # Run epinow2 sim 
    projections <-
      EpiNow2::epinow(reported_cases = data_formatted, 
                      generation_time = generation_time,
                      delays = list(incubation_period, reporting_delay))
    projections <-
      projections[[1]][[2]] # Obtain summarized estimates

  return(projections)
}

short_term_plot <- function(projections,
                            obs_data,
                            forecast_type,
                            start_date = first(as.Date(projections$date)),
                            ylab,
                            title){
  # Filter data based on forecast type
  projections <- projections %>%
    filter(variable == as.character(forecast_type))
  # Set up ggplot object
  plot<- 
    ggplot(projections[as.Date(projections$date) >= as.Date(start_date),],
           aes(x = date, col = type, fill = type))
  
  # Add observed data if R is not specified
  if(forecast_type != as.character("R")){
    plot <- plot +
      geom_col(data = 
                 obs_data[as.Date(obs_data$date) >= as.Date(start_date),],
               aes(x = as.Date(date),
                   y = observed_new_episodes),
               fill = "#008080", col = "white", alpha = 0.25,
               show.legend = FALSE, na.rm = TRUE)
  }
  
  # plot estimates
  plot <- plot +
    geom_vline(
      xintercept = 
        as.numeric(projections[projections$type == "estimate based on partial data"][date == max(date)]$date),
      linetype = 2)
  
  # plot CrIs
  CrIs <- extract_CrIs(projections)
  index <- 1
  alpha_per_CrI <- 0.6 / (length(CrIs) - 1)
  for (CrI in CrIs) {
    bottom <- paste0("lower_", CrI)
    top <-  paste0("upper_", CrI)
    if (index == 1) {
      plot <- plot +
        geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]), 
                             alpha = 0.2, size = 0.05)
    }else{
      plot <- plot +
        geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]],
                                          col = NULL), 
                             alpha = alpha_per_CrI)
    }
    index <- index + 1
  }
  
  # add plot theming
  plot <- plot +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey"),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(y = ylab, x = "Date", col = "Type", fill = "Type", title = title) +
    expand_limits(y = c(-0.4, 0.8)) + 
    scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0)) 
  
  # Convert to plotly object
  plot <- plotly::ggplotly(plot)
  
  return(plot)
}