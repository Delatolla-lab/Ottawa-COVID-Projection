short_term_forecast <- function(data,
                                start_date,
                                end_date,
                                generation_time,
                                incubation_period,
                                reporting_delay,
                                output = "projections"){
  # Format dataset
  if(missing(end_date)) {
    end_date <- max(as.Date(data$date), na.rm = TRUE)
  }
  
  data_formatted <- data %>%
    filter(as.Date(date) >= as.Date(start_date)) %>%
    filter(as.Date(date) <= as.Date(end_date)) %>%
    select(date, observed_new_cases) %>%
    rename(confirm = observed_new_cases) %>%
    mutate(date = as.Date(date))

  # Run epinow2 sim 
  projections <-
      EpiNow2::epinow(reported_cases = data_formatted, 
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                      stan = stan_opts(cores = 4))
  # Extract output
  if(output == as.character("projections")){
    forecast <-
      projections[[1]][[2]] # Obtain summarized projections
  }  
  else if(output == as.character("estimates")){
    forecast <-
      projections[[3]][[3]] # Obtain numeric estimates
  }
  else if(output == as.character("both")){
    forecast <- list(
      projections[[1]][[2]],
      projections[[3]][[3]]
    )
  }  

  return(forecast)
}

short_term_plot <- function(projections,
                            obs_data,
                            forecast_type,
                            start_date = first(as.Date(projections$date)),
                            ylab,
                            title){
  # Filter data based on forecast type and remove 50% CI
  projections <- projections %>%
    filter(variable == as.character(forecast_type)) %>%
    select(-c(lower_50, upper_50, lower_20, upper_20))
  
  # set up CrI index
  CrIs <- extract_CrIs(projections)
  index <- 1
  alpha_per_CrI <- 0.6 / (length(CrIs) - 1)
  
  # Modify CI column names in dataset
  colnames(projections) <- reduce2(c("_", "0"), c(" ", "0%"),
                                   .init = colnames(projections),
                                   str_replace)
  # Set up ggplot object
  plot<- 
    ggplot(projections[as.Date(projections$date) >= as.Date(start_date),],
           aes(x = date, col = type, fill = type))
  
  # Add observed data if R is not specified
  if(forecast_type == as.character("infections")){
    plot <- plot +
      geom_col(data = 
                 obs_data[as.Date(obs_data$date) >= as.Date(start_date),],
               aes(x = as.Date(date),
                   y = observed_new_episodes,
                   text = paste("Observed cases:",
                                observed_new_episodes)),
               fill = "#008080", col = "white", alpha = 0.5,
               show.legend = FALSE, na.rm = TRUE)
  }
  else if(forecast_type == as.character("reported_cases")){
    plot <- plot +
      geom_col(data = 
                 obs_data[as.Date(obs_data$date) >= as.Date(start_date),],
               aes(x = as.Date(date),
                   y = observed_new_cases,
                   text = paste("Observed cases:",
                                observed_new_cases)),
               fill = "#008080", col = "white", alpha = 0.5,
               show.legend = FALSE, na.rm = TRUE)
  }
  
  # plot estimates
  plot <- plot +
    geom_vline(
      xintercept = 
        as.numeric(projections[projections$type == "estimate based on partial data"][date == max(date)]$date),
      linetype = 2)
  
  # plot median line
  plot <- plot +
    geom_line(aes(y = median),
              lwd = 0.9)
  
  # plot CrIs
  for (CrI in CrIs) {
    bottom <- paste("lower", paste0(CrI, "%"))
    top <-  paste("upper", paste0(CrI, "%"))
      plot <- plot +
        geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]), 
                             alpha = 0.2, size = 0.05)
    
  }
  
  # add plot theming
  plot <- plot +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(y = ylab, x = "Date", col = "Type", fill = "Type", title = title) +
    expand_limits(y = c(-0.4, 0.8)) + 
    scale_x_date(expand = c(0,0), date_breaks = "1 week",
                 date_labels = "%b %d") +
    scale_y_continuous(expand = c(0, 0)) 
  
  # Convert to plotly object
  plot <- plotly::ggplotly(plot, tooltip = c("date", "text", "mean",
                                             "lower 90%", "upper 90%"))
  
  
  # Set date display constraints 
  a <- as.numeric(as.Date(last(projections$date) - 40)) 
  b <- as.numeric(as.Date(last(projections$date)))
  
  # Format legend layout & add annotation
  plot <- plotly::layout(plot,
                         xaxis = list(range = c(a, b)),
                         legend = list(
                           #orientation = "h",
                           x = 0.02, y = 1
                         ),
                         annotations = list(
                           x = 1, y = -0.12, text = "*Shaded area represents the 90% credible region", 
                           showarrow = F, xref='paper', yref='paper', 
                           xanchor='right', yanchor='auto', xshift=0, yshift=0,
                           font=list(size=10)
                         ))
  
  return(plot)
}