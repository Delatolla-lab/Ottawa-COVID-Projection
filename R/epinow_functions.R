short_term_forecast <- function(data,
                                input,
                                input_multiplier = 1,
                                start_date,
                                end_date,
                                omit_last_date = FALSE,
                                generation_time,
                                incubation_period,
                                reporting_delay,
                                horizon = 14,
                                rw = 7,
                                gp = NULL,
                                output = "projections",
                                CrI = c(0.2, 0.5, 0.75, 0.9)
                                
){
  # Format dataset
  if(missing(end_date)) {
    end_date <- max(as.Date(data$date), na.rm = TRUE)
  }
  
  data_formatted <- data %>%
    filter(as.Date(date) >= as.Date(start_date)) %>%
    filter(as.Date(date) <= as.Date(end_date)) %>%
    select(date, as.character(input)) %>%
    rename(confirm = as.character(input)) %>%
    mutate(date = as.Date(date),
           confirm = as.integer(confirm * input_multiplier))
  
  if(isTRUE(omit_last_date)){
    data_formatted <- data_formatted[data_formatted$date < as.Date(end_date),]
  }
  
  # Run epinow2 sim 
  if(is.null(reporting_delay)){
    projections <-
      EpiNow2::epinow(reported_cases = data_formatted, 
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = rw),
                      stan = stan_opts(cores = 4),
                      gp = gp, horizon = horizon,
                      CrIs = CrI)
  }
  else{
    projections <-
      EpiNow2::epinow(reported_cases = data_formatted, 
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = rw),
                      stan = stan_opts(cores = 4),
                      gp = gp, horizon = horizon)
  }
  if(output == as.character("projections")){
    forecast <-
      # Obtain summarized projections
      projections[[1]][[2]] 
  }  
  else if(output == as.character("estimates")){
    growth_measures <- cbind(projections[[3]][[1]], projections[[3]][[2]])
    colnames(growth_measures) <- c("measure", "estimate")
    growth_measures[2, "measure"] <- "Expected change in viral amount"
    growth_measures <- growth_measures[-1,]
    forecast <- list()
    forecast[["Growth summary"]] <- growth_measures
    forecast[["Growth estimates"]] <- projections[[3]][[3]]
  }
  else if(output == as.character("both")){
    growth_measures <- cbind(projections[[3]][[1]], projections[[3]][[2]])
    colnames(growth_measures) <- c("measure", "estimate")
    growth_measures[2, "measure"] <- "Expected change in viral amount"
    growth_measures <- growth_measures[-1,]
    forecast <- list()
    forecast[["Forecast"]] <- projections[[1]][[2]]
    forecast[["Growth summary"]] <- growth_measures
    forecast[["Growth estimates"]] <- projections[[3]][[3]]
  }  
  
  return(forecast)
}

short_term_plot <- function(interval_num=40,
                            projections,
                            levels = c("historic", "forecast"),
                            obs_data,
                            obs_column,
                            forecast_type,
                            start_date = first(as.Date(projections$date)),
                            ylab,
                            title,
                            CrI = 75, # credible interval percentage to plot. CrI should be present in forecast.
                            scale = FALSE,
                            tick_period = "1 week",
                            tick_labels_date = "%b %d",
                            annotation_text = "*Shaded area represents the 90% credible region"
                            
                            
){
  # Stop function if CrI not present in forecast
  cr_pct <- CrI/10
  cr_upper <- paste0("upper_", CrI)
  cr_lower <- paste0("lower_", CrI)
  
  if(!(cr_upper %in% colnames(projections)) &&
     !(cr_lower %in% colnames(projections))){
    stop("The ",paste0(CrI, "% "), "credible interval is not included in the projections.")
  }
  
  
  # Filter data based on forecast type and remove 50% CI
  projections <- projections %>%
    filter(variable == as.character(forecast_type)) %>%
    select(-c(lower_50, upper_50, lower_20, upper_20, lower_90, upper_90))
  
  # Set types to levels indicated in function call
  projections$type[projections$date <= as.Date(last(obs_data$date))] <-
    as.character(levels[[1]])
  
  projections$type[projections$date > as.Date(last(obs_data$date))] <-
    as.character(levels[[2]])
  
  projections$type <- factor(projections$type, levels =
                               c(as.character(levels[[1]]), as.character(levels[[2]])))
  
  # set up CrI index
  CrIs <- extract_CrIs(projections)
  index <- 1
  alpha_per_CrI <- 0.6 / (length(CrIs) - 1)
  
  # Set up ggplot object
  plot<- 
    ggplot(projections[as.Date(projections$date) >= as.Date(start_date),],
           aes(x = date, col = type, fill = type))
  
  # Add observed data if R or growth rate is not specified
  obs_plot <- filter(obs_data, as.Date(date) >= start_date)
  y_col <- obs_plot[[as.character(obs_column)]]
  if(forecast_type != as.character("R") &
     forecast_type != as.character("growth_rate")){
    plot <- plot +
      geom_col(data = 
                 obs_plot[as.Date(obs_plot$date) >= as.Date(start_date),],
               aes(x = as.Date(date),
                   y = y_col,
                   text = paste("Observed:",
                                y_col)),
               fill = "#008080", col = "white", alpha = 0.5,
               show.legend = FALSE, na.rm = TRUE)
  }
  
  # plot v line for last observed date
  historic <- projections[projections$type == levels[[1]],]
  plot <- plot +
    geom_vline(
      xintercept = 
        as.numeric(last(historic$date)),
      linetype = 2)
  
  # plot median line
  plot <- plot +
    geom_line(aes(y = median),
              lwd = 0.9)
  
  # plot CrIs
  for (CrI in CrIs) {
    bottom <- paste0("lower_", CrI)
    top <-  paste0("upper_", CrI)
    plot <- plot +
      geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]), 
                  alpha = 0.2, size = 0.05)
    
  }
  
  # Set custom palette
  palette <- brewer.pal(name = "Dark2", n = 8)[c(1,3)]
  
  # add plot theming
  plot <- plot +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    labs(y = ylab, x = "Date", col = "Type", fill = "Type", title = title) +
    expand_limits(y = c(-0.4, 0.8)) + 
    scale_x_date(expand = c(0,0), date_breaks = tick_period,
                 date_labels = tick_labels_date) +
    scale_y_continuous(expand = c(0, 0)) 
  
  # Convert to plotly object
  plot <- plotly::ggplotly(plot, tooltip = c("date", "text", "median",
                                             "lower 75%", "upper 75%"))
  
  
  # Set date display constraints
  if(as.numeric(as.Date(first(projections$date))) > as.Date(last(projections$date) - interval_num)){
    a <- as.numeric(as.Date(first(projections$date)))
  }
  else{
    a <- as.numeric(as.Date(last(projections$date) - interval_num)) 
  }
  b <- as.numeric(as.Date(last(projections$date)))
  
  # Format legend layout & add annotation
  plot <- plotly::layout(plot,
                         xaxis = list( 
                           range = c(a,b)), 
                         legend = list(
                           #orientation = "h",
                           x = 0.02, y = 1
                         ),
                         annotations = list(
                           x = 1, y = -.18, text = annotation_text, 
                           showarrow = F, xref='paper', yref='paper', 
                           xanchor='right', yanchor='auto', xshift=0, yshift=0,
                           font=list(size=10)
                         ),
                         dragmode = "pan"
  )
  
  
  
  if(isTRUE(scale)){
    tmp <- 1.75*max(projections$mean, na.rm = TRUE)
    plot <- layout(plot, 
                   yaxis = list(range = c(0, tmp)))
  }
  
  return(plot)
}