# Import estimates CSV
ott_estimates <- read.csv(file.path(getwd(), "Ottawa_hospitalization_estimates_COVID.csv"))

# Load plotly
library(plotly)

# Plot projection of hospitalizations
hospitalized_50 <- ott_estimates$hospitalized_50
hospitalized_60 <- ott_estimates$hospitalized_60
hospitalized_70 <- ott_estimates$hospitalized_70
date <- ott_estimates$date

hosp_projections <- data.frame(date, hospitalized_50, hospitalized_60,
                               hospitalized_70)

hosp_fig <- plot_ly(hosp_projections, x = date, y = hospitalized_50,
                    name = "50% social distancing", type = 'scatter',
                    mode = "line+markers")
hosp_fig <- hosp_fig %>% add_trace(y = hospitalized_60,
                                   name = "60% social distancing",
                                   mode = "line+markers")