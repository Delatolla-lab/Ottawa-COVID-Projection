peak_projections <- read.csv("Data/peak_projections.csv")
peak_projections$intervention <- 
  factor(peak_projections$intervention, levels = c("No prevention & control",
                                                   "50% physical distancing", "70% physical distancing"))

library(ggplot2)
h <- ggplot(peak_projections, aes(intervention, peak_hospital_census)) + geom_col(fill = "#3182bd") +
  labs(title = "Projected peak hospitalizations in Ottawa by intervention", x = "Scenario",
       y = "Census (# of beds)") + geom_text(aes(label = peak_hospital_census_date), vjust = -0.5) + 
  theme(panel.background = element_rect(fill = 'white'))
h