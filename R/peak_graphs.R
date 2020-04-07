peak_projections <- read.csv("Data/peak_projections.csv")

library(ggplot2)
g <- ggplot(peak_projections, aes(intervention, peak_hospital_census)) + geom_col()
g
