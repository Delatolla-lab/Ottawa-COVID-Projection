peak_projections <- read.csv("Data/peak_projections.csv")

library(ggplot2)
p <- ggplot(peak_projections) + geom_col(aes(intervention, peak_hospital_census))