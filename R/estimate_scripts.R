# Acute estimates from CHIME model
ott_projections$census_acute_care_current <- 
  ott_projections$census_ICU_p_acute_care_current - ott_projections$census_ICU_current

ott_projections$census_acute_care_50 <- 
  ott_projections$census_ICU_p_acute_care_50 - ott_projections$census_ICU_50

ott_projections$census_acute_care_70 <-
  ott_projections$census_ICU_p_acute_care_70 - ott_projections$census_ICU_70

ott_projections$new_acute_care_current <-
  ott_projections$new_ICU_p_acute_care_current - ott_projections$new_ICU_current

ott_projections$new_acute_care_50 <-
  ott_projections$new_ICU_p_acute_care_50 - ott_projections$new_ICU_50

ott_projections$new_acute_care_70 <-
  ott_projections$new_ICU_p_acute_care_70 - ott_projections$new_ICU_70

# Death estimates from OPH probabilities and data from CHIME model

## ICU & death in ICU probabilities
p_ICU_age_0_20 <- 0
p_ICU_age_20_44 <- 0.11
p_ICU_age_45_64 <- 0.22
p_ICU_age_65p <- 0.67
p_death_ICU_age_0_20 <- 0.2
p_death_ICU_age_20_44 <- 0.4 
p_death_ICU_age_45_64 <- 0.7
p_death_ICU_age_65p <- 0.8

## Set all NA to 0
ott_projections[is.na(ott_projections)] <- 0

## Daily death estimates
ott_projections$new_deaths_current_0_20 <- 
  ott_projections$new_ICU_current*p_ICU_age_0_20*p_death_ICU_age_0_20

ott_projections$new_deaths_current_20_44 <- 
  ott_projections$new_ICU_current*p_ICU_age_20_44*p_death_ICU_age_20_44

ott_projections$new_deaths_current_45_64 <- 
  ott_projections$new_ICU_current*p_ICU_age_45_64*p_death_ICU_age_45_64

ott_projections$new_deaths_current_65p <- 
  ott_projections$new_ICU_current*p_ICU_age_65p*p_death_ICU_age_65p

ott_projections$new_deaths_current <- 
  ott_projections$new_deaths_current_0_20 + ott_projections$new_deaths_current_20_44 +
  ott_projections$new_deaths_current_45_64 + ott_projections$new_deaths_current_65p

ott_projections$new_deaths_50_0_20 <- 
  ott_projections$new_ICU_50*p_ICU_age_0_20*p_death_ICU_age_0_20

ott_projections$new_deaths_50_20_44 <- 
  ott_projections$new_ICU_50*p_ICU_age_20_44*p_death_ICU_age_20_44

ott_projections$new_deaths_50_45_64 <- 
  ott_projections$new_ICU_50*p_ICU_age_45_64*p_death_ICU_age_45_64

ott_projections$new_deaths_50_65p <- 
  ott_projections$new_ICU_50*p_ICU_age_65p*p_death_ICU_age_65p

ott_projections$new_deaths_50 <- 
  ott_projections$new_deaths_50_0_20 + ott_projections$new_deaths_50_20_44 +
  ott_projections$new_deaths_50_45_64 + ott_projections$new_deaths_50_65p

ott_projections$new_deaths_70_0_20 <- 
  ott_projections$new_ICU_70*p_ICU_age_0_20*p_death_ICU_age_0_20

ott_projections$new_deaths_70_20_44 <- 
  ott_projections$new_ICU_70*p_ICU_age_20_44*p_death_ICU_age_20_44

ott_projections$new_deaths_70_45_64 <- 
  ott_projections$new_ICU_70*p_ICU_age_45_64*p_death_ICU_age_45_64

ott_projections$new_deaths_70_65p <- 
  ott_projections$new_ICU_70*p_ICU_age_65p*p_death_ICU_age_65p

ott_projections$new_deaths_70 <- 
  ott_projections$new_deaths_70_0_20 + ott_projections$new_deaths_70_20_44 +
  ott_projections$new_deaths_70_45_64 + ott_projections$new_deaths_70_65p

## Cumulative death estimates
library(dplyr)
ott_projections$cumulative_deaths_current <-
  last(na.omit(ott_observed$observed_cumulative_deaths)) +
  cumsum(ott_projections$new_deaths_current)

ott_projections$cumulative_deaths_50 <- 
  last(na.omit(ott_observed$observed_cumulative_deaths)) +
  cumsum(ott_projections$new_deaths_50)

ott_projections$cumulative_deaths_70 <-
  last(na.omit(ott_observed$observed_cumulative_deaths)) +
  cumsum(ott_projections$new_deaths_70)