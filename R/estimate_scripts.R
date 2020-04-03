# Acute estimates from CHIME model
ott_projections$census_acute_care_50 <- 
  ott_projections$census_ICU_p_acute_care_50 - ott_projections$census_ICU_50

ott_projections$census_acute_care_60 <- 
  ott_projections$census_ICU_p_acute_care_60 - ott_projections$census_ICU_60

ott_projections$census_acute_care_70 <-
  ott_projections$census_ICU_p_acute_care_70 - ott_projections$census_ICU_70

ott_projections$new_acute_care_50 <-
  ott_projections$new_ICU_p_acute_care_50 - ott_projections$new_ICU_50

ott_projections$new_acute_care_60 <-
  ott_projections$new_ICU_p_acute_care_60 - ott_projections$new_ICU_60

ott_projections$new_acute_care_70 <-
  ott_projections$new_ICU_p_acute_care_70 - ott_projections$new_ICU_70

# Death estimates from OPH probabilities and data from CHIME model

## ICU & death from ICU probabilities
#p_ICU_age_0_20 <- 0
#p_ICU_age_20_44 <- 0.11
#p_ICU_age_45_64 <- 0.22
#p_ICU_age_65p <- 0.67
#p_death_ICU_age_0_20 <- 0.2
#p_death_ICU_age_20_44 <- 0.4 
#p_death_ICU_age_45_64 <- 0.7
#p_death_ICU_age_65p <- 0.8

## Daily death estimates
#ott_projections$new_deaths_50_0_20 <- 
  #ott_projections$new_ICU_50*p_ICU_age_0_20*p_death_ICU_age_0_20

#ott_projections$new_deaths_50_20_44 <- 
  #ott_projections$new_ICU_50*p_ICU_age_20_44*p_death_ICU_age_20_44

#ott_projections$new_deaths_50_45_64 <- 
  #ott_projections$new_ICU_50*p_ICU_age_45_64*p_death_ICU_age_45_64

#ott_projections$new_deaths_50_65p <- 
  #ott_projections$new_ICU_50*p_ICU_age_65p*p_death_ICU_age_65p

#ott_projections$new_deaths_50 <- 
  #ott_projections$new_deaths_50_0_20 + ott_projections$new_deaths_50_20_44 +
  #ott_projections$new_deaths_50_45_64 + ott_projections$new_deaths_50_65p

#ott_projections$new_deaths_60_0_20 <- 
  #ott_projections$new_ICU_60*p_ICU_age_0_20*p_death_ICU_age_0_20

#ott_projections$new_deaths_60_20_44 <- 
  #ott_projections$new_ICU_60*p_ICU_age_20_44*p_death_ICU_age_20_44

#ott_projections$new_deaths_60_45_64 <- 
  #ott_projections$new_ICU_60*p_ICU_age_45_64*p_death_ICU_age_45_64

#ott_projections$new_deaths_60_65p <- 
  #ott_projections$new_ICU_60*p_ICU_age_65p*p_death_ICU_age_65p

#ott_projections$new_deaths_60 <- 
  #ott_projections$new_deaths_60_0_20 + ott_projections$new_deaths_60_20_44 +
  #ott_projections$new_deaths_60_45_64 + ott_projections$new_deaths_60_65p

#ott_projections$new_deaths_70_0_20 <- 
  #ott_projections$new_ICU_70*p_ICU_age_0_20*p_death_ICU_age_0_20

#ott_projections$new_deaths_70_20_44 <- 
  #ott_projections$new_ICU_60*p_ICU_age_20_44*p_death_ICU_age_20_44

#ott_projections$new_deaths_70_45_64 <- 
  #ott_projections$new_ICU_70*p_ICU_age_45_64*p_death_ICU_age_45_64

#ott_projections$new_deaths_70_65p <- 
  #ott_projections$new_ICU_70*p_ICU_age_65p*p_death_ICU_age_65p

#ott_projections$new_deaths_70 <- 
  #ott_projections$new_deaths_70_0_20 + ott_projections$new_deaths_70_20_44 +
  #ott_projections$new_deaths_70_45_64 + ott_projections$new_deaths_70_65p

## Cumulative death estimates
#ott_projections$cumulative_deaths_50 <- 
  #cumsum(ott_projections$new_deaths_50)

#ott_projections$cumulative_deaths_60 <-
  #cumsum(ott_projections$new_deaths_60)

#ott_projections$cumulative_deaths_70 <-
  #cumsum(ott_projections$new_deaths_70)