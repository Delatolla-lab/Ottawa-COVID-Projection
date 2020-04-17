source("../R/lag_replacement.R")
source("../R/observed_data.R")

## Effective physical distancing estimates
observed_dt <- expected_values[[3]]$expected_val
Tc <- 7 # mean generation time
gamma <- 1/Tc