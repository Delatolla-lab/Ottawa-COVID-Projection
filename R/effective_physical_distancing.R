# Load observed data
tmp <- calc_expected_values_for_n_weeks(ott_observed, number_weeks = 1)

# Baseline parameters
baseline_dt <- 4
Tc <- 7 
gamma <- 1/Tc 
g_baseline <- 2^(1/baseline_dt) - 1
R_naught <- 1 + g_baseline*Tc
beta_baseline <- R_naught*gamma


# Observed parameters
observed_dt <- as.numeric(tmp[[2]]) 
g_observed <- 2^(1/observed_dt) - 1 
Rt <- 1 + g_observed*Tc 
beta_observed <- Rt*gamma 

# Calculating effective physical distancing
effective_pd <- (1 - ((g_observed + gamma)/beta_baseline)) * 100