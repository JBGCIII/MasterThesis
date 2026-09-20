###############################################################################
###################### HOUSING TRANSMISSION MODEL - HABITUAL ##################
###############################################################################
#                           [1] MODEL SET UP
# 1. Load up data

bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")

model_ts <- ts(
  bvar_data,
  start = c(1996, 1),
  frequency = 4
)

#----------------------------------------------------------------------------#
# 2. Model with Housing Transmission variables (CPI omitted, Savings & DSR added)
housing_cols <- c(
  "unemployment_rate", "policy_rate", "house_price_real_log",
  "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
  "saving_rate", "cons_habitual_log"
)
housing_data <- as.matrix(model_ts[, housing_cols])


N_housing <- ncol(housing_data)

#----------------------------------------------------------------------------#
# 3. Variable Index Definitions
# Variable Index Definitions
unemp_idx   <- "unemployment_rate"
policy_idx  <- "policy_rate"
house_idx   <- "house_price_real_log"
dta_idx     <- "debt_to_asset_ratio"
dsr_idx     <- "dsr_value"
liquid_idx  <- "liquid_asset_to_income_ratio"
saving_idx  <- "saving_rate"
cons_idx    <- "cons_habitual_log"

#=============================================================================#
#                           [2] SIGN RESTRICTIONS
# 1. Length of IRF
H_total   <- 30
N_housing <- length(housing_cols)

#----------------------------------------------------------------------------#
# 2. Create array with named dimensions
sign_irf <- array(
  NA_real_,
  dim = c(N_housing, N_housing, H_total),
  dimnames = list(
    variable = housing_cols,  # Rows
    shock    = housing_cols,  # Columns
    horizon  = NULL
  )
)

#----------------------------------------------------------------------------#
# Shocks 
mp_shock      <- 1  # Monetary Policy Shock
macro_shock   <- 2  # Adverse Macro / Aggregate Demand Shock
housing_shock <- 3  # Adverse Housing Collateral / Wealth Shock

# Constrain restrictions for horizons 1 through 3 (Quarters 0, 1, 2)
for (h in 1:3) {
  # 1. Monetary Policy Shock (Rate UP -> Labor market weakens / Unemployment UP)
  sign_irf[policy_idx, mp_shock, h] <-  1
  sign_irf[unemp_idx,  mp_shock, h] <-  1  # Unemployment increases
 # sign_irf[dsr_idx,    mp_shock, h] <-  1  # Debt service ratio is left unrestricted

  # 2. Adverse Macro / Demand Shock (Unemployment UP -> Rate CUTS)
  sign_irf[unemp_idx,  macro_shock, h] <-  1  # Unemployment increases
  sign_irf[policy_idx, macro_shock, h] <- -1  # Policy rate cut in response

  # 3. Adverse Housing Market Shock (House Prices DOWN -> Balance sheet leverage UP)
  sign_irf[house_idx,  housing_shock, h] <- -1
  sign_irf[dta_idx,    housing_shock, h] <-  1
}
  # Target consumption, liquidity (LAI), and precautionary savings are left UNRESTRICTED!

# Vector of prior means for autoregressive parameters (FALSE = Random Walk I(1))
is_stationary <- rep(FALSE, N_housing)

#=============================================================================#
#              [3] MODEL SPECIFICATION AT DIFFERENT LAGS

raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
n_cores       <- max(1, parallel::detectCores() - 2)
set.seed(12345)

#------------------------------------------------------------------------------#
#                               Model 1 (p = 1)
#------------------------------------------------------------------------------#
spec_housing_one_habitual <- specify_bsvarSIGN$new(
  data         = housing_data,          
  p            = 1,                     
  sign_irf     = sign_irf,              
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 1,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                               Model 2 (p = 2)
#------------------------------------------------------------------------------#
spec_housing_two_habitual <- specify_bsvarSIGN$new(
  data         = housing_data,         
  p            = 2,                    
  sign_irf     = sign_irf,              
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                 
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 2,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                               Model 3 (p = 3)
#------------------------------------------------------------------------------#
spec_housing_three_habitual <- specify_bsvarSIGN$new(
  data         = housing_data,          
  p            = 3,                     
  sign_irf     = sign_irf,              
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 3,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                               Model 4 (p = 4) 
#------------------------------------------------------------------------------#
spec_housing_four_habitual <- specify_bsvarSIGN$new(
  data         = housing_data,         
  p            = 4,                     
  sign_irf     = sign_irf,              
  stationary   = is_stationary,        
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 4,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                               Model 5 (p = 5) 
#------------------------------------------------------------------------------#
spec_housing_five_habitual <- specify_bsvarSIGN$new(
  data         = housing_data,         
  p            = 5,                     
  sign_irf     = sign_irf,              
  stationary   = is_stationary,        
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 5,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#=============================================================================#
#                          [4] ESTIMATE HYPER-PARAMETERS

spec_housing_one_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_housing_two_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_housing_three_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_housing_four_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_housing_five_habitual$estimate_hyper(S = 5000, burn_in = 1000)

#=============================================================================#
#                          [5] RUN MODEL

estimate_housing_habitual_1 <- estimate(spec_housing_one_habitual, S = 4000, thin = 1)
estimate_housing_habitual_2 <- estimate(spec_housing_two_habitual, S = 4000, thin = 1)
estimate_housing_habitual_3 <- estimate(spec_housing_three_habitual, S = 4000, thin = 1)
estimate_housing_habitual_4 <- estimate(spec_housing_four_habitual, S = 4000, thin = 1)
estimate_housing_habitual_5 <- estimate(spec_housing_five_habitual, S = 4000, thin = 1)

#=============================================================================#
#                          [6] SAVE ESTIMATED MODELS
#=============================================================================#

dir.create("3_Model_Output/Model_Housing/Habitual", recursive = TRUE, showWarnings = FALSE)

saveRDS(estimate_housing_habitual_1, file = "3_Model_Output/Model_Housing/Habitual/housing_habitual_p1.rds")
saveRDS(estimate_housing_habitual_2, file = "3_Model_Output/Model_Housing/Habitual/housing_habitual_p2.rds")
saveRDS(estimate_housing_habitual_3, file = "3_Model_Output/Model_Housing/Habitual/housing_habitual_p3.rds")
saveRDS(estimate_housing_habitual_4, file = "3_Model_Output/Model_Housing/Habitual/housing_habitual_p4.rds")
saveRDS(estimate_housing_habitual_5, file = "3_Model_Output/Model_Housing/Habitual/housing_habitual_p5.rds")

#=============================================================================#
#                          [7] STABILITY DIAGNOSTIC
#=============================================================================#

ev_one   <- check_posterior_stability(estimate_housing_habitual_1, p = 1)
ev_two   <- check_posterior_stability(estimate_housing_habitual_2, p = 2)
ev_three <- check_posterior_stability(estimate_housing_habitual_3, p = 3)
ev_four  <- check_posterior_stability(estimate_housing_habitual_4, p = 4)
ev_five  <- check_posterior_stability(estimate_housing_habitual_5, p = 5)

stability_diagnostics <- data.frame(
  Model = paste0("Model ", 1:5),
  Pct_Stable = c(
    mean(ev_one < 1) * 100,
    mean(ev_two < 1) * 100,
    mean(ev_three < 1) * 100,
    mean(ev_four < 1) * 100,
    mean(ev_five < 1) * 100
  ),
  Median_Rho = c(
    median(ev_one),
    median(ev_two),
    median(ev_three),
    median(ev_four),
    median(ev_five)
  ),
  P95_Rho = c(
    quantile(ev_one, .95),
    quantile(ev_two, .95),
    quantile(ev_three, .95),
    quantile(ev_four, .95),
    quantile(ev_five, .95)
  ),
  Max_Rho = c(
    max(ev_one),
    max(ev_two),
    max(ev_three),
    max(ev_four),
    max(ev_five)
  )
)

write.csv(stability_diagnostics, file = "3_Model_Output/Model_Housing/Habitual/posterior_stability_summary.csv", row.names = FALSE)
