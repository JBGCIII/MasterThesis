###############################################################################
############################# CONSUMPTION HABITUAL ############################
###############################################################################
#                               [1] MODEL SET UP
# 1. Load up data

bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")

model_ts <- ts(
  bvar_data,
  start = c(1996, 1),
  frequency = 4
)

#----------------------------------------------------------------------------#
# 2. Model with domestic variables only
domestic_cols <- c(
  "gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_habitual_log",
  "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
  "saving_rate", "kix_real_log"
)
domestic_data <- as.matrix(model_ts[, domestic_cols])

N_dom <- ncol(domestic_data)

#----------------------------------------------------------------------------#
# 3. Create an index, otherwise Bsvarsign will not recognize it
gdp_idx <- "gdp_se_log"
unemp_idx <- "unemployment_rate"
cpi_idx <- "cpi_log"
policy_idx <- "policy_rate"
cons_idx <- "cons_habitual_log"
dta_idx <- "debt_to_asset_ratio"
dsr_idx <- "dsr_value"
liquid_idx <- "liquid_asset_to_income_ratio"
saving_idx <- "saving_rate"
kix_idx <- "kix_real_log"

#----------------------------------------------------------------------------#
c(
  gdp = gdp_idx,
  unemployment = unemp_idx,
  cpi = cpi_idx,
  policy = policy_idx,
  consumption = cons_idx,
  dta = dta_idx,
  dsr = dsr_idx,
  liquid = liquid_idx,
  saving = saving_idx,
  exchange_rate = kix_idx
)

#=============================================================================#
#                               [2] SIGN RESTRICTIONS
# 1. Lenght of IRF
H_total <- 30
N_dom   <- length(domestic_cols) 
#----------------------------------------------------------------------------#
# 2. Create array with named dimensions
sign_irf <- array(
  NA_real_,
  dim = c(N_dom, N_dom, H_total),
  dimnames = list(
    variable = domestic_cols,  # Rows
    shock    = domestic_cols,  # Columns (or paste0("shock_", 1:N_dom))
    horizon  = NULL
  )
)

#----------------------------------------------------------------------------#
# Shocks 3
mp_shock    <- policy_idx  # or "policy_rate"
macro_shock <- gdp_idx

# Constrain restrictions for horizons 1 through 3 (Quarters 0, 1, 2)
for (h in 1:3) {
  # 1. Monetary Policy Shock (Rate UP, GDP DOWN, CPI DOWN, Unemployment UP)
  sign_irf[policy_idx, mp_shock, h] <-  1
  sign_irf[gdp_idx,    mp_shock, h] <- -1
  sign_irf[cpi_idx,    mp_shock, h] <- -1
  sign_irf[unemp_idx,  mp_shock, h] <-  1

  # 2. Adverse Macro / Demand Shock (GDP DOWN, Unemployment UP, Policy Rate DOWN)
  sign_irf[gdp_idx,    macro_shock, h] <- -1
  sign_irf[unemp_idx,  macro_shock, h] <-  1
  sign_irf[policy_idx, macro_shock, h] <- -1  # Endogenous rate cut response
}

# Quoted directly from package
#"Stationary an N logical vector - its element set to FALSE sets the prior mean for the autoregressive
#parameters of the Nth equation to the white noise process, otherwise to random walk."
is_stationary <- c(  
  # Domestic Variables (9)
  FALSE,  # gdp_se_log (Non-Stationary I(1))
  FALSE, # unemployment_rate (Non-Stationary I(1)
  FALSE,  # cpi_log (Non-Stationary I(1))
  FALSE,  # policy_rate (Non-Stationary I(1))
  FALSE,  # target_cons [cons_habitual_log / cons_durable_log] (Non-Stationary I(1))
  FALSE,  # debt_to_asset_ratio (Non-Stationary I(1))
  FALSE,  # dsr_value (Non-Stationary / Conflicting I(1))
  FALSE,  # liquid_asset_to_income_ratio (Non-Stationary I(1))
  FALSE,  # saving_rate (Non-Stationary I(1))
  FALSE
)

#=============================================================================#
#                   [3] MODEL SPECIFICATION AT DIFFERENT LAGS

raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
n_cores <- max(1, parallel::detectCores() - 2)
set.seed(12345)
#------------------------------------------------------------------------------#
#                                 Model 1 (p = 1)
#------------------------------------------------------------------------------#

spec_baseline_one_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 1,                     
  sign_irf     = sign_irf,               
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx -1,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                              Model 2 (p = 2)
#------------------------------------------------------------------------------#

spec_baseline_two_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,         
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
#                              Model 3 (p = 3)
#------------------------------------------------------------------------------#

spec_baseline_three_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,          # 9 domestic variables
  p            = 3,                      # p = 2 lags
  sign_irf     = sign_irf,               # 13 x 13 x 20 array
  stationary   = is_stationary,          # Length 13 vector
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  # Kept FALSE for numeric stability
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 3,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)


#------------------------------------------------------------------------------#
#                              Model 4 (p = 4) 
#------------------------------------------------------------------------------#

spec_baseline_four_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,         
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
#                              Model 5 (p = 5) 
#------------------------------------------------------------------------------#

spec_baseline_five_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,         
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
#                          [4]  ESTIMATE HYPER-PARAMETER

spec_baseline_one_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_baseline_two_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_baseline_three_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_baseline_four_habitual$estimate_hyper(S = 5000, burn_in = 1000)
spec_baseline_five_habitual$estimate_hyper(S = 5000, burn_in = 1000)


#=============================================================================#
#                          [5]  RUN MODEL

estimate_baseline_habitual_1 <- estimate(spec_baseline_one_habitual, S = 4000, thin = 1)
estimate_baseline_habitual_2 <- estimate(spec_baseline_two_habitual, S = 4000, thin = 1)
estimate_baseline_habitual_3 <- estimate(spec_baseline_three_habitual, S = 4000, thin = 1)
estimate_baseline_habitual_4 <- estimate(spec_baseline_four_habitual, S = 4000, thin = 1)
estimate_baseline_habitual_5 <- estimate(spec_baseline_five_habitual, S = 4000, thin = 1)


#=============================================================================#
#                          [6]  SAVE ESTIMATED MODELS
#=============================================================================#

dir.create("3_Model_Output/Model_Baseline/Habitual", recursive = TRUE, showWarnings = FALSE)


saveRDS(estimate_baseline_habitual_1, file = "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p1.rds")
saveRDS(estimate_baseline_habitual_2, file = "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p2.rds")
saveRDS(estimate_baseline_habitual_3, file = "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p3.rds")
saveRDS(estimate_baseline_habitual_4, file = "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p4.rds")
saveRDS(estimate_baseline_habitual_5, file = "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p5.rds")


#=============================================================================#
#                          [7]   STABILITY DIAGNOSTIC
#=============================================================================#

ev_one    <- check_posterior_stability(estimate_baseline_habitual_1, p = 1)
ev_two <- check_posterior_stability(estimate_baseline_habitual_2, p = 2)
ev_three   <- check_posterior_stability(estimate_baseline_habitual_3, p = 3)
ev_four <- check_posterior_stability(estimate_baseline_habitual_4, p = 4)
ev_five  <- check_posterior_stability(estimate_baseline_habitual_5, p = 5)


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

# Export to CSV (row.names = FALSE removes the 1,2,3... index column)
write.csv(stability_diagnostics, file = "3_Model_Output/Model_Baseline/Habitual/posterior_stability_summary.csv", row.names = FALSE)
