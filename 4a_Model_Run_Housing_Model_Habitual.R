###############################################################################
###################### HOUSING TRANSMISSION MODEL - HABITUAL ##################
###############################################################################

# ==============================================================================
# [1] Model Environment & Variable Setup
# ==============================================================================

# 1.1 Load seasonally adjusted macroeconomic and financial dataset
bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")

# 1.2 Convert data frame into quarterly time series object (1996Q1 start)
model_ts <- ts(
  bvar_data,
  start = c(1996, 1),
  frequency = 4
)

# 1.3 Select endogenous variables for Housing Transmission Model with Habitual Consumption
# Note: CPI omitted; Household Savings Rate and Debt Service Ratio (DSR) added
housing_cols <- c(
  "unemployment_rate", "policy_rate", "house_price_real_log",
  "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
  "saving_rate", "cons_habitual_log"
)

# 1.4 Cast selected time series to matrix representation for bsvarSIGN
housing_data <- as.matrix(model_ts[, housing_cols])

# 1.5 Store total number of endogenous variables in the system
N_housing <- ncol(housing_data)

# 1.6 Define individual variable string identifiers for sign restriction indexing
unemp_idx   <- "unemployment_rate"
policy_idx  <- "policy_rate"
house_idx   <- "house_price_real_log"
dta_idx     <- "debt_to_asset_ratio"
dsr_idx     <- "dsr_value"
liquid_idx  <- "liquid_asset_to_income_ratio"
saving_idx  <- "saving_rate"
cons_idx    <- "cons_habitual_log"

# ==============================================================================
# [2] Sign Restriction Array & Prior Setup
# ==============================================================================

# 2.1 Set maximum impulse response horizon length (30 Quarters)
H_total   <- 30
N_housing <- length(housing_cols)

# 2.2 Initialize 3D array for structural sign restrictions [Variables x Shocks x Horizon]
sign_irf <- array(
  NA_real_,
  dim = c(N_housing, N_housing, H_total),
  dimnames = list(
    variable = housing_cols,  # Rows
    shock    = housing_cols,  # Columns
    horizon  = NULL
  )
)

# 2.3 Map target structural shocks to corresponding variable indices
mp_shock      <- policy_idx  # Monetary Policy Shock
macro_shock   <- unemp_idx   # Adverse Macro / Aggregate Demand Shock
housing_shock <- house_idx   # Adverse Housing Collateral / Wealth Shock

# 2.4 Impose structural sign restrictions across horizons 1 to 3 (Quarters 0, 1, and 2):
for (h in 1:3) {
  # --- 1. Monetary Policy Shock ---
  # Policy Rate increases (+1) -> Labor market weakens / Unemployment increases (+1)
  sign_irf[policy_idx, mp_shock, h] <-  1
  sign_irf[unemp_idx,  mp_shock, h] <-  1
  # Note: Debt service ratio (dsr_idx) is left unrestricted

  # --- 2. Adverse Macro / Demand Shock ---
  # Unemployment increases (+1) -> Policy rate cuts in response (-1)
  sign_irf[unemp_idx,  macro_shock, h] <-  1
  sign_irf[policy_idx, macro_shock, h] <- -1

  # --- 3. Adverse Housing Market Shock ---
  # Real House Prices drop (-1) -> Debt-to-Asset ratio / Balance sheet leverage increases (+1)
  sign_irf[house_idx,  housing_shock, h] <- -1
  sign_irf[dta_idx,    housing_shock, h] <-  1
}
# Target consumption, liquidity (LAI), and precautionary savings are left UNRESTRICTED

# 2.5 Specify prior AR(1) means (FALSE = Random Walk prior for Non-Stationary I(1) levels)
is_stationary <- rep(FALSE, N_housing)

# ==============================================================================
# [3] Estimation Loop Across Lag Lengths (p = 1:5)
# ==============================================================================

# 3.1 Locate COVID-19 shock observation index (2020Q2) for dummy/scaling correction
raw_covid_idx <- which(bvar_data$quarter == "2020Q2")

# 3.2 Configure CPU multi-core parallel processing
n_cores       <- max(1, parallel::detectCores() - 2)

# 3.3 Set seed for MCMC sampling reproducibility
set.seed(12345)

# 3.4 Create output directory for estimated model files
output_dir <- "3_Model_Output/Model_Housing/Habitual"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 3.5 Vector of candidate lag orders
lags <- 1:5

# 3.6 Loop: Model Specification, Hyperparameter Estimation, MCMC Sampling & Model Disk Saving
for (p in lags) {
  # 3.6.1 Construct bsvarSIGN specification object 
  spec <- specify_bsvarSIGN$new(
    data         = housing_data,
    p            = p,
    sign_irf     = sign_irf,
    stationary   = is_stationary,
    hyper_lambda = TRUE,
    hyper_mu     = TRUE,
    hyper_delta  = TRUE,
    hyper_psi    = FALSE,
    hyper_covid  = raw_covid_idx - p, # Adjust COVID-19 shock index for lag length
    mc.cores     = n_cores
  )
  
  # 3.6.2 Estimate posterior hyperparameters via empirical Bayes / MCMC (5000 iterations)
  spec$estimate_hyper(S = 5000, burn_in = 1000)
  
  # 3.6.3 Estimate Bayesian VAR model parameters (4000 posterior draws)
  fit <- estimate(spec, S = 4000, thin = 1)
  
  # 3.6.4 Save fit object to RDS file
  saveRDS(fit, file.path(output_dir, sprintf("housing_habitual_p%d.rds", p)))
  
  # 3.6.5 Free allocated RAM memory after each model estimation
  rm(spec, fit)
  gc()
}

# ==============================================================================
# [4] Model Stability Diagnostics & Summary Export
# ==============================================================================

# 4.1 Reload saved model fit objects and compute eigenvalue posterior stability metrics
stability_list <- lapply(lags, function(p) {
  # 4.1.1 Read stored RDS model fit from disk
  model_file <- file.path(output_dir, sprintf("housing_habitual_p%d.rds", p))
  fit <- readRDS(model_file)
  
  # 4.1.2 Compute companion matrix maximum eigenvalues across posterior draws
  ev <- check_posterior_stability(fit, p = p)
  
  # 4.1.3 Clear model fit from memory
  rm(fit)
  gc()
  
  # 4.1.4 Return summary metrics dataframe for lag order p
  data.frame(
    Model      = paste0("Model ", p),
    Pct_Stable = mean(ev < 1) * 100,
    Median_Rho = median(ev),
    P95_Rho    = quantile(ev, 0.95),
    Max_Rho    = max(ev)
  )
})

# 4.2 Combine list of stability metrics into a unified summary data frame
stability_diagnostics <- do.call(rbind, stability_list)

# 4.3 Export stability diagnostic metrics to CSV
write.csv(
  stability_diagnostics,
  file = file.path(output_dir, "posterior_stability_summary.csv"),
  row.names = FALSE
)