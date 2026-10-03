###############################################################################
############################# CONSUMPTION DURABLE ############################
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
# 2. Model with both foreign and domestic variables
foreign_cols <- c(
  "fed_funds_rate",
  "kix_gdp_log",
  "kix_cpi"
)

domestic_cols <- c(
  "gdp_se_log",
  "unemployment_rate",
  "cpi_log",
  "policy_rate",
  "cons_durable_log",
  "debt_to_asset_ratio",
  "dsr_value",
  "liquid_asset_to_income_ratio",
  "saving_rate",
  "kix_real_log"
)

#----------------------------------------------------------------------------#

foreign_data  <- as.matrix(model_ts[, foreign_cols])
domestic_data <- as.matrix(model_ts[, domestic_cols])

#----------------------------------------------------------------------------#
N_for <- ncol(foreign_data)
N_dom <- ncol(domestic_data)
N_sys <- N_for + N_dom

#----------------------------------------------------------------------------#

gdp_idx <- N_for + which(domestic_cols == "gdp_se_log")
unemp_idx <- N_for + which(domestic_cols == "unemployment_rate")
cpi_idx <- N_for + which(domestic_cols == "cpi_log")
policy_idx <- N_for + which(domestic_cols == "policy_rate")
cons_idx <- N_for + which(domestic_cols == "cons_durable_log")
dta_idx <- N_for + which(domestic_cols == "debt_to_asset_ratio")
dsr_idx <- N_for + which(domestic_cols == "dsr_value")
liquid_idx <- N_for + which(domestic_cols == "liquid_asset_to_income_ratio")
saving_idx <- N_for + which(domestic_cols == "saving_rate")
kix_idx <- N_for + which(domestic_cols == "kix_real_log")


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

H_total <- 30

sign_irf <- array(
  NA_real_,
  dim = c(N_sys, N_sys, H_total)
)

#----------------------------------------------------------------------------#
mp_shock <- policy_idx
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


#----------------------------------------------------------------------------#
is_stationary <- c(
  # Foreign Variables (4)
  FALSE,  # fed_funds_rate (Non-Stationary I(1))
  FALSE,  # kix_gdp_log (Non-Stationary I(1))
  FALSE, # kix_real_log (Stationary I(0))
  
  # Domestic Variables (9)
  FALSE,  # gdp_se_log (Non-Stationary I(1))
  FALSE, # unemployment_rate (Stationary I(0) based on ADF/KPSS drift spec)
  FALSE,  # cpi_log (Non-Stationary I(1))
  FALSE,  # policy_rate (Non-Stationary I(1))
  FALSE,  # target_cons [cons_habitual_log / cons_durable_log] (Non-Stationary I(1))
  FALSE,  # debt_to_asset_ratio (Non-Stationary I(1))
  FALSE,  # dsr_value (Non-Stationary / Conflicting I(1))
  FALSE,  # liquid_asset_to_income_ratio (Non-Stationary I(1))
  FALSE,   # saving_rate (Non-Stationary I(1))
  FALSE
)

#=============================================================================#
#              [3] MODEL SPECIFICATION AT DIFFERENT LAGS
#=============================================================================#

raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
n_cores       <- max(1, parallel::detectCores() - 2)
set.seed(12345)

output_dir <- "3_Model_Output/Model_Foreign/Durable"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

lags <- 1:5

#=============================================================================#
#              [4-6] SPECIFY, ESTIMATE, SAVE & CLEAR CACHE
#=============================================================================#

for (p in lags) {
  # 1. Specify model
  spec <- specify_bsvarSIGN$new(
    data         = domestic_data,
    p            = p,
    sign_irf     = sign_irf,
    foreign      = foreign_data,
    stationary   = is_stationary,
    hyper_lambda = TRUE,
    hyper_mu     = TRUE,
    hyper_delta  = TRUE,
    hyper_psi    = FALSE,
    hyper_covid  = raw_covid_idx - p, # Lenza & Primiceri scaling
    mc.cores     = n_cores
  )
  
  # 2. Estimate Hyper-parameters
  spec$estimate_hyper(S = 5000, burn_in = 1000)
  
  # 3. Estimate Model
  fit <- estimate(spec, S = 4000, thin = 1)
  
  # 4. Save Model
  saveRDS(fit, file = file.path(output_dir, sprintf("foreign_durable_p%d.rds", p)))
  
  # Clear memory for this iteration
  rm(spec, fit)
  gc()
}

# Clear workspace cache before stability analysis
gc()

#=============================================================================#
#              [7] STABILITY DIAGNOSTIC (RELOAD FROM DISK)
#=============================================================================#

stability_list <- lapply(lags, function(p) {
  # Reload model from disk
  model_file <- file.path(output_dir, sprintf("foreign_durable_p%d.rds", p))
  fit <- readRDS(model_file)
  
  # Compute stability metrics
  ev <- check_posterior_stability(fit, p = p)
  
  # Free RAM immediately after evaluation
  rm(fit)
  gc()
  
  # Build summary row
  data.frame(
    Model      = paste0("Model ", p),
    Pct_Stable = mean(ev < 1) * 100,
    Median_Rho = median(ev),
    P95_Rho    = quantile(ev, 0.95),
    Max_Rho    = max(ev)
  )
})

# Combine summary table
stability_diagnostics <- do.call(rbind, stability_list)

# Export to CSV
write.csv(
  stability_diagnostics,
  file = file.path(output_dir, "posterior_stability_summary.csv"),
  row.names = FALSE
)


