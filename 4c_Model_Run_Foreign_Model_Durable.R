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
  "kix_real_log"
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
  "saving_rate"
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
liquid_idx <- N_for + which(
  domestic_cols == "liquid_asset_to_income_ratio"
)
saving_idx <- N_for + which(domestic_cols == "saving_rate")

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
  saving = saving_idx
)

#=============================================================================#
#                               [2] SIGN RESTRICTIONS

H_total <- 30

sign_irf <- array(
  NA_real_,
  dim = c(N_sys, N_sys, H_total)
)

#----------------------------------------------------------------------------#
mp_shock    <- 1  # or "policy_rate"
macro_shock <- 2

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
  FALSE   # saving_rate (Non-Stationary I(1))
)

# quoted directly from package
#"stationary an N logical vector - its element set to FALSE sets the prior mean for the autoregressive
#parameters of the Nth equation to the white noise process, otherwise to random walk."


#=============================================================================#
#                 [3] MODEL SPECIFICATION AT DIFFERENT LAGS

raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
n_cores <- max(1, parallel::detectCores() - 2)

set.seed(12345)

#------------------------------------------------------------------------------#
#                              Model 1 (p = 1)
#------------------------------------------------------------------------------#

spec_foreign_one <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 1,                      
  sign_irf     = sign_irf,               
  foreign      = foreign_data,           
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 1,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                                 Model 2 (p = 2)
#------------------------------------------------------------------------------#

spec_foreign_two <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 2,                      
  sign_irf     = sign_irf,               
  foreign      = foreign_data,           
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 2,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                                 Model 3 (p = 3)
#------------------------------------------------------------------------------#

spec_foreign_three <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 3,                      
  sign_irf     = sign_irf,               
  foreign      = foreign_data,           
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 3,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                                 Model 4 (p = 4)
#------------------------------------------------------------------------------#

spec_foreign_four <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 4,                      
  sign_irf     = sign_irf,               
  foreign      = foreign_data,           
  stationary   = is_stationary,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx - 4,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)
#------------------------------------------------------------------------------#
#                                 Model 5 (p = 5)
#------------------------------------------------------------------------------#

spec_foreign_five <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 5,                      
  sign_irf     = sign_irf,               
  foreign      = foreign_data,           
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

spec_foreign_one$estimate_hyper(S = 5000, burn_in = 1000)
spec_foreign_two$estimate_hyper(S = 5000, burn_in = 1000)
spec_foreign_three$estimate_hyper(S = 5000, burn_in = 1000)
spec_foreign_four$estimate_hyper(S = 5000, burn_in = 1000)
spec_foreign_five$estimate_hyper(S = 5000, burn_in = 1000)


#=============================================================================#
#                          [5]  RUN MODEL

estimate_foreign_durable_1 <- estimate(spec_foreign_one, S = 4000, thin = 1)
estimate_foreign_durable_2 <- estimate(spec_foreign_two, S = 4000, thin = 1)
estimate_foreign_durable_3 <- estimate(spec_foreign_three, S = 4000, thin = 1)
estimate_foreign_durable_4 <- estimate(spec_foreign_four, S = 4000, thin = 1)
estimate_foreign_durable_5 <- estimate(spec_foreign_five, S = 4000, thin = 1)


#=============================================================================#
#                          [6]  SAVE ESTIMATED MODELS

dir.create("3_Model_Output/Model_Foreign/Durable", recursive = TRUE, showWarnings = FALSE)


saveRDS(estimate_foreign_durable_1, file = "3_Model_Output/Model_Foreign/Durable/baseline_durable_p1.rds")
saveRDS(estimate_foreign_durable_2, file = "3_Model_Output/Model_Foreign/Durable/baseline_durable_p2.rds")
saveRDS(estimate_foreign_durable_3, file = "3_Model_Output/Model_Foreign/Durable/baseline_durable_p3.rds")
saveRDS(estimate_foreign_durable_4, file = "3_Model_Output/Model_Foreign/Durable/baseline_durable_p4.rds")
saveRDS(estimate_foreign_durable_5, file = "3_Model_Output/Model_Foreign/Durable/baseline_durable_p5.rds")


#=============================================================================#
#                          [6]   STABILITY DIAGNOSTIC

ev_one    <- check_posterior_stability(estimate_foreign_durable_1, p = 1)
ev_two <- check_posterior_stability(estimate_foreign_durable_2, p = 2)
ev_three   <- check_posterior_stability(estimate_foreign_durable_3, p = 3)
ev_four <- check_posterior_stability(estimate_foreign_durable_4, p = 4)
ev_five  <- check_posterior_stability(estimate_foreign_durable_5, p = 5)



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
write.csv(stability_diagnostics, file = "3_Model_Output/Model_Foreign/Durable/posterior_stability_summary.csv", row.names = FALSE)







