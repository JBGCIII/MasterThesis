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
# 2. Model with domestic variables only
domestic_cols <- c(
  "gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log",
  "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
  "saving_rate",  "kix_real_log"
)
domestic_data <- as.matrix(model_ts[, domestic_cols])

N_dom <- ncol(domestic_data)

#----------------------------------------------------------------------------#
# 3. Create an index, otherwise Bsvarsign will not recognize it
gdp_idx <- "gdp_se_log"
unemp_idx <- "unemployment_rate"
cpi_idx <- "cpi_log"
policy_idx <- "policy_rate"
cons_idx <- "cons_durable_log"
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
  FALSE,  # gdp_se_log
  FALSE,  # unemployment_rate
  FALSE,  # cpi_log
  FALSE,  # policy_rate
  FALSE,  # cons_durable_log
  FALSE,  # debt_to_asset_ratio
  FALSE,  # dsr_value
  FALSE,  # liquid_asset_to_income_ratio
  FALSE,  # saving_rate
  FALSE   # kix_real_log
)

#=============================================================================#
#                   [3] NARRATIVE RESTRICTION

# 1. Adverse Macro Shock (Shock 2) in 2008Q4 is strictly negative
narrative_gfc <- specify_narrative(
  start   = which(bvar_data$quarter == "2008Q4"),
  periods = 1,
  type    = "S",
  sign    = -1,
  shock   = 2,     # Shock 2: Adverse Macro Shock
  var     = NA     # NA because type = "S" (applies to structural shock directly)
)

# 2. Monetary Policy Shock (Shock 1) in 2022Q2 is strictly positive
narrative_mp <- specify_narrative(
  start   = which(bvar_data$quarter == "2022Q2"),
  periods = 1,
  type    = "S",
  sign    = 1,
  shock   = 1,     # Shock 1: Monetary Policy Shock
  var     = NA     # NA for type = "S"
)

# Combine narratives into a list
narrative_list <- list(narrative_gfc, narrative_mp)


#=============================================================================#
#                         [4] MODEL SPECIFICATION
raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
n_cores <- max(1, parallel::detectCores() - 2)
set.seed(12345)


#------------------------------------------------------------------------------#
#                                 Model 1 (p = 1)
#------------------------------------------------------------------------------#
spec_narrative_one_durable <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 1,                     
  sign_irf     = sign_irf,               
  stationary   = is_stationary, 
  sign_narrative = narrative_list,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx -1,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
#                                 Model 2 (p = 2)
#------------------------------------------------------------------------------#
spec_narrative_two_durable <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 2,                     
  sign_irf     = sign_irf,               
  stationary   = is_stationary, 
  sign_narrative = narrative_list,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx -2,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

#=============================================================================#
#                          [4]  ESTIMATE HYPER-PARAMETER

spec_narrative_one_durable$estimate_hyper(S = 5000, burn_in = 1000)
spec_narrative_two_durable$estimate_hyper(S = 5000, burn_in = 1000)

#=============================================================================#
#                          [5]  RUN MODEL

estimate_narrative_durable_1 <- estimate(spec_narrative_one_durable, S = 4000, thin = 1)

# Model Run Start 21:49. End 02:04
estimate_narrative_durable_2 <- estimate(spec_narrative_two_durable, S = 4000, thin = 1)

#=============================================================================#
#                          [6]  SAVE ESTIMATED MODELS
#=============================================================================#

dir.create("3_Model_Output/Model_Narrative/Durable", recursive = TRUE, showWarnings = FALSE)


saveRDS(estimate_narrative_durable_1, file = "3_Model_Output/Model_Narrative/Durable/narrative_durable_p1.rds")
saveRDS(estimate_narrative_durable_2, file = "3_Model_Output/Model_Narrative/Durable/narrative_durable_p2.rds")

#=============================================================================#
#                          [6]  SAVE ESTIMATED MODELS
#=============================================================================#


estimate_narrative_durable_1 <- readRDS("3_Model_Output/Model_Narrative/Durable/narrative_durable_p1.rds")
estimate_narrative_durable_2 <- readRDS("3_Model_Output/Model_Narrative/Durable/narrative_durable_p2.rds")



ev_one    <- check_posterior_stability(estimate_narrative_durable_1, p = 1)
ev_two <- check_posterior_stability(estimate_narrative_durable_2, p = 2)


stability_diagnostics <- data.frame(
  Model = paste0("Model ", 1:2),
  Pct_Stable = c(
    mean(ev_one < 1) * 100,
    mean(ev_two < 1) * 100
  ),
  Median_Rho = c(
    median(ev_one),
    median(ev_two)
  ),
  P95_Rho = c(
    quantile(ev_one, .95),
    quantile(ev_two, .95)
  ),
  Max_Rho = c(
    max(ev_one),
    max(ev_two)
  )
)



# Export to CSV (row.names = FALSE removes the 1,2,3... index column)
write.csv(stability_diagnostics, file = "3_Model_Output/Model_narrative/Durable/posterior_stability_summary.csv", row.names = FALSE)








