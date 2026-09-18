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
  "cons_habitual_log",
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
  FALSE, 

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
cons_idx <- N_for + which(domestic_cols == "cons_habitual_log")
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

