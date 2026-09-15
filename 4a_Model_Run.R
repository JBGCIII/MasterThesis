###############################################################################
############################# CONSUMPTION HABITUAL ############################
###############################################################################


bvar_data <- read.csv("2_Seasonally_adjusted_data.csv")

# ----------------------------------------------------------------------------
# 1. DEFINE VARIABLES & SEPARATE CONSUMPTION
foreign_cols <- c(
  "fed_funds_rate",
  "kix_gdp_log",
  "ea_hicp_log",
  "kix_real_log"
)

# Select WHICH consumption metric to estimate in this run:
# Choice: "cons_habitual_log" OR "cons_durable_log"
target_cons <- "cons_habitual_log" 

domestic_cols <- c(
  "gdp_se_log",
  "unemployment_rate",
  "cpi_log",
  "policy_rate",
  target_cons,
  "debt_to_asset_ratio",
  "dsr_value",
  "liquid_asset_to_income_ratio",
  "saving_rate"
)

foreign_data  <- as.matrix(bvar_data[, foreign_cols])   # 4 foreign
domestic_data <- as.matrix(bvar_data[, domestic_cols])  # 9 domestic

N_for <- ncol(foreign_data)   # 4
N_dom <- ncol(domestic_data)  # 9
N_sys <- N_for + N_dom        # 13 total variables

# Stationary vector matching system dimension
is_stationary <- rep(TRUE, N_sys) # TRUE = Random Walk prior mean 1

# ============================================================
# 2. UPDATED INDEX MAPPING (13-Variable System)
# ============================================================
gdp_idx     <- N_for + which(domestic_cols == "gdp_se_log")          # 5
unemp_idx   <- N_for + which(domestic_cols == "unemployment_rate")     # 6
cpi_idx     <- N_for + which(domestic_cols == "cpi_log")                # 7
policy_idx  <- N_for + which(domestic_cols == "policy_rate")           # 8
cons_idx    <- N_for + which(domestic_cols == target_cons)             # 9
dta_idx     <- N_for + which(domestic_cols == "debt_to_asset_ratio")   # 10
dsr_idx     <- N_for + which(domestic_cols == "dsr_value")             # 11
liquid_idx  <- N_for + which(domestic_cols == "liquid_asset_to_income_ratio") # 12
saving_idx  <- N_for + which(domestic_cols == "saving_rate")           # 13

mp_shock    <- policy_idx  # 8
macro_shock <- gdp_idx     # 5

# ============================================================
# 3. SIGN RESTRICTION ARRAY (13 x 13 x 20)
# ============================================================
H_total  <- 20
sign_irf <- array(NA_real_, dim = c(N_sys, N_sys, H_total))

# Monetary Policy Shock (mp_shock) - Impact restriction (h = 1)
sign_irf[policy_idx, mp_shock, 1] <-  1
sign_irf[gdp_idx,    mp_shock, 1] <- -1
sign_irf[cpi_idx,    mp_shock, 1] <- -1
sign_irf[unemp_idx,  mp_shock, 1] <-  1

# Adverse Macro Shock (macro_shock) - Impact restriction (h = 1)
sign_irf[gdp_idx,   macro_shock, 1] <- -1
sign_irf[unemp_idx, macro_shock, 1] <-  1

# ============================================================
# 4. SPECIFY AND ESTIMATE MODEL (p = 2)
# ============================================================
p <- 2
raw_covid_idx       <- which(bvar_data$quarter == "2020Q2")
effective_covid_idx <- raw_covid_idx - p

specification_habitual <- specify_bsvarSIGN$new(
  data         = domestic_data,          # 9 domestic variables
  p            = p,                      # p = 2 lags
  sign_irf     = sign_irf,               # 13 x 13 x 20 array
  foreign      = foreign_data,           # 4 foreign variables
  stationary   = is_stationary,          # Length 13 vector
  hyper_lambda = TRUE,
  hyper_mu     = FALSE,                  # Kept FALSE for numeric stability
  hyper_delta  = TRUE,
  hyper_psi    = TRUE,
  hyper_covid  = effective_covid_idx,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)

set.seed(12345)

cat("Estimating hyperparameters for Durable Consumption Model...\n")
specification_habitual$estimate_hyper(S = 3000, burn_in = 1000)

cat(r"(
Things will take a while from now. Go, grab coffee or something.
The model should be saved and run by the time you come back.

      .-~~-.
    ,|`-__-'|
    ||      |     COFFEE OR TEA
    `|      |
      `-__-'

Saving model I found was a good way to prevent me from rerunning things.
It also allowed to move models to other script, which is useful for diagnostics
when comparing multiple models.
)")
#============================================================================#

fit_consumption_model <- estimate(
  specification_habitual, 
  S    = 2000, 
  thin = 5
)

coda::effectiveSize(coda::as.mcmc(fit_consumption_model$posterior$A))

roots <- eigen(companion)$values



#> ls(fit_consumption_model$posterior)
#[1] "A"      "B"      "ess"    "hyper"  "Q"      "shocks" "Sigma"  "Theta0"

summary(fit_consumption_model$posterior$ess)

summary(fit_consumption_model$posterior$Q)

summary(fit_consumption_model$posterior$shocks)






# Extract posterior draws of A
draws_A <- fit_consumption_model$posterior$A

# Reshape from a multi-way array to a 2D matrix (Iterations x Parameters)
# Assuming dimensions are [equations, coefficients, iterations]
dim_A   <- dim(draws_A)
mat_A   <- matrix(draws_A, nrow = dim_A[3], ncol = dim_A[1] * dim_A[2])

# Now coda can compute the ESS for every coefficient
ess_A   <- coda::effectiveSize(coda::as.mcmc(mat_A))
summary(ess_A) # Inspect min, mean, and max ESS







# Check stability across all MCMC draws
draws_A <- fit_consumption_model$posterior$A
n_draws <- dim(draws_A)[3]

# Quick loop or apply to check if max eigenvalue < 1 for every single draw
stable_draws <- sapply(1:n_draws, function(i) {
  A_i <- draws_A[,,i]
  # Assuming 1 lag for simplicity; build companion block here if multi-lag
  roots <- eigen(A_i)$values
  all(Mod(roots) < 1)
})

# What percentage of your posterior draws are stable?
mean(stable_draws) * 100












saveRDS(fit_consumption_model, "fit_cons_durable_log.rds")


coda::effectiveSize(coda::as.mcmc(fit_consumption_model$posterior$A))

roots <- eigen(companion)$values







saveRDS(fit_consumption_model, file = file.path(out_dir, "fit_cons_durable_log.rds"), compress = "xz")
cat("Durable consumption model finished and saved cleanly!\n")










# 3. Residual Variance Check (AR(1) Residual Variances for Minnesota Prior Scales)
cat("\n=================== AR(1) RESIDUAL VARIANCES ===================\n")
ar_vars <- apply(full_data, 2, function(x) {
  fit <- ar.ols(x, aic = FALSE, order.max = 1)
  var(fit$resid, na.rm = TRUE)
})
print(round(ar_vars, 6))

# ==============================================================================
# MODEL B: DURABLE CONSUMPTION SPECIFICATION
# ==============================================================================

target_cons <- "cons_durable_log"

domestic_cols_durable <- c(
  "gdp_se_log",
  "unemployment_rate",
  "cpi_log",
  "policy_rate",
  target_cons,
  "debt_to_asset_ratio",
  "dsr_value",
  "saving_rate"
)

domestic_data_dur <- as.matrix(bvar_data[, domestic_cols_durable])

specification_dur <- specify_bsvarSIGN$new(
  data         = domestic_data_dur,      # 8 domestic variables
  p            = 2,                      # p = 2 lags
  sign_irf     = sign_irf,               # 11 x 11 x 20 array
  foreign      = foreign_data,           # 3 foreign variables
  stationary   = is_stationary,          # Length 11 vector (TRUE)
  hyper_lambda = TRUE,
  hyper_mu     = FALSE,
  hyper_delta  = TRUE,
  hyper_psi    = TRUE,
  hyper_covid  = effective_covid_idx,
  mc.cores     = n_cores
)


A_draw <- fit_consumption_model$posterior$A[, , 1]

A1 <- A_draw[, 1:13]
A2 <- A_draw[, 14:26]

companion <- rbind(
  cbind(A1, A2),
  cbind(diag(13), matrix(0, 13, 13))
)

roots <- eigen(companion)$values
Mod(roots)





