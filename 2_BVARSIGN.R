# ==========================================================================================#
#                             2.BAYESIAN ESTIMATION WITH SIGN RESTRICTION
# ==========================================================================================#

# 1. DATA
bvar_data <- read.csv(
  "Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)

target_cols <- c(
  "gdp_growth",
  "consumption_growth",
  "cpi_growth",
  "saving_rate",
  "policy_rate",
  "interest_burden",
  "debt_growth",
  "real_house_price_growth",
  "asset_liability_ratio"
)

bvar_matrix <- as.matrix(
  bvar_data[, target_cols]
)



# ============================================================
# 2. VARIABLE INDICES
# ============================================================

gdp_idx <- which(target_cols == "gdp_growth")
consumption_idx <- which(target_cols == "consumption_growth")
cpi_idx <- which(target_cols == "cpi_growth")
saving_idx <- which(target_cols == "saving_rate")
policy_idx <- which(target_cols == "policy_rate")
interest_burden_idx <- which(target_cols == "interest_burden")
debt_idx <- which(target_cols == "debt_growth")
house_idx <- which(target_cols == "real_house_price_growth")
asset_liability_idx <- which(target_cols == "asset_liability_ratio")


# ============================================================
# 3. PRIOR / INTEGRATION ORDER
# ============================================================

is_random_walk <- c(
  FALSE,  # gdp_growth
  FALSE,  # consumption_growth
  FALSE,  # cpi_growth
  TRUE,   # saving_rate
  TRUE,   # policy_rate
  TRUE,   # interest_burden
  FALSE,  # debt_growth
  FALSE,  # real_house_price_growth
  TRUE    # asset_liability_ratio
)

# ============================================================
# 4. SIGN RESTRICTIONS
# ============================================================

N <- ncol(bvar_matrix) # Number of Rows and Columns (Variable X Shock)
H_total <- 20 # Time Periods (Roughly 5 years)

sign_irf <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

H_restrict <- 6 # How long the restriction will apply (1 and a half years)


# Monetary policy shock
for (h in 1:H_restrict) {  # For all the entries in the matrices when restriction
                           # applies, apply the following sign restriction.

  sign_irf[policy_idx, policy_idx, h] <-  1  # A positive shock in the policy col
  sign_irf[cpi_idx, policy_idx, h] <-     -1
  sign_irf[interest_burden_idx, policy_idx, h] <-  1
  sign_irf[debt_idx, policy_idx, h] <-    -1
}


# ============================================================
# 5. MODEL
# ============================================================

spec <- specify_bsvarSIGN$new(
  data = bvar_matrix,
  p = 4,
  sign_irf = sign_irf,
  stationary = is_random_walk
)


# ============================================================
# ESTIMATION
# ============================================================

run_bsvar_sign <- estimate(
  spec,
  S = 5000
)

# ============================================================
# IRFs
# ============================================================

irf_updated <- compute_impulse_responses(
  run_bsvar_sign,
  horizon = H_total
)

plot(irf_updated)








# ============================================================
# EXECUTION
# ============================================================

# Simply pass 'irf_updated' directly into the function
fevd_updated <- compute_fevd_from_irf(irf_updated)



# ============================================================
# USAGE IN YOUR SCRIPT
# ============================================================

# Export IRF Results to CSV
irf_df <- export_bvar_results_to_csv(
  bvar_array = irf_updated,
  var_names  = target_cols,
  file_name  = "IRF_Summary_Results3.csv"
)

# Export FEVD Results to CSV
fevd_df <- export_bvar_results_to_csv(
  bvar_array = fevd_updated,
  var_names  = target_cols,
  file_name  = "FEVD_Summary_Results3.csv"
)










library(coda)

# Extract MCMC chain for structural parameters (e.g., autoregressive matrix B)
# run_bsvar_sign$posterior$B has dimensions [N, N*p + 1, Draws]
b_draws <- run_bsvar_sign$posterior$B

# Pick key parameters to inspect (e.g., self-lag of policy_rate)
policy_lag1_chain <- b_draws[policy_idx, policy_idx, ]

# 1. Plot Trace Plot & Autocorrelation Function (ACF)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(policy_lag1_chain, type = "l", col = "navy", 
     main = "Trace Plot: Policy Rate Own-Lag Coefficient", 
     xlab = "MCMC Iteration", ylab = "Value")
acf(policy_lag1_chain, main = "Autocorrelation Function (ACF)")
par(mfrow = c(1, 1))

# 2. Convert to coda mcmc object for Effective Sample Size (ESS) and Geweke diagnostic
mcmc_chain <- coda::mcmc(policy_lag1_chain)

# Effective Sample Size (Higher is better, >1,000 is ideal)
ess_val <- coda::effectiveSize(mcmc_chain)
print(paste("Effective Sample Size (ESS):", round(ess_val, 2)))

# Geweke Diagnostic (Z-score between -1.96 and +1.96 indicates convergence)
geweke_val <- coda::geweke.diag(mcmc_chain)
print(geweke_val)





library(coda)

# Extract MCMC chain for structural parameters (e.g., autoregressive matrix B)
# run_bsvar_sign$posterior$B has dimensions [N, N*p + 1, Draws]
b_draws <- run_bsvar_sign$posterior$B

# Pick key parameters to inspect (e.g., self-lag of policy_rate)
policy_lag1_chain <- b_draws[policy_idx, policy_idx, ]

png(
  "Test.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

# 1. Plot Trace Plot & Autocorrelation Function (ACF)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(policy_lag1_chain, type = "l", col = "navy", 
     main = "Trace Plot: Policy Rate Own-Lag Coefficient", 
     xlab = "MCMC Iteration", ylab = "Value")
acf(policy_lag1_chain, main = "Autocorrelation Function (ACF)")
par(mfrow = c(1, 1))

dev.off()

# 2. Convert to coda mcmc object for Effective Sample Size (ESS) and Geweke diagnostic
mcmc_chain <- coda::mcmc(policy_lag1_chain)


# Effective Sample Size (Higher is better, >1,000 is ideal)
ess_val <- coda::effectiveSize(mcmc_chain)
print(paste("Effective Sample Size (ESS):", round(ess_val, 2)))

# Geweke Diagnostic (Z-score between -1.96 and +1.96 indicates convergence)
geweke_val <- coda::geweke.diag(mcmc_chain)
print(geweke_val)



# Check acceptance diagnostics if available in the model object
if (!is.null(run_bsvar_sign$posterior$acceptance_rate)) {
  message(paste("Sign Restriction Acceptance Rate:", 
                round(run_bsvar_sign$posterior$acceptance_rate * 100, 2), "%"))
} else {
  message("Sign restrictions accepted across posterior draws.")
}


# ------------------------------------------------------------
# Robustness Check A: Alternative Lag Order (p = 2 or p = 6)
# ------------------------------------------------------------
spec_p2 <- specify_bsvarSIGN$new(
  data = bvar_matrix,
  p = 2,                             # Reduced lag length
  sign_irf = sign_irf,
  stationary = is_random_walk
)
run_p2 <- estimate(spec_p2, S = 10000, thin = 5)
irf_p2 <- compute_impulse_responses(run_p2, horizon = H_total)

# ------------------------------------------------------------
# Robustness Check B: Extended Restriction Horizon (H = 2 vs H = 1)
# ------------------------------------------------------------
sign_irf_h2 <- array(NA, dim = c(N, N, H_total))
for (h in 1:2) {                    # Restrict impact AND period 2
  sign_irf_h2[policy_idx, policy_idx, h]          <-  1
  sign_irf_h2[cpi_idx, policy_idx, h]             <- -1
  sign_irf_h2[interest_burden_idx, policy_idx, h] <-  1
  sign_irf_h2[debt_idx, policy_idx, h]            <- -1
}

spec_h2 <- specify_bsvarSIGN$new(
  data = bvar_matrix,
  p = 4,
  sign_irf = sign_irf_h2,
  stationary = is_random_walk
)
run_h2 <- estimate(spec_h2, S = 10000, thin = 5)
irf_h2 <- compute_impulse_responses(run_h2, horizon = H_total)




# Combine baseline, p=2, and H=2 specifications
df_base <- get_irf_median_df(irf_updated, "gdp_growth", "policy_rate", "Baseline (p=4, H=1)")
df_p2   <- get_irf_median_df(irf_p2,      "gdp_growth", "policy_rate", "Robustness (p=2)")
df_h2   <- get_irf_median_df(irf_h2,      "gdp_growth", "policy_rate", "Robustness (H=2)")

df_comp <- rbind(df_base, df_p2, df_h2)


png(
  "Test2.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

# Plot overlaid comparison
ggplot(df_comp, aes(x = Horizon, y = Median, color = Specification, linetype = Specification)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Robustness Check: GDP Growth Response to Monetary Policy Shock",
    subtitle = "Comparing Baseline vs. Alternative Model Specifications",
    x = "Horizon",
    y = "Posterior Median Response"
  ) +
  theme_minimal()

dev.off()