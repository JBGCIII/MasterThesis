
# ==========================================================================================#
#                             2.BAYESIAN ESTIMATION WITH SIGN RESTRICTION
# ==========================================================================================#

# ==========================================================================================#
#                                     [1] Data Load
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


# ==========================================================================================#
#                                     [2] Creating Matrices

# [Model 1]: 
bvar_matrix_policy_shock <- as.matrix(
  bvar_data[, target_cols]
)

# [Model 2]: How does the Swedish Economy react to prolonged borrowing  restriction?
bvar_matrix_credit_supply_shock <- as.matrix(
  bvar_data[, target_cols]
)

# [Model 3]: How does the Swedish Economy react to prolonged housing price shock?
bvar_matrix_demand_shock <- as.matrix(
  bvar_data[, target_cols]
)

# [Model 4]: How does the Swedish Economy react to prolonged housing wealth contraction?
bvar_matrix_wealth_shock <- as.matrix(
  bvar_data[, target_cols]
)


# ==========================================================================================#
#                                     [2] Variables Indices

gdp_idx <- which(target_cols == "gdp_growth")
consumption_idx <- which(target_cols == "consumption_growth")
cpi_idx <- which(target_cols == "cpi_growth")
saving_idx <- which(target_cols == "saving_rate")
policy_idx <- which(target_cols == "policy_rate")
interest_burden_idx <- which(target_cols == "interest_burden")
debt_idx <- which(target_cols == "debt_growth")
house_idx <- which(target_cols == "real_house_price_growth")
asset_liability_idx <- which(target_cols == "asset_liability_ratio")


# ==========================================================================================#
#                                     [3] Prior and Integrations

# bsvarSIGNs convention: TRUE = Random Walk (Mean = 1), FALSE = White Noise (Mean = 0)
is_random_walk <- c(
  FALSE,  # gdp_growth 
  FALSE,  # consumption_growth
  FALSE,  # cpi_growth
  TRUE,   # saving_rate
  TRUE,   # policy_rate
  TRUE,   # interest_burden
  TRUE,  # debt_growth
  FALSE,  # real_house_price_growth
  TRUE    # asset_liability_ratio
)

# ==========================================================================================#
#                                     [4] Sign Restrictions.

N <- ncol(bvar_matrix) # Number of Rows and Columns (Variable X Shock)
H_total <- 20 # Time Periods (5 years)


##[Model 1]
sign_irf_policy <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

##[Model 2]
sign_irf_credit_supply <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

##[Model 3]
sign_irf_demand_shock <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

##[Model 4]
sign_irf_wealth_shock <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)


# ==========================================================================================#
#                                     [4] Sign Restrictions.


## [Model 1: Policy Rate Shock]
#How does the Swedish Economy react to a prolonged monetary policy contraciton?
H_restrict_policy <- 6 # How long the restriction will apply (1 and a half years)
# Why not 8? Mostly due to the lenght it takes to measures the model.

# Monetary policy shock
for (h in 1:H_restrict_policy) {  # For all the entries in the matrices when restriction
                           # applies, apply the following sign restriction.

  sign_irf[policy_idx, policy_idx, h] <-  1  # Positive shock in policy
  sign_irf[cpi_idx, policy_idx, h] <-     -1 # Negative shock in CPI
  sign_irf[interest_burden_idx, policy_idx, h] <-  1 # Positive shock in Interest burden
  sign_irf[debt_idx, policy_idx, h] <-    -1 # Negative shock in debt
}

## [Model 1: Policy Rate Shock]









# ==========================================================================================#
#                                     [5] Model

# Use multi-core execution (detect available cores)
n_cores <- max(1, parallel::detectCores() - 1)


spec <- specify_bsvarSIGN$new(
  data         = bvar_matrix,
  p            = 4,
  sign_irf     = sign_irf,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

# Optional: Find optimal hyperparameter starting points using Adaptive Metropolis
set.seed(321)
spec$estimate_hyper(S = 20000, burn_in = 10000)

# ==========================================================================================#
#                                     [6] Estimation & Analysis
# ==========================================================================================#


run_bsvar_sign <- estimate(
  spec,
  S = 10000)


export_hyperparameters(
  estimation_obj = run_bsvar_sign,
  target_cols    = target_cols,
  file_path      = "Output/hyperparameter_summary2.csv"
)



irf_updated <- compute_impulse_responses(
  run_bsvar_sign,
  horizon = H_total
)



dimnames(irf_updated)

# Export IRF Results to CSV
irf_df <- export_bvar_results_to_csv(
  bvar_array = irf_updated,
  var_names  = target_cols,
  file_name  = "IRF_Summary_Results6.csv"
)



df_base <- get_irf_median_df(irf_updated, "gdp_growth", "policy_rate", "Baseline (p=4, H=1)")


plot(irf_updated)

# ==========================================================================================#
#                                     [8] FEVD

# Simply pass 'irf_updated' directly into the function
fevd_updated <- compute_fevd_from_irf(irf_updated)



# Export FEVD Results to CSV
fevd_df <- export_bvar_results_to_csv(
  bvar_array = fevd_updated,
  var_names  = target_cols,
  file_name  = "FEVD_Summary_Results6.csv"
)





# Works in script 1 or 2 before target_cols is ever declared
df_base <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = 1,              # 1st variable (e.g. gdp_growth)
  shock_var    = 5,              # 5th variable (e.g. policy_rate)
  label_name   = "Baseline (p=4, H=1)"
)




png(
  "Test3.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

# Plot overlaid comparison
ggplot(df_base, aes(x = Horizon, y = Median, color = Specification, linetype = Specification)) +
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








df_gdp_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "gdp_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Monetary Policy Shock",
  target_cols  = target_cols
)





# 1. Extract IRF data frame
df_gdp_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "gdp_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_gdp_policy,
  response_name = "GDP Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Output/IRF_GDP_PolicyRate.png"
)



# 1. Extract IRF data frame
df_gdp_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "saving_rate", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_gdp_policy,
  response_name = "Savings Rate",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Output/IRF_GDP_Savings_Rate.png"
)






# 1. Extract IRF data frame
df_gdp_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "asset_liability_ratio", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_gdp_policy,
  response_name = "Asset to Liability Ratio",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Output/IRF_Asset_to_liability.png"
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






# Row 10 is lambda
plot(run_bsvar_sign$posterior$hyper[10, ], type = "l", ylab = "lambda", col = "navy")

plot(run_bsvar_sign$posterior$hyper[11, ], type = "l", ylab = "alpha", col = "darkgreen")

