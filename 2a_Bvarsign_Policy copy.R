###############################################################################
################ 2.BAYESIAN ESTIMATION WITH SIGN RESTRICTION  #################
###############################################################################

# Directories
dir.create(
  "Analysis", 
  showWarnings = FALSE
)

dir.create(
  "Analysis/BVAR_Sign",
  showWarnings = FALSE
)

dir.create(
  "Analysis/BVAR_Sign/Model_01",
  showWarnings = FALSE
)


dir.create(
  "Analysis/BVAR_Sign/Model_01/IRF",
  showWarnings = FALSE
)



#============================================================================#
#                              [1] Matrix Set Up
#============================================================================#

# Load Seasonally and Outlier Adjusted Data.
bvar_data <- read.csv(
  "Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)

# Column of interest. Note how Debt to Income is left out.
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

# Create a Matrix.
bvar_matrix_policy_shock <- as.matrix(
  bvar_data[, target_cols]
)

# Variable Indices. 
gdp_idx <- which(target_cols == "gdp_growth")
consumption_idx <- which(target_cols == "consumption_growth")
cpi_idx <- which(target_cols == "cpi_growth")
saving_idx <- which(target_cols == "saving_rate")
policy_idx <- which(target_cols == "policy_rate")
interest_burden_idx <- which(target_cols == "interest_burden")
debt_idx <- which(target_cols == "debt_growth")
house_idx <- which(target_cols == "real_house_price_growth")
asset_liability_idx <- which(target_cols == "asset_liability_ratio")


# Number of Rows and Columns (Variable X Shock)
N <- ncol(bvar_matrix_policy_shock) # nolint
H_total <- 20 # Time Periods (5 years)


#============================================================================#
#                              [2] Sing Restriction Set UP
#============================================================================#

## [Model 1: Policy Rate Shock]
sign_irf_policy <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

#How does the Swedish Economy react to a prolonged monetary policy contraciton?
H_restrict_policy <- 2 # How long the restriction will apply

# [Model 1] Monetary policy shock
for (h in 1:H_restrict_policy) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.

  sign_irf_policy[policy_idx, policy_idx, h] <-  1  # Positive shock in policy
  sign_irf_policy[cpi_idx, policy_idx, h] <-     -1 # Negative shock in CPI
  sign_irf_policy[interest_burden_idx, policy_idx, h] <-  1 # Positive shock in Interest.
  sign_irf_policy[debt_idx, policy_idx, h] <-    -1 # Negative shock in debt
}



#============================================================================#
#                              [3] Model Specs
#============================================================================#

# bsvarSIGNs convention:
#TRUE = Random Walk (Mean = 1)
#FALSE = White Noise (Mean = 0)
# See full set up instruction at:
# https://bsvars.org/bsvarSIGNs/reference/specify_bsvarSIGN.html

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

# Use multi-core execution (detect available cores)
# This allows to run the model faster. You need to have one
# removed, otherwise you might not be able to run your machine
# Mine was seven and it took quite a while. I hope your is not
# Less than that.
n_cores <- max(1, parallel::detectCores() - 1)

spec_01 <- specify_bsvarSIGN$new(
  data         = bvar_matrix_policy_shock,
  p            = 4,
  sign_irf     = sign_irf_policy,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

Sys.setenv(OMP_NUM_THREADS = n_cores)


#============================================================================#
#                              [4] Model Run
#============================================================================#

# Optimal hyperparameter starting points using Adaptive Metropolis
set.seed(321)
spec_01$estimate_hyper(S = 30000, burn_in = 10000)

run_bsvar_sign_model_01 <- estimate(
  spec_01,
  S = 20000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

# Run 01: 00:52 to 01:26.



#============================================================================#
#                              [5] Model Diagnostics
#============================================================================#

# Trace Plot

# Let's find Alpha and Lamba
# str(spec$prior)
#Row Index     Parameter    DescriptionRows
#1 to 3$       mu (1-3)     Minnesota prior shrinkage parameters for means.
#Rows 4 to 6   delta (1-3)  Persistence / autoregressive shrinkage parameters.
#Rows 7 to 9   psi(1-3)     Scale parameters for individual equations.
#Row 10$       lambda       Overall Minnesota prior tightness hyperparameter.
#Row 11$       alpha        Lag decay hyperparameter.
#Rows 12 to 16 Additional   Dummy observation weights.

#------------------------------------------------------------------------------#
# Alpha
png(
"Analysis/BVAR_Sign/Model_01/Alpha_Traces.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

plot(run_bsvar_sign_model_01$posterior$hyper[11, ], 
  type = "l",
  ylab = "alpha",
  col = "#000000"
)

dev.off()

# Lambda
png(
  "Analysis/BVAR_Sign/Model_01/Lambda_Traces.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
plot(run_bsvar_sign_model_01$posterior$hyper[10, ], 
  type = "l", 
  ylab = "lambda", 
  col = "#000000")
dev.off()


#------------------------------------------------------------------------------#

# ACF for Alpha
png(
  "Analysis/BVAR_Sign/Model_01/ACF_Alpha.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
acf(run_bsvar_sign_model_01$posterior$hyper[11, ]
, main = "ACF - Lambda")

dev.off()


# ACF for Lambda to check chain mixing
png(
  "Analysis/BVAR_Sign/Model_01/ACF_Lambda.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
acf(run_bsvar_sign_model_01$posterior$hyper[10, ]
, main = "ACF - Lambda")

dev.off()



#------------------------------------------------------------------------------#
# Density Plot

# Alpha
png(
"Analysis/BVAR_Sign/Model_01/Density_Alpha.png",
 width  = 3200,
 height = 2600,
 res    = 300
)

plot(
  density(run_bsvar_sign_model_01$posterior$hyper[11, ]), 
  main = "Posterior Density of Alpha", 
  xlab = "alpha", 
  col = "#000000",
  lwd = 2
)
dev.off()

# Lambda
png(
"Analysis/BVAR_Sign/Model_01/Density_Lamba.png",
 width  = 3200,
 height = 2600,
 res    = 300
)

plot(
  density(run_bsvar_sign_model_01$posterior$hyper[10, ]), 
  main = "Posterior Density of Lambda", 
  xlab = "lambda", 
  col = "#000000",
  lwd = 2
)
dev.off()

#------------------------------------------------------------------------------#
# Export Hyperparamaters

export_hyperparameters(
  estimation_obj = run_bsvar_sign_model_01,
  target_cols    = target_cols,
  file_path      = "Analysis/BVAR_Sign/Model_01/hyperparameter_summary.csv"
)

#------------------------------------------------------------------------------#
# Effective Sample Size

# 1. Extract hyperparameter posterior chains from estimated model
# (Adjust path if needed based on bsvarSIGNs structure)
posterior_lambda <- run_bsvar_sign_model_01$posterior$hyper[10, ]
posterior_alpha  <- run_bsvar_sign_model_01$posterior$hyper[11, ]

# 2. Compute Effective Sample Size (ESS) using the coda package
ess_lambda <- effectiveSize(as.mcmc(posterior_lambda))
ess_alpha  <- effectiveSize(as.mcmc(posterior_alpha))

# Create data frame in R
ess_summary <- data.frame(
  Parameter = c("Lambda", "Alpha"),
  ESS       = c(round(ess_lambda, 1), round(ess_alpha, 1))
)

# Export to CSV
write.csv(ess_summary, file = "Analysis/BVAR_Sign/Model_01/ESS_Diagnostics_Model_01.csv", row.names = FALSE)

 
#------------------------------------------------------------------------------#
# Export Hyperparamaters

export_hyperparameters(
  estimation_obj = run_bsvar_sign_model_01,
  target_cols    = target_cols,
  file_path      = "Analysis/BVAR_Sign/Model_01/hyperparameter_summary.csv"
)


#============================================================================#
#                              [6] IRF
#============================================================================#


irf_updated <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = H_total
)

# Export IRF Results to CSV
irf_df <- export_bvar_results_to_csv(
  bvar_array = irf_updated,
  var_names  = target_cols,
  file_name  = "Analysis/BVAR_Sign/Model_01/IRF_Summary_Results.csv"
)


#------------------------------------------------------------------------------#
# Policy Shock on Gdp Growth

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
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_GDP.png"
)

#------------------------------------------------------------------------------#
# Policy Shock on Consumption Growth

# 1. Extract IRF data frame
df_consumption_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "consumption_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_consumption_policy,
  response_name = "Consumption Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Consumption.png"
)

#------------------------------------------------------------------------------#
# Policy Shock on Inflation

# 1. Extract IRF data frame
df_cpi_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "cpi_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_cpi_policy,
  response_name = "CPI",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_CPI.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on Savings Rate

# 1. Extract IRF data frame
df_saving_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "saving_rate", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_saving_policy,
  response_name = "Savings Rate",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Savings.png"
)


#------------------------------------------------------------------------------#
# Interest Burden

# 1. Extract IRF data frame
df_interest_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "interest_burden", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_interest_policy,
  response_name = "Interest Burden",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Interest.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on Debt Growth

# 1. Extract IRF data frame
df_debt_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "debt_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_debt_policy,
  response_name = "Debt Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Debt_Growth.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on House Price Growth

# 1. Extract IRF data frame
df_house_price_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "real_house_price_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_house_price_policy,
  response_name = "Real House Price Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Real_House_Growth.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on Household's Wealth

# 1. Extract IRF data frame
df_wealth_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "asset_liability_ratio", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_wealth_policy,
  response_name = "Asset to Liability Ratio",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Wealth.png"
)


#============================================================================#
#                              [7] FEVD
#============================================================================#

fevd_updated <- compute_fevd_from_irf2(irf_updated)
