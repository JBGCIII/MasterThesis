###############################################################################
################################# 2. BVAR SIGNS ###############################
###############################################################################

# Comments. Originally I planned to run one script for each model, but due to
# the crash issues with the BVARSIGN and BSVARDS I went for a modular approach
# I will therefore first estimate the models, the model diagnostics
# before conducting the IRFS, FEVD and Historical Decomposition.

#------------------------------------------------------------------------------#

# Main folder for analysis
dir.create("Analysis", showWarnings = FALSE)

# Sub-folder for BVAR Sign.
dir.create("Analysis/BVAR_Sign", showWarnings = FALSE)

# Sub-folder for Each Model
dir.create("Analysis/BVAR_Sign/Model_01", showWarnings = FALSE) # Policy
dir.create("Analysis/BVAR_Sign/Model_02", showWarnings = FALSE)
dir.create("Analysis/BVAR_Sign/Model_03", showWarnings = FALSE)
dir.create("Analysis/BVAR_Sign/Model_04", showWarnings = FALSE)



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
bvar_matrix <- as.matrix(
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
N <- ncol(bvar_matrix) # nolint
H_total <- 20 # Time Periods (5 years)

#============================================================================#
#                              [2] Sign Restriction Set Up
#============================================================================#
## Array Creation
# [Model 1: Policy Rate Shock]
sign_policy <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

# [Model 2: Credit Supply Shock]
sign_credit <- array(
  NA, 
  dim = c(N, N, H_total) 
)

# [Model 3: Housing Demand Shock]
sign_housing <- array(
  NA, 
  dim = c(N, N, H_total) 
)

# [Model 4: Wealth Shock]
sign_wealth <- array(
  NA, 
  dim = c(N, N, H_total) 
)

#============================================================================#
## Sign Restriction Set Up

# [Model 1: Policy Rate Shock]
H_restrict_policy <- 2 # How long the restriction will apply.

for (h in 1:H_restrict_policy) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.

  sign_policy[policy_idx, policy_idx, h] <-  1  # (+) shock in policy
  sign_policy[cpi_idx, policy_idx, h] <-     -1 # (-) in Inflation
  sign_policy[interest_burden_idx, policy_idx, h] <-  1 # (+) in paid interest
  sign_policy[debt_idx, policy_idx, h] <-    -1 # (-) in debt.
}

#------------------------------------------------------------------------------#

# [Model 2: Credit Supply Shock]
H_restrict_supply <- 2 # How long the restriction will apply.
for (h in 1:H_restrict_supply) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.
  sign_credit[debt_idx, debt_idx, h] <-  -1 # (-) shock to credit supply
  sign_credit[interest_burden_idx, debt_idx, h] <- 1 # Not policy to differ.
  sign_credit[house_idx, debt_idx, h] <-  -1 # (-)
}


#------------------------------------------------------------------------------#

# [Model 3: Housing Demand Shock]
H_restrict_demand <- 2 # How long the restriction will apply.

for (h in 1:H_restrict_demand) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.

  sign_housing[house_idx, house_idx, h] <-  1  
  sign_housing[debt_idx, house_idx, h] <-     1 
  sign_housing[gdp_idx, house_idx, h] <-  1 # Positive shock in Interest.
}

#------------------------------------------------------------------------------#

# [Model 4: Wealth Shock]
H_restrict_wealth <- 2 # How long the restriction will apply.

for (h in 1:H_restrict_wealth) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.

  sign_wealth[house_idx, house_idx, h] <-  -1  # Positive shock in policy
  sign_wealth[asset_liability_idx, house_idx, h] <-     -1 # Negative shock in CPI
  sign_wealth[policy_idx, house_idx, h] <-  0 # Positive shock in Interest.
}

#============================================================================#
#                              [3] Model Specs
#============================================================================#

# bsvarSIGNs convention:
#TRUE = Random Walk (Mean = 1)
#FALSE = White Noise (Mean = 0)
# [stationary] an N logical vector - its element set to FALSE sets the
# prior mean for the autoregressive parameters of the Nth equation to
# the white noise process, otherwise to random walk."
# Source: https://bsvars.org/bsvarSIGNs/reference/specify_bsvarSIGN.html


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

#------------------------------------------------------------------------------#
# Specs Model 01

spec_model_01 <- specify_bsvarSIGN$new(
  data         = bvar_matrix,
  p            = 4,
  sign_irf     = sign_policy,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
# Specs Model 02

spec_model_02 <- specify_bsvarSIGN$new(
  data         = bvar_matrix,
  p            = 4,
  sign_irf     = sign_credit,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
# Specs Model 03

spec_model_03 <- specify_bsvarSIGN$new(
  data         = bvar_matrix,
  p            = 4,
  sign_irf     = sign_housing,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

#------------------------------------------------------------------------------#
# Specs Model 04

spec_model_04 <- specify_bsvarSIGN$new(
  data         = bvar_matrix,
  p            = 4,
  sign_irf     = sign_wealth,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)


#============================================================================#
#                              [4] Model Run
#============================================================================#

# Optimal hyperparameter starting points using Adaptive Metropolis
set.seed(321)

spec_model_01$estimate_hyper(S = 40000, burn_in = 20000)
spec_model_02$estimate_hyper(S = 40000, burn_in = 20000)
spec_model_03$estimate_hyper(S = 40000, burn_in = 20000)
spec_model_04$estimate_hyper(S = 40000, burn_in = 20000)

#============================================================================#
# Things will take a while from now. Go, grab coffee or something.
# The model should be saved and run by the time you come back.
#      .-~~-.
#    ,|`-__-'|            
#    ||      |     COFFEE OR TEA
#    `|      |
#      `-__-'
# Saving model I found was a good way to prevent me from rerunning things.
# It also allowed to move models to other script, which is usefull for diagnostiscs
# when comparing multiple models.
#============================================================================#

run_bsvar_sign_model_01 <- estimate(
  spec_model_01,
  S = 20000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

saveRDS(
  run_bsvar_sign_model_01,
  "Analysis/BVAR_Sign/Model_01/run_bsvar_sign_model_1_20000.rds"
)

rm(run_bsvar_sign_model_01)
gc()

#------------------------------------------------------------------------------#

run_bsvar_sign_model_02 <- estimate(
  spec_model_02,
  S = 20000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

saveRDS(
  run_bsvar_sign_model_02,
  "Analysis/BVAR_Sign/Model_02/run_bsvar_sign_model_2_20000.rds"
)

rm(run_bsvar_sign_model_02)
gc()

#------------------------------------------------------------------------------#

run_bsvar_sign_model_03 <- estimate(
  spec_model_03,
  S = 20000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

saveRDS(
  run_bsvar_sign_model_03,
  "Analysis/BVAR_Sign/Model_03/run_bsvar_sign_model_3_20000.rds"
)

rm(run_bsvar_sign_model_03)
gc()
#------------------------------------------------------------------------------#

run_bsvar_sign_model_04 <- estimate(
  spec_model_04,
  S = 20000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

saveRDS(
  run_bsvar_sign_model_04,
  "Analysis/BVAR_Sign/Model_04/run_bsvar_sign_model_4_20000.rds"
)

rm(run_bsvar_sign_model_04)
gc()





