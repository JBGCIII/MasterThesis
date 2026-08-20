

# 1. Load Data & Select Target Columns
bvar_data <- read.csv("Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")
target_cols <- c(
  "gdp_growth", "consumption_growth", "cpi_growth", "saving_rate",
  "policy_rate", "interest_burden", 
  "debt_growth", "real_house_price_growth", "asset_liability_ratio"
)
# bsvars requires a numeric matrix
bvar_matrix <- as.matrix(bvar_data[, target_cols])

# 2. Prior Setup (TRUE = Random Walk, FALSE = White Noise)
is_random_walk <- c(
  FALSE, # gdp_growth
  FALSE, # consumption_growth
  FALSE, # cpi_growth
  TRUE,  # saving_rate
  TRUE,  # policy_rate
  TRUE,  # interest_burden
  FALSE, # debt_growth
  FALSE, # real_house_price_growth
  TRUE   # asset_liability_ratio
)

# 3. 3D Array Sign Restriction Setup [Variables, Shocks, Horizons]
N <- ncol(bvar_matrix)
H_total <- 20
H_restrict <- 2 # Constrain impact and the following quarter

# The h-th slice of the NxNxH array represents the h-1 horizon
sign_irf_3d <- array(NA, dim = c(N, N, H_total))

# Constrain the Monetary Policy Shock (Column 5) for slices 1 and 2
for (h in 1:H_restrict) {
  sign_irf_3d[5, 5, h] <-  1   # policy_rate (+)
  sign_irf_3d[3, 5, h] <- -1   # cpi_growth (-)
  sign_irf_3d[1, 5, h] <- -1   # gdp_growth (-)
  sign_irf_3d[2, 5, h] <- -1   # consumption_growth (-)
}

# 4. Model Specification
spec <- specify_bsvarSIGN$new(
  data       = bvar_matrix,
  p          = 4, 
  sign_irf   = sign_irf_3d,
  stationary = is_random_walk
)

# 5. C++ Estimation & Outputs
# The estimate() function executes the Gibbs sampler
run_bsvar_sign <- estimate(spec, S = 20000)

irf_updated <- compute_impulse_responses(run_bsvar_sign, horizon = H_total)
fevd_updated <- compute_variance_decompositions(run_bsvar_sign, horizon = H_total)

plot(irf_updated)
plot(fevd_updated)