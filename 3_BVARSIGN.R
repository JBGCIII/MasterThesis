


library(bsvars)
library(bsvarSIGNs)



# ============================================================
# 1. DATA
# ============================================================

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

N <- ncol(bvar_matrix)


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

H_total <- 20

# Start with impact-period restrictions only
H_restrict <- 1

sign_irf <- array(
  NA,
  dim = c(N, N, H_total)
)

# Monetary policy shock
for (h in 1:H_restrict) {

  sign_irf[policy_idx, policy_idx, h] <-  1
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
  S = 50000
)

# ============================================================
# IRFs
# ============================================================

irf_updated <- compute_impulse_responses(
  run_bsvar_sign,
  horizon = H_total
)

# ============================================================
# FEVD
# ============================================================

fevd_updated <- compute_variance_decompositions(
  run_bsvar_sign,
  horizon = H_total
)

plot(irf_updated)
plot(fevd_updated)




hd <- compute_historical_decompositions(run_bsvar_sign)



irf_updated