


# ==========================================================================================#
#                                      Set Up                                           

# 1. Load Data
bvar_data <- read.csv("Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")

# 2. Target Data Selection
target_cols <- c(
"gdp_growth", "consumption_growth", "cpi_growth", "saving_rate", # Real Block
"policy_rate", "interest_burden", # Policy Instrument
"debt_growth", "real_house_price_growth", "asset_liability_ratio" # Financial block
)

# 3. Transform to ts format.
bvar_ts <- stats::ts(
  data = as.matrix(bvar_data[, target_cols]),
  start = c(1996, 2),
  frequency = 4
)


# ==========================================================================================#
#                                      Prior Set Up                                            

# 1. Prior Vector (0 = Stationary, 1 = Non-Stationary)
prior_b_vec <- c(
"gdp_growth" = 0, 
"consumption_growth" = 0, 
"cpi_growth" = 0, 
"saving_rate" = 1,
"policy_rate" =1 , 
"interest_burden" = 1, 
"debt_growth" = 0, 
"real_house_price_growth" = 0, 
"asset_liability_ratio" = 1
)

# 2. Minnesota Prior Setup
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.2, min = 1e-4, max = 2),
  alpha  = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
  b      = prior_b_vec
)

priors_dual <- bv_priors(hyper = c("lambda", "alpha"), mn = mn)

# 3. MCMC & Adaptive Tuning Specification
mh_spec_adjusted <- bv_mh(
  adjust_acc = TRUE,
  acc_lower  = 0.30,
  acc_upper  = 0.50,
  acc_change = 0.05
)

# 4. Run Model with 2 Lags to Preserve Degrees of Freedom
# (33 params/eq with 4 lags vs 17 params/eq with 2 lags on 115 obs)
run_bvar_updated <- bvar(
  bvar_ts,
  lags    = 4,
  priors  = priors_dual,
  mh      = mh_spec_adjusted,
  n_draw  = 200000,
  n_burn  = 20000,
  n_thin  = 10,
  verbose = TRUE
)

# ==========================================================================================#
#                                   Run identified IRFs
# Positions from your actual var_order:
# [1] gdp_growth  [2] consumption_growth  [3] cpi_growth
# [4] saving_rate [5] policy_rate         [6] interest_burden
# [7] debt_growth [8] real_house_price_growth [9] asset_liability_ratio

gdp_idx <- which(var_order == "gdp_growth")
consumption_idx <- which(var_order == "consumption_growth")
cpi_idx <- which(var_order == "cpi_growth")
saving_idx <- which(var_order == "saving_rate")
policy_idx <- which(var_order == "policy_rate")
interest_burden_idx <- which(var_order == "interest_burden")
debt_idx <- which(var_order == "debt_growth")
house_idx <- which(var_order == "real_house_price_growth")
asset_liability_idx <- which(var_order == "asset_liability_ratio")

sign_restr_policy <- matrix(NA, nrow = N, ncol = N)


 #    [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
 #[1,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[2,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[3,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[4,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[5,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[6,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[7,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[8,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
 #[9,]   NA   NA   NA   NA   NA   NA   NA   NA   NA

# Impose restrictions on the Monetary Policy Shock (Column 5)
sign_restr_policy[policy_idx,          policy_idx] <-  1   # policy_rate (+)
sign_restr_policy[cpi_idx,             policy_idx] <- -1  # cpi_growth (-)
sign_restr_policy[interest_burden_idx, policy_idx] <-  1   # interest_burden (+)
sign_restr_policy[debt_idx,            policy_idx] <- -1  # debt_growth (-)

sign_restr_policy

# Pass directly to irf()
irf_policy <- irf(
  run_bvar_updated,
  horizon    = 20,
  fevd       = TRUE,
  sign_restr = sign_restr_policy,
  sign_lim   = 1000000
)



# Attempt plotting only column/shock 5 across target variables
plot(irf_policy, vars = 1:9, shock = 5)


par(mfrow = c(3, 3))
apply(irf_policy$quants[, , , 5], 2, function(x) {
  matplot(t(x), type = "l", lty = c(2, 1, 2), col = c("red", "black", "red"), ylab = "")
})

par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))

sapply(1:9, function(i) {
  matplot(t(irf_policy$quants[, i, , 5]), type = "l", 
          lty = c(2, 1, 2), col = c("red", "black", "red"),
          main = irf_policy$variables[i], xlab = "Horizon", ylab = "")
  abline(h = 0, lty = 3, col = "gray")
})




library(bsvars)
library(bsvarSIGNs)

install.packages("bsvars")
install.packages("bsvarSIGNs")
















# 1. Define dimensions
N <- length(var_order)
h_total <- 20
h_restrict <- 2 # Restrict only quarters 1 and 2 (6 months)

# 2. Create a 3D Array: [Variables, Shocks, Horizon]
sign_restr_3d <- array(NA, dim = c(N, N, h_total))

# 3. Populate restrictions for Quarters 1 to h_restrict
for (h in 1:h_restrict) {
  # Standard Monetary Policy Shock Identification (Column 5)
  sign_restr_3d[policy_idx,          policy_idx, h] <-  1   # policy_rate (+)
  sign_restr_3d[cpi_idx,             policy_idx, h] <- -1   # cpi_growth (-)
  sign_restr_3d[gdp_idx,             policy_idx, h] <- -1   # gdp_growth (-)
  sign_restr_3d[consumption_idx,     policy_idx, h] <- -1   # consumption_growth (-)
  
  # Leave debt_growth, interest_burden, house_prices, and asset_liability_ratio 
  # as NA across all quarters to let the data speak on household vulnerability!
}

# 4. Pass the 3D array to irf()
irf_policy_short <- irf(
  run_bvar_updated,
  horizon    = h_total,
  fevd       = TRUE,
  sign_restr = sign_restr_3d,
  sign_lim   = 1000000
)

# 5. Plot results
plot(irf_policy_short)







# ==========================================================================================#
#                                      FEVD                                           
# Unfortunately  with BVAR 1.0.5. FEVD plot() method isn't available

# ==========================================================================================
#                   Correct Multi-Dimensional FEVD Calculation
# ==========================================================================================

# ==========================================================================================
#              2-Color FEVD: Monetary Policy Shock vs. Unidentified Variance
# ==========================================================================================

n_vars  <- length(irf_policy$variables)
horizon <- 20

# Matrix to hold 2 components: [1 = Monetary Policy Shock, 2 = All Other Shocks Combined]
fevd_binary <- array(0, dim = c(n_vars, 2, horizon))

for (i in 1:n_vars) {
  for (h in 1:horizon) {
    
    # 1. Variance from Monetary Policy Shock (Shock 5)
    irf_mp <- irf_policy$quants[2, i, 1:h, policy_idx]
    var_mp <- sum(irf_mp^2)
    
    # 2. Variance from ALL OTHER Shocks combined
    var_other <- 0
    for (j in (1:n_vars)[-policy_idx]) {
      irf_other <- irf_policy$quants[2, i, 1:h, j]
      var_other <- var_other + sum(irf_other^2)
    }
    
    # Total system variance at horizon h
    total_var <- var_mp + var_other
    
    # Proportions (%)
    if (total_var > 0) {
      fevd_binary[i, 1, h] <- 100 * (var_mp / total_var)
      fevd_binary[i, 2, h] <- 100 * (var_other / total_var)
    } else {
      fevd_binary[i, 1, h] <- 50
      fevd_binary[i, 2, h] <- 50
    }
  }
}

# ==========================================================================================
#                                     Clean Plotting
# ==========================================================================================

vars <- irf_policy$variables

# Two contrasting colors: Teal for Policy, Light Gray for Other Shocks
binary_colors <- c("#1b9e77", "#e7e1ef")

png(
  "FEVD_Monetary_Policy_Contribution.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

par(
  mfrow = c(3, 3),
  mar   = c(3.5, 4, 2.5, 1),
  oma   = c(4, 1, 1, 1)
)

for (i in 1:n_vars) {
  fevd_slice <- fevd_binary[i, , ] # [2 x 20]
  
  barplot(
    fevd_slice,
    beside    = FALSE,
    col       = binary_colors,
    names.arg = ifelse(1:20 %in% c(1, 5, 10, 15, 20), paste0("Q", 1:20), ""),
    xlab      = "Quarter",
    ylab      = "FEVD (%)",
    main      = vars[i],
    ylim      = c(0, 100),
    las       = 1,
    cex.names = 0.85
  )
}

# Global Legend
par(xpd = NA)
legend(
  x         = "bottom",
  inset     = c(0, -0.32),
  legend    = c("Monetary Policy Shock", "Other / Unidentified Shocks"),
  fill      = binary_colors,
  ncol      = 2,
  cex       = 1.2,
  bty       = "n"
)

dev.off()