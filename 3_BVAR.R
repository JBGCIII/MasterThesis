##################################### 3.BVAR ################################################
##                                                                                         ##

# ==================================== 0.Directory ========================================#

# ==========================================================================================#
#                                           BVAR                                            #

bvar_data <- read.csv("Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")
 

# 1. Target Data
target_cols <- c(
  "gdp_growth", "consumption_growth", "debt_growth", 
  "asset_liability_ratio", "saving_rate", 
  "interest_burden", "real_house_price_growth", "policy_rate"
)

bvar_ts <- stats::ts(
  data = as.matrix(bvar_data[, target_cols]),
  start = c(1996, 2),
  frequency = 4
)

# 2. Prior Vector (0 = Stationary, 1 = Non-Stationary)
prior_b_vec <- c(
  "gdp_growth"              = 0,
  "consumption_growth"        = 0,
  "debt_growth"               = 0,
  "asset_liability_ratio"     = 1,
  "saving_rate"               = 1,
  "interest_burden"           = 1,
  "real_house_price_growth"   = 0,
  "policy_rate"               = 1
)

# 3. Minnesota Prior
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 1e-4, max = 5),
  alpha  = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
  b      = prior_b_vec
)

# If you want to estimate both hyperparameters and achieve 35-55% acceptance rate:
priors_dual <- bv_priors(hyper = c("lambda", "alpha"), mn = mn)

# Tune adaptive proposal step size:
mh_spec_adjusted <- bv_mh(
  adjust_acc = TRUE,
  acc_lower  = 0.30,
  acc_upper  = 0.50,
  acc_change = 0.05
)

run_bvar <- bvar(
  bvar_ts,
  lags    = 4,
  priors  = priors_dual,
  mh      = mh_spec_adjusted,
  n_draw  = 200000,
  n_burn  = 20000,
  n_thin  = 10,
  verbose = TRUE
)

summary(run_bvar)


# ==========================================================================================#
#                                    Diagnostics                                            #


# 3. Save diagnostic plot
png("Analysis/Figure_3_MCMC_Trace.png", width = 10, height = 6, units = "in", res = 300)
plot(run_bvar, type = "trace")
dev.off()


png("Analysis/Figure_3_MCMC_Density.png", width = 10, height = 6, units = "in", res = 300)
plot(run_bvar, type = "density")
dev.off()


companion(run_bvar)
C <- companion(run_bvar)
eig <- eigen(C)$values
sorted_eig  <- sort(Mod(eig), decreasing = TRUE)

# B. Calculate Effective Sample Size (ESS) using the coda package
hyper_draws <- run_bvar$hyper # Extract posterior parameter draws
ess_vals    <- coda::effectiveSize(coda::as.mcmc(hyper_draws))

write.csv(ess_vals, "Analysis/ess_vals.csv")
write.csv(sorted_eig, "Analysis/sorted_eig.csv")


