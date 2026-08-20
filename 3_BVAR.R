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


# ==========================================================================================#
#                                    Diagnostics                                            #

# -------------------------------------------------------------
# Run Preliminary IRFs
# -------------------------------------------------------------
irf_preliminary <- irf(
  run_bvar,
  horizon = 20,
  fevd    = TRUE
)

# Extract the FEVD component first
fevd_obj <- fevd(irf_preliminary)

# Plot FEVD
plot(fevd_obj)


# Focus on a single target variable
plot(fevd_obj, vars = "gdp_growth")


# 1. Take the posterior mean across MCMC draws (dim 1) -> 3D Array [Vars, Horizon, Shocks]
fevd_mean <- apply(fevd_obj$fevd, c(2, 3, 4), mean)

# 2. Assign dimension names
var_names <- fevd_obj$variables
dimnames(fevd_mean) <- list(
  Response = var_names,
  Horizon  = 1:dim(fevd_mean)[2],
  Shock    = var_names
)

# 3. Flatten array into a long Data Frame
fevd_df <- as.data.frame.table(fevd_mean, responseName = "Variance_Share") %>%
  mutate(Horizon = as.numeric(as.character(Horizon)))

# 4. Plot stacked area decomposition
ggplot(fevd_df, aes(x = Horizon, y = Variance_Share, fill = Shock)) +
  geom_area(alpha = 0.85, color = "black", linewidth = 0.2) +
  facet_wrap(~ Response, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Bayesian FEVD Posterior Means",
    x = "Forecast Horizon",
    y = "% Variance Explained",
    fill = "Shock Source"
  )


class(fevd_obj)
str(fevd_obj, max.level = 1)

# Assign the missing class attribute
class(fevd_obj) <- "bvar_fevd"

# Now standard plot will work
plot(fevd_obj)



# Option A: Extract FEVD with proper class directly from the IRF summary
plot(summary(irf_preliminary), FEVD = TRUE)

# Option B: Pass FEVD parameter directly to plot
plot(irf_preliminary, FEVD = TRUE)


plot(irf_preliminary)


# 1. Compute FEVD directly from the main model fit
fevd_fit <- fevd(run_bvar, response = "gdp_growth",  horizon = 20,
  fevd    = TRUE)

# 2. Assign the bvar_fevd class
class(fevd_fit) <- "bvar_fevd"

# 3. Plot
plot(fevd_fit)









BVAR:::plot.bvar_fevd(bv_fevd)


?irf.bvar
vignette("BVAR")



var_order <- colnames(bvar_ts)
# Expect: "gdp_growth" "consumption_growth" "debt_growth"
#         "asset_liability_ratio" "saving_rate" "interest_burden"
#         "real_house_price_growth" "policy_rate"
print(var_order)
 
K <- length(var_order)
sign_restr <- matrix(NA, nrow = K, ncol = K,
                      dimnames = list(var_order, var_order))



shock_col <- "debt_growth"
 
sign_restr["debt_growth", shock_col]             <-  1
sign_restr["real_house_price_growth", shock_col]  <-  1
# sign_restr["policy_rate", shock_col]            <-  1  # uncomment for option (b)
 
print(sign_restr)


 

 
# -------------------------------------------------------------
# Run identified IRFs
# -------------------------------------------------------------
irf_signed <- irf(
  run_bvar,
  horizon    = 20,
  fevd       = TRUE,
  sign_restr = sign_restr,
  sign_lim   = 1000000   # max rotations attempted before giving up;
                        # raise if you get too few accepted draws
)
 
plot(irf_signed)
 


 # 1. Run unconstrained Cholesky IRFs
irf_chol <- irf(run_bvar, horizon = 20)

# 2. Plot the impulse response functions
plot(irf_chol)









