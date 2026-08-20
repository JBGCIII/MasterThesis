


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
  lags    = 2,
  priors  = priors_dual,
  mh      = mh_spec_adjusted,
  n_draw  = 200000,
  n_burn  = 20000,
  n_thin  = 10,
  verbose = TRUE
)


summary(run_bvar_updated)

# 5. Compute Updated FEVD
irf_updated <- irf(
  run_bvar_updated,
  horizon = 20,
  fevd    = TRUE
)

plot(irf_updated)

# ==========================================================================================#
#                                      FEVD                                           
# Unfortunately  with BVAR 1.0.5. FEVD plot() method isn't available

# 1. Plot FEVD Results
fevd_median <- 100 * irf_updated$fevd$quants[2, , , ]
vars <- irf_updated$variables
n_vars <- length(vars)

# 2. Fix Palette: Generate distinct colors for all variables (supports > 8 vars)
var_colors <- hcl.colors(n = n_vars, palette = "Set 2")

png(
  "FEVD_all_variables.png",
  width = 3600,
  height = 3000,
  res = 300
)

par(
  mfrow = c(5, 2),
  mar = c(4, 4.5, 2.5, 1),
  oma = c(5, 0, 0, 0)
)

# 3. Plot each variable
for (i in 1:n_vars) {
  # Fix Dimensions: t() ensures matrix is [shocks (10) x horizons (20)]
  fevd_slice <- t(fevd_median[i, , ])
  
  barplot(
    fevd_slice,
    beside = FALSE,
    col = var_colors,
    names.arg = ifelse(
      1:20 %in% c(1, 5, 10, 15, 20),
      paste0("Q", 1:20),
      ""
    ),
    xlab = "Quarter",
    ylab = "FEVD (%)",
    main = vars[i],
    ylim = c(0, 100),
    las = 1,
    cex.names = 0.8
  )
}

# 4. Add single global legend in outer bottom margin
par(xpd = NA)
# 5. Add single legend in bottom right
par(xpd = NA)
legend(
  x = "bottomright",
  inset = c(-1.0, -0.55), # Adjust vertical offset (second value) as needed
  legend = vars,
  fill = var_colors,
  ncol = 2,            # Reduced columns to fit better in the corner
  cex = 1.5,             # Increased from 0.85 to enlarge text and boxes
  x.intersp = 1.1,       # Increased spacing between color boxes and text
  y.intersp = 1.1,       # Added vertical spacing between rows
  bty = "n"
)
dev.off()

# ==========================================================================================#
#                                      Sign Restriction                                           
# ==========================================================================================#


# ==========================================================================================#
#                                       Sign Restriction                                    
# ==========================================================================================#

# ==========================================================================================#
#                                       Sign Restriction                                    
# ==========================================================================================#

# 1. Define variable ordering vector
var_order <- c(
  "gdp_growth", "consumption_growth", "cpi_growth", "saving_rate",
  "policy_rate", "interest_burden", "debt_growth", 
  "real_house_price_growth", "asset_liability_ratio"
)

# 2. Extract position indices using var_order
gdp_idx             <- which(var_order == "gdp_growth")
consumption_idx     <- which(var_order == "consumption_growth")
cpi_idx             <- which(var_order == "cpi_growth")
saving_idx          <- which(var_order == "saving_rate")
policy_idx          <- which(var_order == "policy_rate")
interest_burden_idx <- which(var_order == "interest_burden")
debt_idx            <- which(var_order == "debt_growth")
house_idx           <- which(var_order == "real_house_price_growth")
asset_liability_idx <- which(var_order == "asset_liability_ratio")

# Reset restriction matrix
sign_restr_final <- matrix(NA, nrow = 9, ncol = 9)
rownames(sign_restr_final) <- var_order
colnames(sign_restr_final) <- paste0("shock_", 1:9)

# Impose ONLY core monetary restrictions on Shock 5
sign_restr_final[policy_idx,          policy_idx] <-  1   # policy_rate (+)
sign_restr_final[cpi_idx,             policy_idx] <- -1   # cpi_growth (-)
sign_restr_final[interest_burden_idx, policy_idx] <-  1   # interest_burden (+)
sign_restr_final[debt_idx,            policy_idx] <- -1   # debt_growth (-)

# Compute IRFs with relaxed restrictions
irf_spec <- bv_irf(
  horizon    = 20,
  sign_restr = sign_restr_final,
  sign_lim   = 2000000 # Increased iteration limit
)

irf_signed_tight <- irf(run_bvar_updated, spec = irf_spec)



# 7. Plot 3x3 Grid for Monetary Policy Shock (Shock 5)
png("IRF_Monetary_Policy_Shock.png", width = 2400, height = 2400, res = 300)

par(mfrow = c(3, 3), mar = c(3, 3, 2.5, 1), oma = c(0, 0, 2, 0))

sapply(1:9, function(i) {
  matplot(t(irf_signed_tight$quants[, i, , policy_idx]), 
          type = "l", lty = c(2, 1, 2), col = c("red", "black", "red"), lwd = c(1, 2, 1),
          main = var_order[i], xlab = "Horizon", ylab = "")
  abline(h = 0, lty = 3, col = "gray40")
})

mtext("Impulse Responses to Monetary Policy Shock (Shock 5)", outer = TRUE, cex = 1.1, font = 2)

dev.off()
par(mfrow = c(1, 1)) # Reset layout














# Positions from your actual var_order:
# [1] gdp_growth  [2] consumption_growth  [3] cpi_growth
# [4] saving_rate [5] policy_rate         [6] interest_burden
# [7] debt_growth [8] real_house_price_growth [9] asset_liability_ratio





var_order <- colnames(bvar_ts)
print(var_order)
 
# Define 9 Structural Shock Labels for the Columns
shock_names <- c("gdp_shock", "consumption_shock", "cpi_shock", 
                 "saving_shock", "monetary_shock", "interest_burden_shock", 
                 "credit_demand_shock", "housing_supply_shock", "asset_shock")


K <- length(var_order)
sign_restr <- matrix(NA, nrow = K, ncol = K,
                      dimnames = list(var_order, shock_names))

sign_restr

sign_restr["policy_rate", "monetary_shock"]            <- 1  # Positive Monetary Shock
sign_restr["cpi_growth", "monetary_shock"]             <- -1  # Negative effect on Inflation
sign_restr["interest_burden", "monetary_shock"]        <- 1
sign_restr["debt_growth", "monetary_shock"]            <- -1


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

# 1. Setup dimensions using N (number of variables)
N <- length(var_order)

# 2. Define standard 2D Sign Restriction Matrix (N x N)
# Rows = Variables responding | Cols = Structural Shocks
# Col 5 (policy_idx) defines the Monetary Policy Shock
sign_restr_final <- matrix(NA, nrow = N, ncol = N)

# Impose restrictions on the Monetary Policy Shock (Column 5)
sign_restr_final[policy_idx,          policy_idx] <-  1   # policy_rate (+)
sign_restr_final[cpi_idx,             policy_idx] <- -1   # cpi_growth (-)
sign_restr_final[interest_burden_idx, policy_idx] <-  1   # interest_burden (+)
sign_restr_final[debt_idx,            policy_idx] <- -1   # debt_growth (-)

print(sign_restr_final)

# Attempt plotting only column/shock 5 across target variables
plot(irf_signed, vars = 1:9, shock = 5)


par(mfrow = c(3, 3))
apply(irf_signed$quants[, , , 5], 2, function(x) {
  matplot(t(x), type = "l", lty = c(2, 1, 2), col = c("red", "black", "red"), ylab = "")
})

par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))

sapply(1:9, function(i) {
  matplot(t(irf_signed$quants[, i, , 5]), type = "l", 
          lty = c(2, 1, 2), col = c("red", "black", "red"),
          main = irf_signed$variables[i], xlab = "Horizon", ylab = "")
  abline(h = 0, lty = 3, col = "gray")
})





irf_signed <- irf(
  run_bvar_updated,
  horizon    = 20,
  fevd       = TRUE,
  sign_restr = sign_restr_final,
  sign_lim   = 1000000
)

plot(irf_signed)


#Dimensions
dim(irf_signed$irf)

# 1. Define variable and shock indices
hpg_idx <- 8  # real_house_price_growth
gdp_idx <- 1  # gdp_growth

# 2. Extract draws for this specific pair across all horizons
# Array slice dimensions: [18000 draws, 20 horizon steps]
draws_matrix <- irf_signed$irf[, hpg_idx, , gdp_idx]

# 3. Compute median (50%), lower (16%), and upper (84%) bounds across draws (MARGIN = 2)
med_val   <- apply(draws_matrix, 2, median)
lower_val <- apply(draws_matrix, 2, quantile, probs = 0.16)
upper_val <- apply(draws_matrix, 2, quantile, probs = 0.84)

# 4. Define Horizon (1 to 20)
horizon <- 1:ncol(draws_matrix)

# 5. Base R Plot with Credible Intervals
plot(horizon, med_val, type = "l", lwd = 2, col = "#005580",
     ylim = range(c(lower_val, upper_val)),
     xlab = "Horizon", ylab = "Response",
     main = "Response of Real House Price Growth to GDP Growth Shock")

# Add 68% credible interval (dashed lines)
lines(horizon, lower_val, lty = 2, col = "#005580")
lines(horizon, upper_val, lty = 2, col = "#005580")

# Add zero line
abline(h = 0, col = "red", lty = 3)

irf_signed$horizon
irf_signed$sign_restr


irf_signed$setup$sign_restr

str(irf_signed)
# look specifically for what dimension 3 of $irf represents,
# and whether there's a separate count of "successful" vs
# "attempted" posterior draws

# 1. Total successful identification draws stored:
n_accepted <- dim(irf_signed$irf)[1]
print(n_accepted)

# 2. Check setup configurations
irf_signed$setup$sign_restr


saving_idx   <- which(var_order == "saving_rate")
hpg_lower <- apply(hpg_response_draws, 2, quantile, 0.16)
hpg_upper <- apply(hpg_response_draws, 2, quantile, 0.84)
cbind(median = hpg_median_irf, lower = hpg_lower, upper = hpg_upper)

debt_response_draws <- irf_signed$irf[, debt_idx, , 7]
debt_median_irf <- apply(debt_response_draws, 2, median)
saving_response_draws <- irf_signed$irf[, saving_idx, , 7]  # get saving_rate's index
saving_median_irf <- apply(saving_response_draws, 2, median)
saving_lower <- apply(saving_response_draws, 2, quantile, 0.16)
saving_upper <- apply(saving_response_draws, 2, quantile, 0.84)
p_negative <- apply(saving_response_draws, 2, function(x) mean(x < 0))



# Positions from your actual var_order:
# [1] gdp_growth  [2] consumption_growth  [3] cpi_growth
# [4] saving_rate [5] policy_rate         [6] interest_burden
# [7] debt_growth [8] real_house_price_growth [9] asset_liability_ratio

debt_idx   <- which(var_order == "debt_growth")
hpg_idx    <- which(var_order == "real_house_price_growth")
policy_idx <- which(var_order == "policy_rate")

sign_restr_final <- matrix(NA, nrow = K, ncol = K)

sign_restr_final[debt_idx, debt_idx] <- 1
sign_restr_final[hpg_idx,  debt_idx] <- 1
# policy_rate left NA (unrestricted) per your "let the data decide" choice
# sign_restr_final[policy_idx, debt_idx] <- 1   # uncomment for the alternative option

irf_signed <- irf(
  run_bvar_updated,
  horizon    = 20,
  fevd       = TRUE,
  sign_restr = sign_restr_final,
  sign_lim   = 1000000
)

#Dimensions
#dim(irf_signed$irf)
#plot(irf_signed)

# 1. Define variable and shock indices
hpg_idx <- 8  # real_house_price_growth
gdp_idx <- 1  # gdp_growth

# 2. Extract draws for this specific pair across all horizons
# Array slice dimensions: [18000 draws, 20 horizon steps]
draws_matrix <- irf_signed$irf[, hpg_idx, , gdp_idx]

# 3. Compute median (50%), lower (16%), and upper (84%) bounds across draws (MARGIN = 2)
med_val   <- apply(draws_matrix, 2, median)
lower_val <- apply(draws_matrix, 2, quantile, probs = 0.16)
upper_val <- apply(draws_matrix, 2, quantile, probs = 0.84)

# 4. Define Horizon (1 to 20)
horizon <- 1:ncol(draws_matrix)

# 5. Base R Plot with Credible Intervals
plot(horizon, med_val, type = "l", lwd = 2, col = "#005580",
     ylim = range(c(lower_val, upper_val)),
     xlab = "Horizon", ylab = "Response",
     main = "Response of Real House Price Growth to GDP Growth Shock")

# Add 68% credible interval (dashed lines)
lines(horizon, lower_val, lty = 2, col = "#005580")
lines(horizon, upper_val, lty = 2, col = "#005580")

# Add zero line
abline(h = 0, col = "red", lty = 3)


str(irf_signed)
# look specifically for what dimension 3 of $irf represents,
# and whether there's a separate count of "successful" vs
# "attempted" posterior draws

# 1. Total successful identification draws stored:
n_accepted <- dim(irf_signed$irf)[1]
print(n_accepted)

# 2. Check setup configurations
irf_signed$setup$sign_restr
irf_signed$setup$sign_lim

# GDP response to the credit expansion shock (shock 7 = debt_idx)
gdp_response_draws <- irf_signed$irf[, gdp_idx, , debt_idx]
gdp_median_irf <- apply(gdp_response_draws, 2, median)
gdp_lower <- apply(gdp_response_draws, 2, quantile, 0.16)
gdp_upper <- apply(gdp_response_draws, 2, quantile, 0.84)
p_negative_gdp <- apply(gdp_response_draws, 2, function(x) mean(x < 0))

# Consumption response to the same shock
cons_response_draws <- irf_signed$irf[, cons_idx, , debt_idx]
cons_median_irf <- apply(cons_response_draws, 2, median)
cons_lower <- apply(cons_response_draws, 2, quantile, 0.16)
cons_upper <- apply(cons_response_draws, 2, quantile, 0.84)
p_negative_cons <- apply(cons_response_draws, 2, function(x) mean(x < 0))





# -------------------------------------------------------------
# 1. Compute historical decomposition
# -------------------------------------------------------------
# Confirm exact call signature against your installed version —
# ?hist_decomp.bvar — this may need irf_signed's identification
# passed in explicitly, or may recompute its own identification
# internally. Check whether it accepts your sign_restr scheme
# directly or works off the reduced-form model only.
 

hd <- hist_decomp(run_bvar_updated, type = "quantile")
str(hd)
# Inspect structure before indexing into it
str(hd)
# Expect something like [time, variable, shock] per the docs —
# confirm variable and shock ordering match var_order.
 
# -------------------------------------------------------------
# 2. Extract debt_growth shock's contribution to gdp_growth
# -------------------------------------------------------------
gdp_idx  <- which(var_order == "gdp_growth")
debt_idx <- which(var_order == "debt_growth")
 
# Adjust indexing once str(hd) confirms actual dimension order —
# this assumes [time, variable, shock]:
debt_contribution_to_gdp <- hd[, gdp_idx, debt_idx]
 
# -------------------------------------------------------------
# 3. Build counterfactual
# -------------------------------------------------------------
actual_gdp <- bvar_data$gdp_growth[(nrow(bvar_data) - length(debt_contribution_to_gdp) + 1):nrow(bvar_data)]
 
counterfactual_gdp <- actual_gdp - debt_contribution_to_gdp
 
dates <- time(bvar_ts)[(length(bvar_ts[,1]) - length(actual_gdp) + 1):length(bvar_ts[,1])]
 
# -------------------------------------------------------------
# 4. Plot full sample
# -------------------------------------------------------------
plot(dates, actual_gdp, type = "l", col = "black", lwd = 2,
     ylab = "GDP growth", xlab = "Time",
     main = "Actual vs. counterfactual (no credit shock) GDP growth")
lines(dates, counterfactual_gdp, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Actual", "Counterfactual (no credit shock)"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2))
 
# -------------------------------------------------------------
# 5. Zoom into the two episodes of interest
# -------------------------------------------------------------
plot_window <- function(start_year, end_year, title) {
  idx <- which(dates >= start_year & dates <= end_year)
  plot(dates[idx], actual_gdp[idx], type = "l", col = "black", lwd = 2,
       ylab = "GDP growth", xlab = "Time", main = title,
       ylim = range(c(actual_gdp[idx], counterfactual_gdp[idx])))
  lines(dates[idx], counterfactual_gdp[idx], col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c("Actual", "Counterfactual"),
         col = c("black", "red"), lwd = 2, lty = c(1, 2))
}
 
plot_window(2007, 2010, "2008-09 financial crisis: actual vs. counterfactual")
plot_window(2021.5, 2024, "2022-23 tightening cycle: actual vs. counterfactual")


# -------------------------------------------------------------
# Reading the result
# -------------------------------------------------------------
# Svensson-consistent (duck): counterfactual line sits close to
# actual through both episodes — removing the credit shock
# barely changes the GDP path, meaning debt didn't amplify the
# downturns beyond normal interest-rate/demand transmission.
#
# Rabbit reading: counterfactual sits meaningfully ABOVE actual
# during 2008-09 and/or 2022-23 — the credit shock was dragging
# GDP down beyond what other shocks alone would explain, i.e.
# household leverage amplified the downturn.
#
# Report both the point comparison AND whatever uncertainty
# hist_decomp's "quantile" output gives you — don't just eyeball
# the median lines, since your identification (from the sign-
# restriction exercise) has already shown wide bands are the
# norm here, not the exception.
 
gap <- actual_gdp - counterfactual_gdp
gap[dates >= 2007 & dates <= 2010]
gap[dates >= 2021.5 & dates <= 2024]















# ==============================================================================
# Structural Historical Decomposition for Sign-Restricted BVAR
# ==============================================================================

# 1. Extract dimensions and data
var_names <- colnames(bvar_ts)
K <- length(var_names)
T_len <- nrow(bvar_ts) - run_bvar_updated$meta$lags  # Effective sample size after lags

# 2. Extract companion matrix / coefficients and residuals
# Obtain posterior median estimates or process draw-by-draw
resids <- residuals(run_bvar_updated) # Reduced-form residuals [T_len x K]

# 3. Extract structural rotation matrix from irf_signed
# irf_signed stores successful structural impulse responses [draws x vars x horizon x shocks]
# The contemporaneous (h=1) IRFs give the structural impact matrix B_0 = P * Q
# Take the median structural impact matrix across accepted draws:
impact_draws <- irf_signed$irf[, , 1, ] # Horizon 1 impact matrix
B0_median <- apply(impact_draws, c(2, 3), median) # [K x K]

# 4. Compute structural shocks: \varepsilon_t^s = B_0^{-1} * u_t
struct_shocks <- t(solve(B0_median) %*% t(resids)) # [T_len x K]

# 5. Extract structural IRFs (\Theta_j) up to horizon T_len
# irf_signed$irf dimensions: [draws, response_var, horizon, shock_var]
irf_median <- apply(irf_signed$irf, c(2, 3, 4), median) # [K x horizon x K]

# 6. Compute Historical Decomposition array: [Time, Variable, Shock]
hd_structural <- array(0, dim = c(T_len, K, K),
                       dimnames = list(NULL, var_names, var_names))

for (t in 1:T_len) {
  for (k in 1:K) { # Shock k
    # Aggregate past structural shocks weighted by structural IRFs
    for (j in 0:(t - 1)) {
      shock_val <- struct_shocks[t - j, k]
      irf_weights <- irf_median[, j + 1, k] # IRF weight at lag j
      hd_structural[t, , k] <- hd_structural[t, , k] + irf_weights * shock_val
    }
  }
}

# ==============================================================================
# Extract Identified Credit Expansion Shock Contribution to GDP
# ==============================================================================
gdp_idx  <- which(var_names == "gdp_growth")
debt_idx <- which(var_names == "debt_growth")

# Identified structural contribution of debt_growth shock to gdp_growth
struct_debt_contrib_gdp <- hd_structural[, gdp_idx, debt_idx]

# Calculate true structural counterfactual
actual_gdp_trimmed <- bvar_ts[(run_bvar_updated$meta$lags + 1):nrow(bvar_ts), "gdp_growth"]
counterfactual_gdp_struct <- actual_gdp_trimmed - struct_debt_contrib_gdp

# Correct structural gap
gap_structural <- actual_gdp_trimmed - counterfactual_gdp_struct


gap_structural











sign_restr_final <- matrix(NA, nrow = 9, ncol = 9)
rownames(sign_restr_final) <- var_order
colnames(sign_restr_final) <- paste0("shock_", 1:9)

# Restrict ONLY Policy Rate (+) and Inflation (-)
sign_restr_final[policy_idx, policy_idx] <-  1
sign_restr_final[cpi_idx,    policy_idx] <- -1

irf_spec <- bv_irf(
  horizon    = 20,
  sign_restr = sign_restr_final,
  sign_lim   = 1000000
)

irf_signed_tight <- irf(run_bvar_updated, spec = irf_spec)





# 1. Reset matrix to clean NAs
sign_restr_final <- matrix(NA, nrow = 9, ncol = 9)
rownames(sign_restr_final) <- var_order
colnames(sign_restr_final) <- paste0("shock_", 1:9)

# 2. Impose the 3 core restrictions on Shock 5
sign_restr_final[policy_idx,          policy_idx] <-  1   # policy_rate (+)
sign_restr_final[cpi_idx,             policy_idx] <- -1   # cpi_growth (-)
sign_restr_final[interest_burden_idx, policy_idx] <-  1   # interest_burden (+)

# 3. Define IRF Spec
irf_spec <- bv_irf(
  horizon    = 20,
  sign_restr = sign_restr_final,
  sign_lim   = 1000000
)

# 4. Run IRF
irf_signed_tight <- irf(run_bvar_updated, spec = irf_spec)





















# 1. Pull exact column sequence from your fitted BVAR object
ts_names <- colnames(run_bvar_updated$meta$y)

# 2. Extract numeric indices using the model's own data names
policy_idx          <- which(ts_names == "policy_rate")          # 5
cpi_idx             <- which(ts_names == "cpi_growth")           # 3
interest_burden_idx <- which(ts_names == "interest_burden")      # 6
debt_idx            <- which(ts_names == "debt_growth")          # 7

# 3. Create clean numeric matrix WITHOUT attaching rownames/colnames
K <- length(ts_names)
sign_restr_final <- matrix(NA, nrow = K, ncol = K)

# 4. Assign signs by raw integer positions
sign_restr_final[policy_idx,          policy_idx] <-  1
sign_restr_final[cpi_idx,             policy_idx] <- -1
sign_restr_final[interest_burden_idx, policy_idx] <-  1
sign_restr_final[debt_idx,            policy_idx] <- -1

# 5. Run IRF setup
irf_spec <- bv_irf(
  horizon    = 20,
  sign_restr = sign_restr_final,
  sign_lim   = 1000000
)

irf_signed_tight <- irf(run_bvar_updated, spec = irf_spec)