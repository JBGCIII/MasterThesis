###############################################################################
################################# 2. BVAR BASE ################################
###############################################################################

# This is a standard cholensky variable with fewer variables than the full
# model (5 compared to 9). The purpouse is to have a standard cholensky model
# that acts as a sanity check by providing insight on how a standard Riksbank 
# rate hike impacts consumption, household debt, and housing market growth 
# without complex sign restrictions.
# cpi_growth --> consumption_growth --> policy_rate --> debt_growth
# --> real_house_price_growth
# This will also, hopefully, allow to run a FEVD and historical composition
# without crashing, giving insigh on what drove Swedish Consumption Swings
# historically as well as show whether one gains structural insight by
# adding the remaining variables.


# Load Seasonally and Outlier Adjusted Data.
bvar_data <- read.csv(
  "1_Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)

# Create Output Directories
dir.create("3_Model_Output", 
 recursive = TRUE, showWarnings = FALSE)

# Create Output Directories
dir.create("3_Model_Output/Baseline_5Var",
 recursive = TRUE, showWarnings = FALSE)

dir.create("3_Model_Output/Baseline_5Var/Model_Diagnostics", 
 recursive = TRUE, showWarnings = FALSE)



cat("\n==================== [1] Variable Index Mapping ====================\n")

target_cols_5var <- c(
  "cpi_growth",
  "consumption_growth",
  "policy_rate",
  "debt_growth",
  "real_house_price_growth"
)

bvar_matrix_5var <- as.matrix(bvar_data[, target_cols_5var])
N_5var <- ncol(bvar_matrix_5var)
H_total <- 20


# Create explicit named indices
cpi_idx        <- which(target_cols_5var == "cpi_growth")
cons_idx       <- which(target_cols_5var == "consumption_growth")
policy_idx     <- which(target_cols_5var == "policy_rate")
debt_idx       <- which(target_cols_5var == "debt_growth")
house_idx      <- which(target_cols_5var == "real_house_price_growth")

cat("\n==================[2] Cholesky Identification Matrix===============\n")

# To satisfy bsvarSIGNs C++ sampler limits:
# Shock j (Column j) can have AT MOST (N - j) zero restrictions on impact (h = 1)

sign_cholesky <- array(NA, dim = c(N_5var, N_5var, H_total))

# Column 1 (CPI Shock): Max 4 zeros allowed -> Rows 2, 3, 4, 5 restricted to 0
sign_cholesky[cons_idx,   1, 1] <- 0
sign_cholesky[policy_idx, 1, 1] <- 0
sign_cholesky[debt_idx,   1, 1] <- 0
sign_cholesky[house_idx,  1, 1] <- 0

# Column 2 (Consumption Shock): Max 3 zeros allowed -> Rows 3, 4, 5 restricted to 0
sign_cholesky[policy_idx, 2, 1] <- 0
sign_cholesky[debt_idx,   2, 1] <- 0
sign_cholesky[house_idx,  2, 1] <- 0

# Column 3 (Policy Rate Shock): Max 2 zeros allowed -> Rows 4, 5 restricted to 0
sign_cholesky[debt_idx,   3, 1] <- 0
sign_cholesky[house_idx,  3, 1] <- 0

# Column 4 (Debt Growth Shock): Max 1 zero allowed -> Row 5 restricted to 0
sign_cholesky[house_idx,  4, 1] <- 0

# Column 5 (House Price Shock): 0 zeros allowed (fully unconstrained impact)


cat("\n====================== [3] Prior Settings =========================\n")

is_random_walk_5var <- c(
  FALSE, # cpi_growth
  FALSE, # consumption_growth
  TRUE,  # policy_rate
  TRUE, # debt_growth
  FALSE  # real_house_price_growth
)

p       <- 4 # Lag order

raw_covid_idx <- which(bvar_data$quarter == "2020K2") # 97
effective_covid_idx <- raw_covid_idx - 4              # 93

# 1. Specify Model
spec_baseline <- specify_bsvarSIGN$new(
  data         = bvar_matrix_5var,
  p            = p,
  sign_irf     = sign_cholesky,
  stationary   = is_random_walk_5var,
  hyper_covid  = effective_covid_idx,  # Pass shifted index 91
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,
  hyper_delta  = TRUE,
  hyper_psi    = TRUE,
  mc.cores     = 1L
)

cat("\n====================== [4] Model Estimation =======================\n")

# 2. Fit Model 
# (Reduced S to 2,000 to keep the C++ HD array allocation within safe memory limits)
set.seed(321)
spec_baseline$estimate_hyper(S = 2000, burn_in = 1000)
fit_baseline <- estimate(spec_baseline, S = 1000, thin = 5)

# Save Model Object
saveRDS(fit_baseline, "3_Model_Output/Baseline_5Var/baseline_model_fit.rds")


cat("\n====================== [5] Model Diagnostics =======================\n")


# Alpha & Lambda Plot
png(
"3_Model_Output/Baseline_5Var/Model_Diagnostics/Alpha_Lambda_Traces.png",
 width  = 3200, 
 height = 2600, 
 res    = 300
)

# Plot trace plots side-by-side
par(mfrow = c(1, 2))
plot(fit_baseline$posterior$hyper[11, ], type = "l", 
 main = "Trace Plot: Alpha", ylab = "alpha", xlab = "Draw")
plot(fit_baseline$posterior$hyper[10, ], 
 type = "l", main = "Trace Plot: Lambda", ylab = "lambda", xlab = "Draw")
par(mfrow = c(1, 1))

dev.off()


#------------------------------------------------------------------------------#
# Alpha & Lambda Density Plot

png(
"3_Model_Output/Baseline_5Var/Model_Diagnostics/Alpha_Lambda_Density.png",
 width  = 3200, 
 height = 2600, 
 res    = 300
)

par(mfrow = c(1, 2))
plot(
  density(fit_baseline$posterior$hyper[11, ]), 
  main = "Posterior Density of Alpha", 
  xlab = "alpha", 
  col = "#000000",
  lwd = 2
)
plot(
  density(fit_baseline$posterior$hyper[10, ]), 
  main = "Posterior Density of Lambda", 
  xlab = "lambda", 
  col = "#000000",
  lwd = 2
)
dev.off()

#------------------------------------------------------------------------------#
# Alpha & Lambda ACF

png(
"3_Model_Output/Baseline_5Var/Model_Diagnostics/Alpha_Lambda_ACF.png",
 width  = 3200, 
 height = 2600, 
 res    = 300
)
# Plot ACF plots side-by-side
par(mfrow = c(1, 2))
acf(fit_baseline$posterior$hyper[10, ],
 main = "ACF - Alpha")
acf(fit_baseline$posterior$hyper[11, ],
 main = "ACF - Lambda")
par(mfrow = c(1, 1))
dev.off()


#------------------------------------------------------------------------------#
# Model Hyperparamaters

export_hyperparameters(
  estimation_obj = fit_baseline,
  target_cols    = target_cols_5var,
  file_path      = "3_Model_Output/Baseline_5Var/Model_Diagnostics/hyperparameter_summary.csv"

)

#------------------------------------------------------------------------------#
# Model Effective Sample Size

ess_results <- compute_and_export_ess(
  model_obj = fit_baseline,
  file_path = "3_Model_Output/Baseline_5Var/Model_Diagnostics/ESS_Diagnostics_Model_01.csv"
  )


cat("\n======================= [6] Impulse Response ===========================\n")

# Note: I attempted to use the plot function within the bsvarSIGNs package, as denoted
# In http://127.0.0.1:19135/library/bsvars/html/plot.PosteriorIR.html, but it denotes
# all columns by the name of the first variables in the vector. Most likely due to
# an internal bug with dealing with 3D arrays instead of 2D one's
#plot(
#  irf_output,
#  probability = 0.68,                      # 68% credible interval
#  shock_names = target_cols_5var,          # Fixes top column headers ("shock 1" -> actual names)
#  col         = "steelblue"                # Replaces the default pink (#ff69b4)
#)


#------------------------------------------------------------------------------#

irf_output <- compute_impulse_responses(fit_stable, horizon = 20)

# irf_output structure: [Response_Var, Shock_Var, Horizon, Draw]
# Extract consumption_growth across DIMENSION 1 (Response_Var)
irf_cons_slice <- irf_output[cons_idx, , , ] # Matrix shape: [N_shocks x Horizon x Draws]

N <- length(target_cols_5var)
H <- dim(irf_output)[3]

irf_list <- list()

for (j in 1:N) {
  shock_name <- target_cols_5var[j]
  shock_mat  <- irf_cons_slice[j, , ] # [Horizon x Draws]
  
  # Quantiles across stable draws
  irf_list[[j]] <- tibble(
    horizon          = 1:H,
    structural_shock = shock_name,
    median           = apply(shock_mat, 1, median),
    low_68           = apply(shock_mat, 1, quantile, probs = 0.16),
    high_68          = apply(shock_mat, 1, quantile, probs = 0.84),
    low_95           = apply(shock_mat, 1, quantile, probs = 0.025),
    high_95          = apply(shock_mat, 1, quantile, probs = 0.975)
  )
}

irf_df <- bind_rows(irf_list)
irf_df$structural_shock <- factor(irf_df$structural_shock, levels = target_cols_5var)

#------------------------------------------------------------------------------#
# 2. Render Corrected IRF Panel Plot
plot_irf_consumption <- ggplot(irf_df, aes(x = horizon)) +
  geom_ribbon(aes(ymin = low_95, ymax = high_95), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = low_68, ymax = high_68), fill = "steelblue", alpha = 0.30) +
  geom_line(aes(y = median), color = "darkblue", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  facet_wrap(~ structural_shock, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(0, 20, by = 5)) +
  labs(
    title    = "Impulse Response Functions: Response of consumption_growth",
    subtitle = "68% & 95% Posterior Credible Intervals (Corrected Dimension Slicing)",
    x        = "Horizon (Quarters)",
    y        = "Percentage Points"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text       = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", size = 13)
  )

print(plot_irf_consumption)


cat("\n============================ [7] FEVD =================================\n")


# 1.Compute FEVD array
fevd_output <- compute_variance_decompositions(fit_stable, horizon = 20)

# 2. Extract FEVD for stationary draws
# bsvarSIGNs typical shape: [N_vars, N_shocks, Horizon, Draws]
# Average across stable posterior draws first
fevd_draw_mean <- apply(fevd_output, c(1, 2, 3), mean) # [N_vars, N_shocks, Horizon]

# 3. Extract target variable (consumption_growth) matrix across all horizons
# Matrix shape: [N_shocks x Horizon]
fevd_target_raw <- fevd_draw_mean[cons_idx, , ] 

# 4. Normalize by column sums so every horizon sums to 100%
fevd_target_pct <- sweep(fevd_target_raw, 2, colSums(fevd_target_raw), "/") * 100
rownames(fevd_target_pct) <- target_cols_5var

# Extract Horizon 20 slice specifically
fevd_h20 <- fevd_target_pct[, 20]

cat("\n==================================================\n")
cat("FEVD: CONSUMPTION GROWTH (% Variance Share at H=20)\n")
cat("==================================================\n")
print(round(fevd_h20, 2))
cat("Total Sum Check:", sum(fevd_h20), "%\n")


# Convert raw matrix to standard data frame
fevd_df <- as.data.frame(t(fevd_target_pct))

# Set horizon explicitly as an integer vector
fevd_df$horizon <- 1:ncol(fevd_target_pct)

# Pivot long for stacked area presentation
fevd_long <- fevd_df %>%
  pivot_longer(
    cols      = all_of(target_cols_5var),
    names_to  = "structural_shock",
    values_to = "variance_share"
  )

# Plot FEVD horizon dynamics
plot_fevd_consumption <- ggplot(fevd_long, aes(x = horizon, y = variance_share, fill = structural_shock)) +
  geom_area(alpha = 0.85, colour = "black", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1, 20, by = 2)) +
  labs(
    title    = "Forecast Error Variance Decomposition: consumption_growth",
    subtitle = "Stationary Posterior Draws (H = 1 to 20 Quarters)",
    x        = "Forecast Horizon (Quarters)",
    y        = "Variance Share (%)",
    fill     = "Structural Shock"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 14)
  )

print(plot_fevd_consumption)

# Save FEVD plot
ggsave(
  filename = "3_Model_Output/Baseline_5Var/Figure_FEVD_consumption_growth.png",
  plot     = plot_fevd_consumption,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)


cat("\n================== [8] Historical Decomposition =======================\n")

# 1. Stationarity Filter (Eigenvalue Modulus Check)
A_draws <- fit_baseline$posterior$A
S_draws <- dim(A_draws)[3]
N       <- dim(fit_baseline$posterior$B)[1]

is_stable <- vector("logical", S_draws)

for (s in 1:S_draws) {
  A_mat <- A_draws[, 1:(N * p), s]
  
  companion_matrix <- matrix(0, nrow = N * p, ncol = N * p)
  companion_matrix[1:N, ] <- A_mat
  if (p > 1) {
    companion_matrix[(N + 1):(N * p), 1:(N * (p - 1))] <- diag(N * (p - 1))
  }
  
  is_stable[s] <- max(abs(eigen(companion_matrix)$values)) < 1.0
}

stable_indices <- which(is_stable)
cat(sprintf("\n--- STATIONARITY DIAGNOSTIC ---\nRetained %d stable draws out of %d total draws (%.1f%%)\n\n", 
            length(stable_indices), S_draws, 100 * mean(is_stable)))

# Create thinned fit object on stationary draws
select_draws <- head(stable_indices, min(200, length(stable_indices)))

fit_stable <- fit_baseline
fit_stable$posterior$B      <- fit_baseline$posterior$B[, , select_draws]
fit_stable$posterior$A      <- fit_baseline$posterior$A[, , select_draws]
fit_stable$posterior$Theta0 <- fit_baseline$posterior$Theta0[, , select_draws]

gc()

#------------------------------------------------------------------------------#
# 2. Crash-Proof Historical Decomposition (Native R Engine)

# Run custom safe HD function on fit_stable
hd_safe_output <- compute_hd_bsvarSIGN_safe(fit_stable)

# Calculate posterior mean across stable draws: shape (N_vars, N_shocks, T_periods)
hd_mean <- apply(hd_safe_output, c(1, 2, 3), mean)

# Extract consumption growth HD matrix
target_var_idx <- cons_idx 
hd_target      <- hd_mean[target_var_idx, , ] 

# Format Tidy Dataframe
hd_df           <- as.data.frame(t(hd_target))
colnames(hd_df) <- target_cols_5var
hd_df$quarter   <- bvar_data$quarter[(p + 1):nrow(bvar_data)]
hd_df$time      <- 1:nrow(hd_df)

hd_long <- hd_df %>%
  pivot_longer(
    cols      = all_of(target_cols_5var),
    names_to  = "structural_shock",
    values_to = "contribution"
  )

# Plot Historical Decomposition
plot_hd_consumption <- ggplot(hd_long, aes(x = time, y = contribution, fill = structural_shock)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Historical Decomposition: consumption_growth",
    subtitle = "Stationary Posterior Draws (Crash-Free Native R Engine)",
    x        = "Time Period",
    y        = "Contribution",
    fill     = "Structural Shock"
  ) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 14)
  )

print(plot_hd_consumption)

ggsave(
  filename = "3_Model_Output/Baseline_5Var/Figure_HD_consumption_growth_stationary.png",
  plot     = plot_hd_consumption,
  width    = 10,
  height   = 6,
  dpi      = 300
)


