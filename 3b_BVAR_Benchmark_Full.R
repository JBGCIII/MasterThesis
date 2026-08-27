
# ==============================================================================
# [1] Setup Directories & Load Data
# ==============================================================================
out_dir <- "3_Model_Output/Baseline_Full/Model_Diagnostics"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dir.create("4_Output_Analysis/Baseline_Full", recursive = TRUE, showWarnings = FALSE)

bvar_data <- read.csv("1_Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")


target_cols_full <- c(
  "gdp_growth", # Real Macro Economy
  "consumption_growth", # Demand
  "cpi_growth", # Inflation
  "saving_rate", # Household Balance Sheet Adjustment
  "policy_rate", # Riksbank Repo/Policy Rate
  "interest_burden", # Mortgage Interest-to-Income / Debt Service
  "debt_growth", # Mortgage Borrowing Aggregate
  "real_house_price_growth", # Real Estate Index
  "asset_liability_ratio" # Household Financial Leverage
)

bvar_matrix_full <- as.matrix(bvar_data[, target_cols_full])
N_Full   <- ncol(bvar_matrix_full)
cons_idx <- which(target_cols_full == "consumption_growth")
H_total  <- 20
p        <- 2

# ==============================================================================
# [2] Identification & Model Specification
# ==============================================================================

# Initialize 3D array
sign_cholesky <- array(NA, dim = c(N_Full, N_Full, H_total))
mat_h1 <- matrix(NA, nrow = N_Full, ncol = N_Full)

# Zero out the LOWER triangle (Below the diagonal)
mat_h1[lower.tri(mat_h1)] <- 0 

sign_cholesky[, , 1] <- mat_h1

dimnames(sign_cholesky) <- list(
  Response = target_cols_full, 
  Shock    = target_cols_full, 
  Horizon  = 1:H_total
)

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

effective_covid_idx <- which(bvar_data$quarter == "2020K2") - p
mc.cores = max(1L, parallel::detectCores() - 1L)

spec_baseline <- specify_bsvarSIGN$new(
  data         = bvar_matrix_full,
  p            = p, # Set to 2
  sign_irf     = sign_cholesky, # Correct upper-triangular 0 mask
  stationary   = is_random_walk,
  hyper_covid  = effective_covid_idx,
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,
  hyper_delta  = TRUE,
  hyper_psi    = TRUE,
  mc.cores     = mc.cores
)



# ==============================================================================
# [3] Model Estimation & Stationarity Filter
# ==============================================================================
set.seed(321)

spec_baseline$estimate_hyper(S = 3000, burn_in = 1000)
fit_baseline_full <- estimate(spec_baseline, S = 2000, thin = 5)


saveRDS(fit_baseline_full, "3_Model_Output/Baseline_Full/baseline_model_fit.rds")


# ==============================================================================
# Stationarity Filtering & Stable Posterior Extraction
# ==============================================================================

# 1. Load pristine model fit (1,000 draws)
fit_baseline_full <- readRDS("3_Model_Output/Baseline_Full/baseline_model_fit.rds")

# 2. Extract Posterior Arrays
A_draws <- fit_baseline_full$posterior$A # Dimensions: [9 x 37 x 1000]
S_draws <- dim(A_draws)[3]

# 3. Evaluate Spectral Radius for Each Draw
is_stable <- map_lgl(1:S_draws, function(s) {
  # Extract VAR lag parameters (columns 1..36), drop column 37 (intercept)
  A_mat    <- A_draws[, 1:(N_Full * p), s]
  comp_mat <- matrix(0, nrow = N_Full * p, ncol = N_Full * p)
  
  # Row block 1: Lag matrices A1...Ap
  comp_mat[1:N_Full, ] <- A_mat
  
  # Lower blocks: Identity matrix shift (36x36 total companion matrix)
  if (p > 1) {
    comp_mat[(N_Full + 1):(N_Full * p), 1:(N_Full * (p - 1))] <- diag(N_Full * (p - 1))
  }
  
  # Unit root check: max eigenvalue absolute magnitude < 1.0
  max(abs(eigen(comp_mat, only.values = TRUE)$values)) < 1.000
})

stable_indices <- which(is_stable)
cat(sprintf("\nRetained %d stable draws out of %d total (%.1f%%)\n", 
            length(stable_indices), S_draws, 100 * mean(is_stable)))

# 4. Safely Slice Stationary Draws into Subsampled Object
if (length(stable_indices) > 0) {
  set.seed(321)
  n_target     <- min(200L, length(stable_indices))
  select_draws <- sample(stable_indices, size = n_target)
  
  fit_stable <- fit_baseline_full
  fit_stable$posterior$B      <- fit_baseline_full$posterior$B[, , select_draws]
  fit_stable$posterior$A      <- fit_baseline_full$posterior$A[, , select_draws]
  fit_stable$posterior$Theta0 <- fit_baseline_full$posterior$Theta0[, , select_draws]
  
  if (!is.null(fit_baseline_full$posterior$hyper)) {
    fit_stable$posterior$hyper <- fit_baseline_full$posterior$hyper[, select_draws]
  }
} else {
  stop("No stable draws found! Check model specification or tightness of priors.")
}


# ==============================================================================
# [4] Model Diagnostics
# ==============================================================================

fit_baseline_full <- readRDS("3_Model_Output/Baseline_Full/baseline_model_fit.rds")

# Helper to cleanly save diagnostic panels
save_diag_plot <- function(file_name, expr) {
  png(file.path(out_dir, file_name), width = 3200, height = 2600, res = 300)
  par(mfrow = c(1, 2))
  expr()
  dev.off()
}

save_diag_plot("Alpha_Lambda_Traces.png", function() {
  plot(fit_baseline_full$posterior$hyper[11, ], type = "l", main = "Trace Plot: Alpha", ylab = "alpha", xlab = "Draw")
  plot(fit_baseline_full$posterior$hyper[10, ], type = "l", main = "Trace Plot: Lambda", ylab = "lambda", xlab = "Draw")
})

save_diag_plot("Alpha_Lambda_Density.png", function() {
  plot(density(fit_baseline_full$posterior$hyper[11, ]), main = "Posterior Density: Alpha", xlab = "alpha", lwd = 2)
  plot(density(fit_baseline_full$posterior$hyper[10, ]), main = "Posterior Density: Lambda", xlab = "lambda", lwd = 2)
})

save_diag_plot("Alpha_Lambda_ACF.png", function() {
  acf(fit_baseline_full$posterior$hyper[10, ], main = "ACF - Alpha")
  acf(fit_baseline_full$posterior$hyper[11, ], main = "ACF - Lambda")
})

export_hyperparameters(
  estimation_obj = fit_baseline_full,
  target_cols    = target_cols_full,
  file_path      = file.path(out_dir, "hyperparameter_summary.csv")
)

compute_and_export_ess(
  model_obj = fit_baseline_full,
  file_path = file.path(out_dir, "ESS_Diagnostics_Model_01.csv")
)



# ==============================================================================
# Dynamic 9-Variable Cholesky IRF Extraction & Sign Normalization
# ==============================================================================

cons_idx <- which(target_cols_full == "consumption_growth")
N_Full   <- length(target_cols_full)

# 1. Compute IRFs
irf_raw <- compute_impulse_responses(fit_stable, horizon = H_total)
irf_arr <- if (is.array(irf_raw)) irf_raw else irf_raw$posterior$irf

# Array shape confirmed: [9 x 9 x 21 x 200]
irf_list <- lapply(1:N_Full, function(j) {
  
  # Extract response of consumption (Dim 1 = cons_idx) to Shock j (Dim 2 = j)
  # Resulting matrix shape: [21 Horizons x 200 Draws]
  shock_mat <- irf_arr[cons_idx, j, , ] 
  
  # ----------------------------------------------------------------------------
  # Cholesky Identification Normalization:
  # Ensure Shock j is ALWAYS positive (+1) on ITS OWN variable at Horizon 0
  # ----------------------------------------------------------------------------
  own_impact <- irf_arr[j, j, 1, ] # Impact of Shock j on Variable j at h=0
  sign_flip  <- ifelse(own_impact < 0, -1, 1)
  
  # Align signs across all 200 draws
  shock_mat  <- sweep(shock_mat, 2, sign_flip, FUN = "*")
  # ----------------------------------------------------------------------------
  
  n_horizons <- nrow(shock_mat) 
  
  tibble(
    horizon          = 0:(n_horizons - 1),
    structural_shock = target_cols_full[j],
    median           = apply(shock_mat, 1, median),
    low_68           = apply(shock_mat, 1, quantile, probs = 0.16),
    high_68          = apply(shock_mat, 1, quantile, probs = 0.84),
    low_95           = apply(shock_mat, 1, quantile, probs = 0.025),
    high_95          = apply(shock_mat, 1, quantile, probs = 0.975)
  )
})

irf_df <- bind_rows(irf_list) %>% 
  mutate(structural_shock = factor(structural_shock, levels = target_cols_full))

# 2. Plotting
plot_irf_consumption <- ggplot(irf_df, aes(x = horizon)) +
  geom_ribbon(aes(ymin = low_95, ymax = high_95), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = low_68, ymax = high_68), fill = "steelblue", alpha = 0.30) +
  geom_line(aes(y = median), color = "darkblue", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  facet_wrap(~ structural_shock, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = seq(0, 20, by = 5)) +
  labs(
    title    = "Cholesky IRF: Response of consumption_growth",
    subtitle = "Normalized Positive Structural Shocks (68% & 95% Posterior Intervals)",
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

ggsave("4_Output_Analysis/Baseline_Full/Figure_IRF_consumption_growth.png", 
       plot = plot_irf_consumption, width = 9, height = 7.5, dpi = 300)


# ==============================================================================
# [6] Forecast Error Variance Decomposition (FEVD)
# ==============================================================================
# 1. Compute raw FEVD array: [Variable, Shock, Horizon, Draw]
fevd_raw <- compute_variance_decompositions(fit_stable, horizon = H_total)
fevd_arr <- if (is.array(fevd_raw)) fevd_raw else fevd_raw$posterior$fevd

# 2. Extract consumption growth responses across all shocks, horizons, and draws
# Dimensions: [Shock, Horizon, Draw]
fevd_cons <- fevd_arr[cons_idx, , , ] 

# 3. Normalize shares per draw so shocks sum to 1 (100%) at each horizon
fevd_cons_norm <- apply(fevd_cons, c(2, 3), function(x) x / sum(x)) 
# Dimensions are now: [Shock, Horizon, Draw]

# 4. Take posterior mean across draws
fevd_mean <- apply(fevd_cons_norm, c(1, 2), mean) * 100
rownames(fevd_mean) <- target_cols_full

# Print FEVD at Horizon 20
cat("\nFEVD: CONSUMPTION GROWTH (% Variance Share at H=20)\n")
print(round(fevd_mean[, 20], 2))

fevd_long <- as.data.frame(fevd_mean) %>%
  rownames_to_column(var = "structural_shock") %>%
  pivot_longer(
    cols      = -structural_shock,
    names_to  = "horizon",
    values_to = "variance_share"
  ) %>%
  mutate(
    # Strip non-numeric characters if column names are like "V1" or "H1"
    horizon          = as.numeric(gsub("[^0-9]", "", horizon)),
    structural_shock = factor(structural_shock, levels = target_cols_full)
  )


plot_fevd_consumption <- ggplot(fevd_long, aes(x = horizon, y = variance_share, fill = structural_shock)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.3) + # White borders look cleaner between stacked areas
  scale_fill_viridis_d(option = "turbo") +                      # Supports 9+ distinct, highly scannable colors
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 20, by = 2), expand = c(0, 0)) +
  labs(
    title    = "Forecast Error Variance Decomposition: consumption_growth",
    subtitle = "Stationary Posterior Draws (H = 1 to 20 Quarters)",
    x        = "Forecast Horizon (Quarters)",
    y        = "Variance Share (%)",
    fill     = "Structural Shock"
  ) +
  theme_bw() +
  theme(
    legend.position   = "bottom", 
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 14),
    panel.grid.minor  = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) # Wraps the 9-variable legend into 2 clean rows

print(plot_fevd_consumption)

# Saved to updated 9-variable directory
ggsave(
  filename = "4_Output_Analysis/Baseline_Full/Figure_FEVD_consumption_growth.png", 
  plot     = plot_fevd_consumption, 
  width    = 10, 
  height   = 6, 
  dpi      = 300
)


# ==============================================================================
# [7] Historical Decomposition (HD)
# ==============================================================================






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
colnames(hd_df) <- target_cols_full
hd_df$quarter   <- bvar_data$quarter[(p + 1):nrow(bvar_data)]
hd_df$time      <- 1:nrow(hd_df)








# ==============================================================================
# 1. Compute Posterior Mean Array from hd_result
# ==============================================================================
# hd_result shape: [N_vars, N_shocks, T_periods, S_draws]
# Calculate posterior mean across MCMC draws (dimension 4)
hd_mean <- apply(hd_result, c(1, 2, 3), mean)  # Shape: [N_vars, N_shocks, T_periods]

# Extract consumption growth HD matrix for all structural shocks
# hd_mean[cons_idx, , ] shape: [N_shocks, T_periods]
hd_cons_shocks <- as.data.frame(t(hd_mean[cons_idx, , ]))
colnames(hd_cons_shocks) <- target_cols_full

# ==============================================================================
# 2. Add Time Identifiers & Realized Actual Data
# ==============================================================================
hd_cons_shocks$quarter <- bvar_data$quarter[(p + 1):nrow(bvar_data)]
hd_cons_shocks$time    <- 1:nrow(hd_cons_shocks)

# Extract actual realized consumption data for comparison line
actual_cons <- bvar_data$consumption_growth[(p + 1):nrow(bvar_data)]
hd_cons_shocks$actual  <- actual_cons

# ==============================================================================
# 3. Pivot Long for ggplot2
# ==============================================================================
hd_long <- hd_cons_shocks %>%
  pivot_longer(
    cols      = all_of(target_cols_full),
    names_to  = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = factor(component, levels = target_cols_full)
  )













# ==============================================================================
# Corrected HD Extraction Script
# ==============================================================================
# 1. Compute posterior mean across draws (dimension 4)
# hd_result array dimensions: [Variable, Shock, Time, Draw]
hd_mean <- apply(hd_result, c(1, 2, 3), mean)

# 2. Extract consumption row (cons_idx = 2)
# Resulting matrix dimensions: [Shock (1:9), Time (1:T)]
hd_cons_matrix <- hd_mean[cons_idx, , ]

# 3. Transpose so rows = Time and columns = Shocks 1 to 9
hd_cons_df <- as.data.frame(t(hd_cons_matrix))

# Assign column names IN EXACT ORDER of target_cols_full
colnames(hd_cons_df) <- target_cols_full

# 4. Add Quarter dates and Actual Realized Data
hd_cons_df$quarter <- bvar_data$quarter[(p + 1):nrow(bvar_data)]
hd_cons_df$actual  <- bvar_data$consumption_growth[(p + 1):nrow(bvar_data)]

# 5. Convert to long format for ggplot2
hd_long <- hd_cons_df %>%
  pivot_longer(
    cols      = all_of(target_cols_full),
    names_to  = "structural_shock",
    values_to = "contribution"
  ) %>%
  mutate(
    structural_shock = factor(structural_shock, levels = target_cols_full)
  )

# ==============================================================================
# 4. Plot Historical Decomposition
# ==============================================================================
plot_hd_consumption_initial_condition <- ggplot(hd_long, aes(x = quarter)) +
  # Stacked bars for structural shock contributions
  geom_col(aes(y = contribution, fill = component), position = "stack", width = 0.8, color = NA) +
  # Black line for actual realized consumption growth
  geom_line(aes(y = actual, group = 1, color = "Realized Data"), linewidth = 0.8) +
  # Color scale supporting all 9 variables
  scale_fill_viridis_d(option = "turbo") +
  scale_color_manual(name = "", values = c("Realized Data" = "black")) +
  labs(
    title    = "Historical Decomposition: consumption_growth",
    subtitle = "Black Line = Realized Consumption Growth | Stacked Bars = Structural Shocks",
    x        = "Quarter",
    y        = "Percentage Points",
    fill     = "Structural Shock"
  ) +
  theme_bw() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, color = "grey30"),
    panel.grid.minor  = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  guides(
    fill  = guide_legend(nrow = 2, byrow = TRUE, order = 1),
    color = guide_legend(order = 2)
  )

print(plot_hd_consumption_initial_condition)


ggsave(
  filename = "4_Output_Analysis/Baseline_Full/Figure_HD_consumption_growth_with_initial_cond.png", 
  plot     = plot_hd_consumption_initial_condition, 
  width    = 16, 
  height   = 11, 
  dpi      = 300
)

