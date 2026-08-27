
library(tidyverse)
# Ensure custom project functions and bsvarSIGNs are loaded

# ==============================================================================
# [1] Setup Directories & Load Data
# ==============================================================================
out_dir <- "3_Model_Output/Baseline_5Var/Model_Diagnostics"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dir.create("4_Output_Analysis/Baseline_5Var", recursive = TRUE, showWarnings = FALSE)

bvar_data <- read.csv("1_Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")

target_cols_5var <- c(
  "cpi_growth",
  "consumption_growth",
  "policy_rate",
  "debt_growth",
  "real_house_price_growth"
)

bvar_matrix_5var <- as.matrix(bvar_data[, target_cols_5var])
N_5var   <- ncol(bvar_matrix_5var)
cons_idx <- which(target_cols_5var == "consumption_growth")
H_total  <- 20
p        <- 4

# ==============================================================================
# [2] Identification & Model Specification
# ==============================================================================
# Initialize 3D array
sign_cholesky <- array(NA, dim = c(N_5var, N_5var, H_total))

# Target ONLY the 2D slice for Horizon 1
mat_h1 <- matrix(NA, nrow = N_5var, ncol = N_5var)
mat_h1[lower.tri(mat_h1)] <- 0

# Assign back to horizon 1
sign_cholesky[,, 1] <- mat_h1


# 2. Verify: Ensure impact matrix matches target variable ordering
dimnames(sign_cholesky) <- list(Response = target_cols_5var, Shock = target_cols_5var, Horizon = 1:H_total)


is_random_walk_5var <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
effective_covid_idx <- which(bvar_data$quarter == "2020K2") - p

spec_baseline <- specify_bsvarSIGN$new(
  data         = bvar_matrix_5var,
  p            = p,
  sign_irf     = sign_cholesky,
  stationary   = is_random_walk_5var,
  hyper_covid  = effective_covid_idx,
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,
  hyper_delta  = TRUE,
  hyper_psi    = TRUE,
  mc.cores     = 1L
)









# ==============================================================================
# [3] Model Estimation & Stationarity Filter
# ==============================================================================
set.seed(321)

spec_baseline$estimate_hyper(S = 3000, burn_in = 1000)
fit_baseline <- estimate(spec_baseline, S = 1000, thin = 5)

saveRDS(fit_baseline, "3_Model_Output/Baseline_5Var/baseline_model_fit.rds")

# Stationarity Filter: Moved UPSTREAM to define fit_stable before IRF/FEVD
A_draws <- fit_baseline$posterior$A
S_draws <- dim(A_draws)[3]

is_stable <- map_lgl(1:S_draws, function(s) {
  A_mat <- A_draws[, 1:(N_5var * p), s]
  comp_mat <- matrix(0, nrow = N_5var * p, ncol = N_5var * p)
  comp_mat[1:N_5var, ] <- A_mat
  if (p > 1) comp_mat[(N_5var + 1):(N_5var * p), 1:(N_5var * (p - 1))] <- diag(N_5var * (p - 1))
  max(abs(eigen(comp_mat)$values)) < 1.0
})

stable_indices <- which(is_stable)
cat(sprintf("\nRetained %d stable draws out of %d total (%.1f%%)\n", 
            length(stable_indices), S_draws, 100 * mean(is_stable)))

select_draws <- head(stable_indices, min(200, length(stable_indices)))

fit_stable <- fit_baseline
fit_stable$posterior$B      <- fit_baseline$posterior$B[, , select_draws]
fit_stable$posterior$A      <- fit_baseline$posterior$A[, , select_draws]
fit_stable$posterior$Theta0 <- fit_baseline$posterior$Theta0[, , select_draws]

# ==============================================================================
# [4] Model Diagnostics
# ==============================================================================
# Helper to cleanly save diagnostic panels
save_diag_plot <- function(file_name, expr) {
  png(file.path(out_dir, file_name), width = 3200, height = 2600, res = 300)
  par(mfrow = c(1, 2))
  expr()
  dev.off()
}

save_diag_plot("Alpha_Lambda_Traces.png", function() {
  plot(fit_baseline$posterior$hyper[11, ], type = "l", main = "Trace Plot: Alpha", ylab = "alpha", xlab = "Draw")
  plot(fit_baseline$posterior$hyper[10, ], type = "l", main = "Trace Plot: Lambda", ylab = "lambda", xlab = "Draw")
})

save_diag_plot("Alpha_Lambda_Density.png", function() {
  plot(density(fit_baseline$posterior$hyper[11, ]), main = "Posterior Density: Alpha", xlab = "alpha", lwd = 2)
  plot(density(fit_baseline$posterior$hyper[10, ]), main = "Posterior Density: Lambda", xlab = "lambda", lwd = 2)
})

save_diag_plot("Alpha_Lambda_ACF.png", function() {
  acf(fit_baseline$posterior$hyper[10, ], main = "ACF - Alpha")
  acf(fit_baseline$posterior$hyper[11, ], main = "ACF - Lambda")
})

export_hyperparameters(
  estimation_obj = fit_baseline,
  target_cols    = target_cols_5var,
  file_path      = file.path(out_dir, "hyperparameter_summary.csv")
)

compute_and_export_ess(
  model_obj = fit_baseline,
  file_path = file.path(out_dir, "ESS_Diagnostics_Model_01.csv")
)

# ==============================================================================
# [5] Impulse Responses
# ==============================================================================
irf_output     <- compute_impulse_responses(fit_stable, horizon = H_total)
irf_cons_slice <- irf_output[cons_idx, , , ] # [N_shocks x Horizon x Draws]

irf_list <- lapply(1:N_5var, function(j) {
  shock_mat <- irf_cons_slice[j, , ] # [Horizon x Draws]
  
  # Number of horizon periods (21 periods: 0 to 20)
  n_horizons <- nrow(shock_mat) 
  
  tibble(
    horizon          = 0:(n_horizons - 1), # Quarters 0 to 20
    structural_shock = target_cols_5var[j],
    median           = apply(shock_mat, 1, median),
    low_68           = apply(shock_mat, 1, quantile, probs = 0.16),
    high_68          = apply(shock_mat, 1, quantile, probs = 0.84),
    low_95           = apply(shock_mat, 1, quantile, probs = 0.025),
    high_95          = apply(shock_mat, 1, quantile, probs = 0.975)
  )
})

irf_df <- bind_rows(irf_list) %>% 
  mutate(structural_shock = factor(structural_shock, levels = target_cols_5var))

plot_irf_consumption <- ggplot(irf_df, aes(x = horizon)) +
  geom_ribbon(aes(ymin = low_95, ymax = high_95), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = low_68, ymax = high_68), fill = "steelblue", alpha = 0.30) +
  geom_line(aes(y = median), color = "darkblue", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  facet_wrap(~ structural_shock, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(0, 20, by = 5)) +
  labs(
    title    = "Impulse Response Functions: Response of consumption_growth",
    subtitle = "68% & 95% Posterior Credible Intervals",
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


ggsave("4_Output_Analysis/Baseline_5Var/Figure_IRF_consumption_growth.png", 
       plot = plot_irf_consumption, width = 9, height = 5.5, dpi = 300)

# ==============================================================================
# [6] Forecast Error Variance Decomposition (FEVD)
# ==============================================================================
fevd_output     <- compute_variance_decompositions(fit_stable, horizon = H_total)
fevd_draw_mean  <- apply(fevd_output, c(1, 2, 3), mean)
fevd_target_raw <- fevd_draw_mean[cons_idx, , ] 

# Normalize shares to percentage
fevd_target_pct <- sweep(fevd_target_raw, 2, colSums(fevd_target_raw), "/") * 100
rownames(fevd_target_pct) <- target_cols_5var

cat("\nFEVD: CONSUMPTION GROWTH (% Variance Share at H=20)\n")
print(round(fevd_target_pct[, 20], 2))

fevd_long <- as.data.frame(t(fevd_target_pct)) %>%
  mutate(horizon = row_number()) %>%
  pivot_longer(
    cols      = all_of(target_cols_5var),
    names_to  = "structural_shock",
    values_to = "variance_share"
  )

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
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

print(plot_fevd_consumption)

ggsave("4_Output_Analysis/Baseline_5Var/Figure_FEVD_consumption_growth.png", 
       plot = plot_fevd_consumption, width = 9, height = 5.5, dpi = 300)

# ==============================================================================
# [7] Historical Decomposition (HD)
# ==============================================================================
hd_safe_output <- compute_hd_bsvarSIGN_safe(fit_stable)
hd_mean        <- apply(hd_safe_output, c(1, 2, 3), mean)

hd_long <- as.data.frame(t(hd_mean[cons_idx, , ])) %>%
  setNames(target_cols_5var) %>%
  mutate(
    quarter = bvar_data$quarter[(p + 1):nrow(bvar_data)],
    time    = row_number()
  ) %>%
  pivot_longer(
    cols      = all_of(target_cols_5var),
    names_to  = "structural_shock",
    values_to = "contribution"
  )


# Extract actual data for the same period
actual_cons <- bvar_data$consumption_growth[(p + 1):nrow(bvar_data)]

hd_long <- hd_df %>%
  pivot_longer(
    cols      = all_of(target_cols_5var),
    names_to  = "structural_shock",
    values_to = "contribution"
  )

# Plot HD with Actual Data overlay line
plot_hd_consumption <- ggplot() +
  geom_bar(data = hd_long, aes(x = time, y = contribution, fill = structural_shock), 
           stat = "identity", position = "stack", width = 0.8) +
  geom_line(data = data.frame(time = 1:length(actual_cons), actual = actual_cons), 
            aes(x = time, y = actual), color = "black", linewidth = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Historical Decomposition: consumption_growth",
    subtitle = "Black line = Actual Data; Stacked Bars = Shock Contributions",
    x        = "Time Period",
    y        = "Percentage Points",
    fill     = "Structural Shock"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(plot_hd_consumption)

ggsave("4_Output_Analysis/Baseline_5Var/Figure_HD_consumption_growth_stationary.png", 
       plot = plot_hd_consumption, width = 10, height = 6, dpi = 300)



# 2. Extract consumption components (Target variable index)
hd_cons_shocks <- as.data.frame(t(hd_result$hd_mean[cons_idx, , ]))
colnames(hd_cons_shocks) <- target_cols_5var

# Add Initial Conditions Baseline & Identifiers
hd_cons_shocks$Initial_Conditions <- hd_result$base_component[cons_idx, ]
hd_cons_shocks$time               <- 1:nrow(hd_cons_shocks)
hd_cons_shocks$actual             <- hd_result$actual_data[cons_idx, ]

# 3. Pivot long for ggplot
hd_long <- hd_cons_shocks %>%
  pivot_longer(
    cols      = c(all_of(target_cols_5var), "Initial_Conditions"),
    names_to  = "component",
    values_to = "contribution"
  )

# 4. Plot (Bars will sum EXACTLY to the actual data line)
plot_hd_consumption_initial_condition <- ggplot(hd_long, aes(x = time)) +
  geom_bar(aes(y = contribution, fill = component), stat = "identity", width = 0.8) +
  geom_line(aes(y = actual), color = "black", linewidth = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "Historical Decomposition: consumption_growth",
    subtitle = "Black Line = Realized Consumption Growth | Stacked Bars = Shocks + Initial Conditions",
    x        = "Time Period",
    y        = "Percentage Points",
    fill     = "Structural Component"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


print(plot_hd_consumption_initial_condition)

ggsave("4_Output_Analysis/Baseline_5Var/Figure_HD_consumption_growth_stationary_initial_condition.png", 
       plot = plot_hd_consumption_initial_condition, width = 10, height = 6, dpi = 300)

  #The fact that the structural shocks does not blow up to 10% during COVID is proof 
  # that your outlier correction is doing its job. 
  # It prevents the 2020 pandemic from distorting the identification of normal structural shocks.
  # However, it still leaves much left to be explained.