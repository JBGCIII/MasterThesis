
library(tidyverse)
library(bsvarSIGNs)



# ==============================================================================
# SCRIPT: 03_bvar_sign_postprocessing.R
# PURPOSE: Eigenvalue filtering, Crash-Proof HD, and Normalized FEVD
# ==============================================================================

library(tidyverse)
library(bsvarSIGNs)

# 1. Source custom crash-proof HD function
source("2_Scripts/01_helper_functions.R")

# 2. Load data and setup variables
bvar_data <- read.csv("1_Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv")

target_cols_9var <- c(
  "gdp_growth", "consumption_growth", "cpi_growth", "saving_rate",
  "policy_rate", "interest_burden", "debt_growth", "real_house_price_growth",
  "asset_liability_ratio"
)

# Ensure output directories exist
models <- c("Model_01", "Model_02", "Model_03", "Model_04")
for (m in models) {
  dir.create(file.path("3_Model_Output/BVAR_Sign", m, "Plots_HD"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("3_Model_Output/BVAR_Sign", m, "Plots_FEVD"), recursive = TRUE, showWarnings = FALSE)
}

# Variable metadata setup
target_cols_9var <- c(
  "gdp_growth", "consumption_growth", "cpi_growth", "saving_rate",
  "policy_rate", "interest_burden", "debt_growth", "real_house_price_growth",
  "asset_liability_ratio"
)
N <- length(target_cols_9var)
p <- 4
H_total <- 20
effective_covid_idx <- which(bvar_data$quarter == "2020K2") - p

# Model list mapping
model_files <- list(
  list(id = "Model_01", rds = "run_bsvar_sign_model_1_20000.rds", label = "Policy Rate Shock"),
  list(id = "Model_02", rds = "run_bsvar_sign_model_2_20000.rds", label = "Credit Supply Shock"),
  list(id = "Model_03", rds = "run_bsvar_sign_model_3_20000.rds", label = "Housing Demand Shock"),
  list(id = "Model_04", rds = "run_bsvar_sign_model_4_20000.rds", label = "Wealth Shock")
)

# ==============================================================================
# MAIN BATCH LOOP ACROSS ALL 4 MODELS
# ==============================================================================
for (m_info in model_files) {
  
  m_id   <- m_info$id
  m_path <- file.path("3_Model_Output/BVAR_Sign", m_id, m_info$rds)
  
  if (!file.exists(m_path)) {
    cat(sprintf("\n[WARNING] Model file missing: %s. Skipping...\n", m_path))
    next
  }
  
  cat(sprintf("\n==================================================\n"))
  cat(sprintf("   PROCESSING: %s (%s)\n", m_id, m_info$label))
  cat(sprintf("==================================================\n"))
  
  # 1. Load Model Object
  fit_baseline <- readRDS(m_path)
  
  # ----------------------------------------------------------------------------
  # STEP A: Stationarity Eigenvalue Filter
  # ----------------------------------------------------------------------------
  A_draws <- fit_baseline$posterior$A
  S_draws <- dim(A_draws)[3]
  
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
  cat(sprintf("Retained %d stable draws out of %d total draws (%.1f%%)\n", 
              length(stable_indices), S_draws, 100 * mean(is_stable)))
  
  # Create thinned fit_stable object (cap at 200 draws for RAM safety)
  select_draws <- head(stable_indices, min(200, length(stable_indices)))
  
  fit_stable <- fit_baseline
  fit_stable$posterior$B      <- fit_baseline$posterior$B[, , select_draws]
  fit_stable$posterior$A      <- fit_baseline$posterior$A[, , select_draws]
  fit_stable$posterior$Theta0 <- fit_baseline$posterior$Theta0[, , select_draws]
  
  # ----------------------------------------------------------------------------
  # STEP B: Crash-Proof Native R Historical Decomposition
  # ----------------------------------------------------------------------------
  cat("Computing crash-proof Historical Decomposition...\n")
  hd_safe_output <- compute_hd_bsvarSIGN_safe(fit_stable)
  
  # Average across stationary draws: [N_vars, N_shocks, T_periods]
  hd_mean <- apply(hd_safe_output, c(1, 2, 3), mean)
  
  # Generate and save HD plots for ALL 9 variables
  for (v_idx in 1:N) {
    var_name <- target_cols_9var[v_idx]
    
    hd_target       <- hd_mean[v_idx, , ] 
    hd_df           <- as.data.frame(t(hd_target))
    colnames(hd_df) <- target_cols_9var
    hd_df$quarter   <- bvar_data$quarter[(p + 1):nrow(bvar_data)]
    hd_df$time      <- 1:nrow(hd_df)
    
    hd_long <- hd_df %>%
      pivot_longer(
        cols      = all_of(target_cols_9var),
        names_to  = "structural_shock",
        values_to = "contribution"
      )
    
    plot_hd <- ggplot(hd_long, aes(x = time, y = contribution, fill = structural_shock)) +
      geom_bar(stat = "identity", position = "stack", width = 0.8) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title    = sprintf("Historical Decomposition (%s): %s", m_id, var_name),
        subtitle = sprintf("Model: %s | Filtered Stationary Posterior Draws", m_info$label),
        x        = "Time Period",
        y        = "Contribution",
        fill     = "Structural Shock"
      ) +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13)
      )
    
    ggsave(
      filename = file.path("3_Model_Output/BVAR_Sign", m_id, "Plots_HD", sprintf("HD_%s.png", var_name)),
      plot     = plot_hd,
      width    = 10,
      height   = 6,
      dpi      = 300
    )
  }
  
  # ----------------------------------------------------------------------------
  # STEP C: Forecast Error Variance Decomposition (FEVD) & Horizon Plots
  # ----------------------------------------------------------------------------
  cat("Computing FEVD and normalizing variance shares...\n")
  fevd_output <- compute_variance_decompositions(fit_stable, horizon = H_total)
  
  # Average FEVD across draws: [N_vars, N_shocks, Horizon]
  fevd_draw_mean <- apply(fevd_output, c(1, 2, 3), mean)
  
  # Process and save FEVD plots for ALL 9 variables
  for (v_idx in 1:N) {
    var_name <- target_cols_9var[v_idx]
    
    # Extract [N_shocks x Horizon] matrix for current variable
    fevd_var_raw <- fevd_draw_mean[v_idx, , ] 
    
    # Normalize by column sums so every horizon sums to 100%
    fevd_var_pct <- sweep(fevd_var_raw, 2, colSums(fevd_var_raw), "/") * 100
    rownames(fevd_var_pct) <- target_cols_9var
    
    # Build dataframe for horizon stacked area chart
    fevd_df <- as.data.frame(t(fevd_var_pct))
    fevd_df$horizon <- 1:H_total
    
    fevd_long <- fevd_df %>%
      pivot_longer(
        cols      = all_of(target_cols_9var),
        names_to  = "structural_shock",
        values_to = "variance_share"
      )
    
    plot_fevd <- ggplot(fevd_long, aes(x = horizon, y = variance_share, fill = structural_shock)) +
      geom_area(alpha = 0.85, colour = "black", linewidth = 0.2) +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title    = sprintf("FEVD (%s): %s", m_id, var_name),
        subtitle = sprintf("Model: %s | Horizon H = 1 to %d", m_info$label, H_total),
        x        = "Forecast Horizon (Quarters)",
        y        = "Variance Share (%)",
        fill     = "Structural Shock"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13)
      )
    
    ggsave(
      filename = file.path("3_Model_Output/BVAR_Sign", m_id, "Plots_FEVD", sprintf("FEVD_%s.png", var_name)),
      plot     = plot_fevd,
      width    = 9,
      height   = 5.5,
      dpi      = 300
    )
  }
  
  # Print Horizon 20 Summary Table for Consumption Growth
  cons_idx <- which(target_cols_9var == "consumption_growth")
  fevd_cons_raw <- fevd_draw_mean[cons_idx, , 20]
  fevd_cons_pct <- (fevd_cons_raw / sum(fevd_cons_raw)) * 100
  names(fevd_cons_pct) <- target_cols_9var
  
  cat(sprintf("\n--- %s: CONSUMPTION GROWTH FEVD AT HORIZON 20 ---\n", m_id))
  print(round(fevd_cons_pct, 2))
  
  # Clean up loop memory allocations
  rm(fit_baseline, fit_stable, hd_safe_output, fevd_output)
  gc()
}

cat("\n==================================================\n")
cat("   ALL 4 MODELS PROCESSED AND PLOTS EXPORTED SUCCESSFULLY   \n")
cat("==================================================\n")