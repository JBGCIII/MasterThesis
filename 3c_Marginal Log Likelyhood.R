###############################################################################
############################# MARGINAL LIKELIHOOD #############################
###############################################################################

# ==============================================================================
# [1] Data Import and Time Series Matrix Construction
# ==============================================================================

# 1.1 Load seasonally adjusted macroeconomic dataset
bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")

# 1.2 Convert complete data frame into quarterly time series matrix starting at 1996Q1
model_ts <- ts(bvar_data, start = c(1996, 1), frequency = 4)

# ==============================================================================
# [2] Candidate BVAR Model Specifications
# ==============================================================================

# 2.1 Define sub-system matrix configurations across candidate model structures
specifications <- list(
  # Housing Block with Durable Consumption
  housing_durable  = as.matrix(model_ts[, c("unemployment_rate", "policy_rate", "house_price_real_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_durable_log")]),
  
  # Housing Block with Habitual Consumption
  housing_habitual = as.matrix(model_ts[, c("unemployment_rate", "policy_rate", "house_price_real_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_habitual_log")]),
  
  # Domestic Macro Block with Durable Consumption
  macro_durable    = as.matrix(model_ts[, c("gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")]),
  
  # Domestic Macro Block with Habitual Consumption
  macro_habitual   = as.matrix(model_ts[, c("gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_habitual_log")]),
  
  # Open-Economy Foreign Block with Durable Consumption
  foreign_durable  = as.matrix(model_ts[, c("fed_funds_rate", "kix_gdp_log", "kix_cpi", "gdp_se_log",
                                            "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log", 
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")]),
  
  # Open-Economy Foreign Block with Habitual Consumption
  foreign_habitual = as.matrix(model_ts[, c("fed_funds_rate", "kix_gdp_log", "kix_cpi", "gdp_se_log",
                                            "unemployment_rate", "cpi_log", "policy_rate", "cons_habitual_log", 
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")])
)

# ==============================================================================
# [3] Bayesian VAR Prior Setup
# ==============================================================================

# 3.1 Configure Minnesota prior specification with hierarchical hyperparameter optimization
setup <- bv_priors(
  hyper = "auto",
  mn = bv_mn(lambda = bv_lambda())
)

# ==============================================================================
# [4] Model Estimation & Marginal Log-Likelihood (MLL) Loop
# ==============================================================================

# 4.1 Set candidate lag structure range (lags 1 through 5)
lags_to_test <- 1:5

# 4.2 Initialize master container for storing specification results
all_results <- list()

# 4.3 Iterate estimation across model specifications and lag lengths
for (spec_name in names(specifications)) {
  data_matrix <- specifications[[spec_name]]
  spec_results <- list()
  
  for (p in lags_to_test) {
    message(paste("Fitting:", spec_name, "| Lag:", p))
    
    # 4.3.1 Fit Bayesian VAR model using Minnesota prior configuration
    fit <- bvar(data_matrix, lags = p, priors = setup)
    
    # 4.3.2 Extract Marginal Log-Likelihood (MLL) value directly using logLik()
    spec_results[[length(spec_results) + 1]] <- data.frame(
      Specification           = spec_name,
      Num_Variables           = ncol(data_matrix),
      Lag                     = p,
      Marginal_Log_Likelihood = as.numeric(logLik(fit))
    )
  }
  
  # 4.3.3 Combine lag model results for current specification
  spec_df <- do.call(rbind, spec_results)
  
  # 4.3.4 Identify maximum MLL score for local normalization
  best_mll <- max(spec_df$Marginal_Log_Likelihood)
  
  # 4.3.5 Calculate log-likelihood deviation (Delta MLL) from top specification
  spec_df$Delta_MLL <- spec_df$Marginal_Log_Likelihood - best_mll
  
  # 4.3.6 Compute Bayes Factors relative to optimal lag length
  spec_df$Bayes_Factor_vs_Best <- exp(spec_df$Delta_MLL)
  
  # 4.3.7 Save specification dataframe into global list
  all_results[[spec_name]] <- spec_df
}

# ==============================================================================
# [5] Master Data Frame Assembly & CSV Export
# ==============================================================================

# 5.1 Combine specification metrics into a master summary data frame
master_mll_df <- do.call(rbind, all_results)

# 5.2 Export master Bayesian VAR model selection results to CSV
write.csv(master_mll_df, file = "2_Data_Inspection/master_bvar_mll_results.csv", row.names = FALSE)