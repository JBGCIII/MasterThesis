

#============================================================================#
#                            [2] Correlation Test
#============================================================================# 

#============================================================================#
#                   Marginal Likelyhood
#============================================================================# 
library(BVAR)

library(BVAR)

# 1. Load Data
bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")
model_ts <- ts(bvar_data, start = c(1996, 1), frequency = 4)

# 2. Define Specifications
specifications <- list(
  housing_durable  = as.matrix(model_ts[, c("unemployment_rate", "policy_rate", "house_price_real_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_durable_log")]),
  
  housing_habitual = as.matrix(model_ts[, c("unemployment_rate", "policy_rate", "house_price_real_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_habitual_log")]),
  
  macro_durable    = as.matrix(model_ts[, c("gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")]),
  
  macro_habitual   = as.matrix(model_ts[, c("gdp_se_log", "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log",
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "cons_habitual_log")]),
  
  foreign_durable  = as.matrix(model_ts[, c("fed_funds_rate", "kix_gdp_log", "kix_cpi", "gdp_se_log",
                                            "unemployment_rate", "cpi_log", "policy_rate", "cons_durable_log", 
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")]),
  
  foreign_habitual = as.matrix(model_ts[, c("fed_funds_rate", "kix_gdp_log", "kix_cpi", "gdp_se_log",
                                            "unemployment_rate", "cpi_log", "policy_rate", "cons_habitual_log", 
                                            "debt_to_asset_ratio", "dsr_value", "liquid_asset_to_income_ratio",
                                            "saving_rate", "kix_real_log")])
)

# 3. Setup Prior
setup <- bv_priors(
  hyper = "auto",
  mn = bv_mn(lambda = bv_lambda())
)

# 4. Estimation Loop (Direct MLL Extraction)
lags_to_test <- 1:5
all_results <- list()

for (spec_name in names(specifications)) {
  data_matrix <- specifications[[spec_name]]
  spec_results <- list()
  
  for (p in lags_to_test) {
    message(paste("Fitting:", spec_name, "| Lag:", p))
    
    # Fit model
    fit <- bvar(data_matrix, lags = p, priors = setup)
    
    # Extract MLL directly using logLik()
    spec_results[[length(spec_results) + 1]] <- data.frame(
      Specification = spec_name,
      Num_Variables = ncol(data_matrix),
      Lag = p,
      Marginal_Log_Likelihood = as.numeric(logLik(fit))
    )
  }
  
  # Combine lags & calculate Bayes Factors for this specification
  spec_df <- do.call(rbind, spec_results)
  best_mll <- max(spec_df$Marginal_Log_Likelihood)
  spec_df$Delta_MLL <- spec_df$Marginal_Log_Likelihood - best_mll
  spec_df$Bayes_Factor_vs_Best <- exp(spec_df$Delta_MLL)
  
  all_results[[spec_name]] <- spec_df
}

# 5. Master Data Frame & Export
master_mll_df <- do.call(rbind, all_results)

# Save to CSV
write.csv(master_mll_df, file = "2_Data_Inspection/master_bvar_mll_results.csv", row.names = FALSE)