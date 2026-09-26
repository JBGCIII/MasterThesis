

#============================================================================#
#                            [2] Correlation Test
#============================================================================# 


data_set_correlation <- read_csv("2b_seasonally_adjusted_data.csv")


# 2. Compute Correlation Matrix on Raw Levels (for inspection)
df_numeric_levels <- data_set_correlation %>% dplyr::select(where(is.numeric))

write.csv(
  cor(df_numeric_levels, use = "complete.obs"), 
  "2_Data_Inspection/Correlation_Matrix_Levels.csv"
)

# 3. Transform I(1) variables to stationary I(0) representations
# Log-levels become Quarter-on-Quarter Growth Rates (Log-Differences)
# Percentages/Ratios become First Differences (Percentage Point Changes)
df_stationary <- data_set_correlation %>%
  mutate(
    # --- Foreign Block (Growth / Diff) ---
    d_oil_price_log                = oil_price_log - lag(oil_price_log),
    d_fed_funds_rate               = fed_funds_rate - lag(fed_funds_rate),
    d_kix_gdp_log                  = kix_gdp_log - lag(kix_gdp_log),
    d_kix_cpi                      = kix_cpi - lag(kix_cpi),
    
    # --- Domestic Real Block (Growth / Diff) ---
    d_gdp_se_log                   = gdp_se_log - lag(gdp_se_log),
    d_cons_total_log               = cons_total_log - lag(cons_total_log),
    d_cons_durable_log             = cons_durable_log - lag(cons_durable_log),
    d_cons_habitual_log            = cons_habitual_log - lag(cons_habitual_log),
    d_unemployment_rate            = unemployment_rate - lag(unemployment_rate),
    d_consumer_confidence          = consumer_confidence - lag(consumer_confidence),
    
    # --- Housing & Balance Sheet Block (Growth / Diff) ---
    d_house_price_real_log         = house_price_real_log - lag(house_price_real_log),
    d_debt_income                  = debt_income - lag(debt_income),
    d_debt_to_asset_ratio          = debt_to_asset_ratio - lag(debt_to_asset_ratio),
    d_dsr_value                    = dsr_value - lag(dsr_value),
    d_liquid_asset_to_income_ratio = liquid_asset_to_income_ratio - lag(liquid_asset_to_income_ratio),
    d_saving_rate                  = saving_rate - lag(saving_rate),
    
    # --- Domestic Nominal & Policy Block (Growth / Diff) ---
    d_cpi_log                      = cpi_log - lag(cpi_log),
    d_policy_rate                  = policy_rate - lag(policy_rate),
    d_kix_real_log                 = kix_real_log - lag(kix_real_log)
  ) %>%
  drop_na()

# 4. Select ONLY the stationary transformed variables
df_clean_pca <- df_stationary %>% 
  dplyr::select(starts_with("d_"))

# 5. Export Stationary Correlation Matrix
write.csv(
  cor(df_clean_pca, use = "complete.obs"), 
  "2_Data_Inspection/Correlation_Matrix_First_Diff.csv"
)

# 6. Run Principal Component Analysis (PCA) on standardized stationary series
pca_stationary <- prcomp(df_clean_pca, scale. = TRUE)

# 7. Print PCA Summary to Console and Export Factor Loadings
summary(pca_stationary)

# Export PCA loadings (eigenvectors) to examine variable contributions per component
write.csv(
  pca_stationary$rotation, 
  "2_Data_Inspection/PCA_Loadings_Stationary.csv"
)


#============================================================================#
#                  [2] Correlation Test Financial Variables
#============================================================================# 

# 2. Define Specifications using ONLY differenced variables (d_)
specifications_diff <- list(
  housing_durable  = c("d_unemployment_rate", "d_policy_rate", "d_house_price_real_log",
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_cons_durable_log"),
  
  housing_habitual = c("d_unemployment_rate", "d_policy_rate", "d_house_price_real_log",
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_cons_habitual_log"),
  
  macro_durable    = c("d_gdp_se_log", "d_unemployment_rate", "d_cpi_log", "d_policy_rate", "d_cons_durable_log",
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_kix_real_log"),
  
  macro_habitual   = c("d_gdp_se_log", "d_unemployment_rate", "d_cpi_log", "d_policy_rate", "d_cons_habitual_log",
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_kix_real_log"),
  
  foreign_durable  = c("d_fed_funds_rate", "d_kix_gdp_log", "d_kix_cpi", "d_gdp_se_log",
                       "d_unemployment_rate", "d_cpi_log", "d_policy_rate", "d_cons_durable_log", 
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_kix_real_log"),
  
  foreign_habitual = c("d_fed_funds_rate", "d_kix_gdp_log", "d_kix_cpi", "d_gdp_se_log",
                       "d_unemployment_rate", "d_cpi_log", "d_policy_rate", "d_cons_habitual_log", 
                       "d_debt_to_asset_ratio", "d_dsr_value", "d_liquid_asset_to_income_ratio",
                       "d_saving_rate", "d_kix_real_log")
)

# 3. Create export directory
dir.create("2_Data_Inspection/Specs_PCA", recursive = TRUE, showWarnings = FALSE)

# 4. Iterate over specifications: compute Correlation, run PCA, export results
pca_results <- imap(specifications_diff, function(var_names, spec_name) {
  
  # Extract relevant differenced subset
  df_sub <- df_stationary %>% dplyr::select(all_of(var_names))
  
  # A. Save Correlation Matrix
  cor_matrix <- cor(df_sub, use = "complete.obs")
  write.csv(
    cor_matrix, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("Correlation_", spec_name, ".csv"))
  )
  
  # B. Run PCA on standardized differenced series
  pca_obj <- prcomp(df_sub, scale. = TRUE)
  
  # C. Save Loadings (Eigenvectors)
  write.csv(
    pca_obj$rotation, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("PCA_Loadings_", spec_name, ".csv"))
  )
  
  # D. Save Variance Summary Table (Standard Dev, Proportion, Cumulative)
  pca_summary <- summary(pca_obj)$importance
  write.csv(
    pca_summary, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("PCA_Variance_Summary_", spec_name, ".csv"))
  )
  
  return(pca_obj)
})

# Access any PCA model from the list directly (e.g., macro_durable summary):
summary(pca_results$macro_durable)





#============================================================================#
#                  [2] Correlation Test Financial Variables
#============================================================================# 

# 1. Extract and take first differences of the household variables
household_diff <- data.frame(
  debt_to_asset_ratio          = diff(bvar_data$debt_to_asset_ratio),
  dsr_value                    = diff(bvar_data$dsr_value),
  liquid_asset_to_income_ratio = diff(bvar_data$liquid_asset_to_income_ratio),
  saving_rate                  = diff(bvar_data$saving_rate)
)

# Remove any missing values resulting from the diff() operation
household_diff <- na.omit(household_diff)

# 2. Compute Correlation Matrix on Differences
cor_diff <- cor(household_diff, use = "pairwise.complete.obs")
cat("=== Correlation Matrix (First Differences) ===\n")
print(round(cor_diff, 3))

# 3. Compute VIF on First Differences
vif_diff_results <- sapply(names(household_diff), function(var) {
  predictors <- setdiff(names(household_diff), var)
  formula_str <- paste(var, "~", paste(predictors, collapse = " + "))
  model <- lm(as.formula(formula_str), data = household_diff)
  
  r_sq <- summary(model)$r.squared
  if (r_sq == 1) return(Inf)
  1 / (1 - r_sq)
})

cat("\n=== Variance Inflation Factors (First Differences) ===\n")
print(round(vif_diff_results, 2))



# Export PCA loadings (eigenvectors) to examine variable contributions per component
write.csv(
  vif_diff_results, 
  "2_Data_Inspection/Household_Financials_Correlation.csv"
)


