###############################################################################
########## Correlation Test and Particle Component Analysis ###################
###############################################################################

# ==============================================================================
# [1] Dataset Import and Full System Exploratory Analysis
# ==============================================================================

# 1.1 Read seasonally adjusted dataset for correlation and PCA diagnostics
data_set_correlation <- read_csv("2b_seasonally_adjusted_data.csv")

# 1.2 Select all numeric columns for initial levels correlation
df_numeric_levels <- data_set_correlation %>% dplyr::select(where(is.numeric))

# 1.3 Compute and export complete levels correlation matrix
write.csv(
  cor(df_numeric_levels, use = "complete.obs"), 
  "2_Data_Inspection/Correlation_Matrix_Levels.csv"
)

# 1.4 Transform non-stationary I(1) variables to stationary I(0) representations:
# - Log-levels converted to quarter-on-quarter growth rates (log-differences)
# - Rates/ratios converted to first differences (percentage point changes)
df_stationary <- data_set_correlation %>%
  mutate(
    # --- Foreign Block (Growth / Differences) ---
    d_oil_price_log                = oil_price_log - lag(oil_price_log),
    d_fed_funds_rate               = fed_funds_rate - lag(fed_funds_rate),
    d_kix_gdp_log                  = kix_gdp_log - lag(kix_gdp_log),
    d_kix_cpi                      = kix_cpi - lag(kix_cpi),
    
    # --- Domestic Real Block (Growth / Differences) ---
    d_gdp_se_log                   = gdp_se_log - lag(gdp_se_log),
    d_cons_total_log               = cons_total_log - lag(cons_total_log),
    d_cons_durable_log             = cons_durable_log - lag(cons_durable_log),
    d_cons_habitual_log            = cons_habitual_log - lag(cons_habitual_log),
    d_unemployment_rate            = unemployment_rate - lag(unemployment_rate),
    d_consumer_confidence          = consumer_confidence - lag(consumer_confidence),
    
    # --- Housing & Balance Sheet Block (Growth / Differences) ---
    d_house_price_real_log         = house_price_real_log - lag(house_price_real_log),
    d_debt_income                  = debt_income - lag(debt_income),
    d_debt_to_asset_ratio          = debt_to_asset_ratio - lag(debt_to_asset_ratio),
    d_dsr_value                    = dsr_value - lag(dsr_value),
    d_liquid_asset_to_income_ratio = liquid_asset_to_income_ratio - lag(liquid_asset_to_income_ratio),
    d_saving_rate                  = saving_rate - lag(saving_rate),
    
    # --- Domestic Nominal & Policy Block (Growth / Differences) ---
    d_cpi_log                      = cpi_log - lag(cpi_log),
    d_policy_rate                  = policy_rate - lag(policy_rate),
    d_kix_real_log                 = kix_real_log - lag(kix_real_log)
  ) %>%
  drop_na()

# 1.5 Isolate only stationary differenced variables starting with 'd_'
df_clean_pca <- df_stationary %>% 
  dplyr::select(starts_with("d_"))

# 1.6 Compute and export stationary first-difference correlation matrix
write.csv(
  cor(df_clean_pca, use = "complete.obs"), 
  "2_Data_Inspection/Correlation_Matrix_First_Diff.csv"
)

# 1.7 Perform Principal Component Analysis (PCA) on standardized stationary series
pca_stationary <- prcomp(df_clean_pca, scale. = TRUE)

# 1.8 Display PCA summary to console (proportion of variance explained)
summary(pca_stationary)

# 1.9 Export PCA factor loadings (eigenvectors) across all system variables
write.csv(
  pca_stationary$rotation, 
  "2_Data_Inspection/PCA_Loadings_Stationary.csv"
)

# ==============================================================================
# [2] Specification-Specific PCA & Multicollinearity Diagnostics
# ==============================================================================

# 2.1 Define VAR model candidate specifications using differenced variables
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

# 2.2 Create output directory for specification-level PCA files
dir.create("2_Data_Inspection/Specs_PCA", recursive = TRUE, showWarnings = FALSE)

# 2.3 Iterate over candidate specifications: export Correlation, PCA Loadings, and Variance
pca_results <- imap(specifications_diff, function(var_names, spec_name) {
  
  # 2.3.1 Subset data frame to target specification variables
  df_sub <- df_stationary %>% dplyr::select(all_of(var_names))
  
  # 2.3.2 Export specification correlation matrix
  cor_matrix <- cor(df_sub, use = "complete.obs")
  write.csv(
    cor_matrix, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("Correlation_", spec_name, ".csv"))
  )
  
  # 2.3.3 Perform PCA on standardized specification subset
  pca_obj <- prcomp(df_sub, scale. = TRUE)
  
  # 2.3.4 Export specification PCA loadings (eigenvectors)
  write.csv(
    pca_obj$rotation, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("PCA_Loadings_", spec_name, ".csv"))
  )
  
  # 2.3.5 Export specification variance summary table (Std Dev, Proportion, Cumulative)
  pca_summary <- summary(pca_obj)$importance
  write.csv(
    pca_summary, 
    file.path("2_Data_Inspection/Specs_PCA", paste0("PCA_Variance_Summary_", spec_name, ".csv"))
  )
  
  return(pca_obj)
})

# 2.4 Example inspection: Print summary for macro_durable specification
summary(pca_results$macro_durable)

# ==============================================================================
# [3] Targeted Household Financials Multicollinearity & VIF Analysis
# ==============================================================================

# 3.1 Extract and first-difference specific household balance sheet variables
household_diff <- data.frame(
  debt_to_asset_ratio          = diff(bvar_data$debt_to_asset_ratio),
  dsr_value                    = diff(bvar_data$dsr_value),
  liquid_asset_to_income_ratio = diff(bvar_data$liquid_asset_to_income_ratio),
  saving_rate                  = diff(bvar_data$saving_rate)
)

# 3.2 Remove initial missing values (NA) resulting from differencing
household_diff <- na.omit(household_diff)

# 3.3 Compute pairwise correlation matrix across household balance sheet differences
cor_diff <- cor(household_diff, use = "pairwise.complete.obs")
cat("=== Correlation Matrix (First Differences) ===\n")
print(round(cor_diff, 3))

# 3.4 Calculate Variance Inflation Factors (VIF) to detect multicollinearity:
# Formula: VIF_i = 1 / (1 - R_i^2) from regressing variable i on remaining variables
vif_diff_results <- sapply(names(household_diff), function(var) {
  predictors <- setdiff(names(household_diff), var)
  formula_str <- paste(var, "~", paste(predictors, collapse = " + "))
  model <- lm(as.formula(formula_str), data = household_diff)
  
  r_sq <- summary(model)$r.squared
  if (r_sq == 1) return(Inf)
  1 / (1 - r_sq)
})

# 3.5 Output household financial VIF values to console
cat("\n=== Variance Inflation Factors (First Differences) ===\n")
print(round(vif_diff_results, 2))

# 3.6 Export household balance sheet VIF results to CSV
write.csv(
  vif_diff_results, 
  "2_Data_Inspection/Household_Financials_Correlation.csv"
)
