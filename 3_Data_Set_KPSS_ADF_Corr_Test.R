###############################################################################
############################ 2b. KPSS/ADF/CORR TEST ###########################
###############################################################################
#                              [1] Unit root Tests
#============================================================================#

data_set_adf_kpss <- read_csv("2b_seasonally_adjusted_data.csv")

# Helper function to run ADF & KPSS tests across multiple specifications
check_stationarity_multi <- function(x_vector, var_name, alpha = 0.05) {
  # Clean vector (remove NAs)
  x <- na.omit(x_vector)
  
  # Ensure there's enough data to test
  if (length(x) < 10) return(NULL)
  
  results_list <- list()
  
  # Define the specifications to test
  specs <- list(
    list(name = "Drift (Constant)", adf_type = "drift", kpss_type = "mu"),
    list(name = "Trend (Constant + Trend)", adf_type = "trend", kpss_type = "tau")
  )
  
  for (spec in specs) {
    # --------------------------------------------------------------------------
    # 1. ADF Test (Null H0: Series has a unit root / Non-stationary)
    # --------------------------------------------------------------------------
    adf_res <- ur.df(x, type = spec$adf_type, selectlags = "AIC")
    
    # Extract test statistic and 5% critical value based on type
    if (spec$adf_type == "drift") {
      adf_stat <- adf_res@teststat[1, "tau2"]
      adf_crit_5pct <- adf_res@cval["tau2", "5pct"]
    } else if (spec$adf_type == "trend") {
      adf_stat <- adf_res@teststat[1, "tau3"]
      adf_crit_5pct <- adf_res@cval["tau3", "5pct"]
    } else {
      adf_stat <- adf_res@teststat[1, "tau1"]
      adf_crit_5pct <- adf_res@cval["tau1", "5pct"]
    }
    
    adf_is_stationary <- adf_stat < adf_crit_5pct
    
    # --------------------------------------------------------------------------
    # 2. KPSS Test (Null H0: Series is stationary)
    # --------------------------------------------------------------------------
    kpss_res <- ur.kpss(x, type = spec$kpss_type)
    kpss_stat <- kpss_res@teststat
    kpss_crit_5pct <- kpss_res@cval[1, "5pct"]
    
    kpss_is_stationary <- kpss_stat < kpss_crit_5pct
    
    # --------------------------------------------------------------------------
    # 3. Combined Verdict
    # --------------------------------------------------------------------------
    verdict <- case_when(
      adf_is_stationary & kpss_is_stationary  ~ "Stationary (I(0))",
      !adf_is_stationary & !kpss_is_stationary ~ "Non-Stationary (I(1))",
      adf_is_stationary & !kpss_is_stationary  ~ "Conflicting (ADF=I(0), KPSS=I(1))",
      !adf_is_stationary & kpss_is_stationary ~ "Conflicting (ADF=I(1), KPSS=I(0))"
    )
    
    results_list[[spec$name]] <- data.frame(
      Variable       = var_name,
      Specification  = spec$name,
      ADF_Stat       = round(adf_stat, 3),
      ADF_Crit_5pct  = round(adf_crit_5pct, 3),
      ADF_Pass       = adf_is_stationary,
      KPSS_Stat      = round(kpss_stat, 3),
      KPSS_Crit_5pct = round(kpss_crit_5pct, 3),
      KPSS_Pass      = kpss_is_stationary,
      Verdict        = verdict,
      stringsAsFactors = FALSE
    )
  }
  
  return(bind_rows(results_list))
}

# ==============================================================================
# Run automatically across ALL numeric columns
# ==============================================================================

numeric_cols <- names(data_set_adf_kpss)[sapply(data_set_adf_kpss, is.numeric)]

# Run loop and combine into one comprehensive summary table
stationarity_summary_multi <- map_dfr(numeric_cols, function(col) {
  check_stationarity_multi(data_set_adf_kpss[[col]], var_name = col)
})

# Print summary to console
print(stationarity_summary_multi)

# Export to CSV for inspection
write_csv(stationarity_summary_multi, "2_Data_Inspection/Stationarity_Summary_MultiSpec.csv")


#============================================================================#
#                            [2] Correlation Test
#============================================================================# 

# 2. Compute Correlation Matrix on Raw Levels (for inspection)
df_numeric_levels <- data_set_adf_kpss %>% dplyr::select(where(is.numeric))

write.csv(
  cor(df_numeric_levels, use = "complete.obs"), 
  "2_Data_Inspection/Correlation_Matrix_Levels.csv"
)

# 3. Transform I(1) variables to stationary I(0) representations
# Log-levels become Quarter-on-Quarter Growth Rates (Log-Differences)
# Percentages/Ratios become First Differences (Percentage Point Changes)
df_stationary <- data_set_adf_kpss %>%
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



