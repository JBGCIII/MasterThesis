###############################################################################
######################### 2b. Stationarity Test ###############################
###############################################################################

#=============================================================================#
#                          [1] Unit Root Tests (ADF & KPSS)
#=============================================================================#

# 1.1 Read seasonally adjusted macroeconomic dataset for stationarity testing
data_set_adf_kpss <- read_csv("2b_seasonally_adjusted_data.csv")

# 1.2 Define multi-specification stationarity checking function (ADF and KPSS)
check_stationarity_multi <- function(x_vector, var_name, alpha = 0.05) {
  
  # 1.2.1 Clean vector by removing missing (NA) observations
  x <- na.omit(x_vector)
  
  # 1.2.2 Ensure adequate sample size before executing unit root tests
  if (length(x) < 10) return(NULL)
  
  results_list <- list()
  
  # 1.2.3 Define testing specifications (Constant/Drift vs. Constant + Trend)
  specs <- list(
    list(name = "Drift (Constant)", adf_type = "drift", kpss_type = "mu"),
    list(name = "Trend (Constant + Trend)", adf_type = "trend", kpss_type = "tau")
  )
  
  # 1.2.4 Loop through deterministic specifications
  for (spec in specs) {
    
    # --------------------------------------------------------------------------
    # 1. ADF Test (Null H0: Series has a unit root / Non-stationary)
    # --------------------------------------------------------------------------
    # Fit Augmented Dickey-Fuller model selecting lag length via AIC
    adf_res <- ur.df(x, type = spec$adf_type, selectlags = "AIC")
    
    # Extract test statistic and 5% critical value based on drift or trend specification
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
    
    # ADF decision rule: Reject H0 if statistic < critical value
    adf_is_stationary <- adf_stat < adf_crit_5pct
    
    # --------------------------------------------------------------------------
    # 2. KPSS Test (Null H0: Series is stationary)
    # --------------------------------------------------------------------------
    # Run Kwiatkowski-Phillips-Schmidt-Shin test for stationarity
    kpss_res <- ur.kpss(x, type = spec$kpss_type)
    kpss_stat <- kpss_res@teststat
    kpss_crit_5pct <- kpss_res@cval[1, "5pct"]
    
    # KPSS decision rule: Fail to reject H0 if statistic < critical value
    kpss_is_stationary <- kpss_stat < kpss_crit_5pct
    
    # --------------------------------------------------------------------------
    # 3. Combined Verdict Logic
    # --------------------------------------------------------------------------
    # Reconcile ADF and KPSS test outcomes to form combined order-of-integration verdict
    verdict <- case_when(
      adf_is_stationary & kpss_is_stationary   ~ "Stationary (I(0))",
      !adf_is_stationary & !kpss_is_stationary ~ "Non-Stationary (I(1))",
      adf_is_stationary & !kpss_is_stationary  ~ "Conflicting (ADF=I(0), KPSS=I(1))",
      !adf_is_stationary & kpss_is_stationary ~ "Conflicting (ADF=I(1), KPSS=I(0))"
    )
    
    # 1.2.5 Store specification test statistics and verdicts
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
  
  # 1.2.6 Return combined specification results data frame
  return(bind_rows(results_list))
}

#=============================================================================#
#                   [2] Automated Execution Across All Variables
#=============================================================================#

# 2.1 Identify all numeric column names for stationarity evaluation
numeric_cols <- names(data_set_adf_kpss)[sapply(data_set_adf_kpss, is.numeric)]

# 2.2 Map unit root function across numeric columns and assemble summary table
stationarity_summary_multi <- map_dfr(numeric_cols, function(col) {
  check_stationarity_multi(data_set_adf_kpss[[col]], var_name = col)
})

# 2.3 Print complete multi-specification stationarity table to console
print(stationarity_summary_multi)

# 2.4 Export complete stationarity summary report to CSV
write_csv(stationarity_summary_multi, "2_Data_Inspection/Stationarity_Summary_MultiSpec.csv")