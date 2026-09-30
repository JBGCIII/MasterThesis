###############################################################################
############################# 2.A DATA_SET_INSPECTION #########################
###############################################################################

# 0.1 Create target directory for outlier figures if it does not already exist
dir.create("2_Data_Inspection/Outliers", recursive = TRUE, showWarnings = FALSE)

# 0.2 Read narrowed pre-inspection macroeconomic dataset
macro_data_inspection <- read_csv("1d_pre_inspection_data_narrowed.csv")

#============================================================================#
#                              1. Seasonal Testing
#============================================================================#

# 1.1 Initialize empty data frame to store seasonality test results
results <- data.frame(Variable = character(), Is_Seasonal = logical(), stringsAsFactors = FALSE)

# 1.2 Identify numeric column names (excluding non-numeric quarter identifiers)
numeric_vars <- names(macro_data_inspection)[sapply(macro_data_inspection, is.numeric)]

# 1.3 Loop over numeric variables to evaluate seasonality
for (var_name in numeric_vars) {
  
  # Extract numeric vector removing any NA values
  vec <- na.omit(macro_data_inspection[[var_name]])
  
  # Convert series to quarterly time series object starting at 1996Q1
  var_ts <- ts(vec, start = c(1996, 1), frequency = 4)
  
  # Evaluate combined seasonality test with error handling fallback
  is_seas <- tryCatch({
    isSeasonal(var_ts, test = "combined", freq = 4)
  }, error = function(e) {
    NA # Return NA if test evaluation fails
  })
  
  # Append variable seasonality result to summary data frame
  results <- rbind(results, data.frame(Variable = var_name, Is_Seasonal = is_seas))
}

# 1.4 Export seasonality test diagnostic results to CSV
write.csv(results, "2_Data_Inspection/Seasonality.csv")

#============================================================================#
#                        2. Seasonal Adjustment (SA)
#============================================================================#

# 2.1 Load generated seasonality diagnostic results
seasonality <- read_csv("2_Data_Inspection/Seasonality.csv")

# 2.2 Identify variables flagged for seasonal adjustment
vars_to_adjust <- seasonality %>% 
  filter(Is_Seasonal == TRUE) %>% 
  pull(Variable)

# 2.3 Initialize container for seasonally adjusted dataset
macro_data_sa <- macro_data_inspection

# 2.4 Apply X-13-ARIMA-SEATS seasonal adjustment to flagged variables
for (var in vars_to_adjust) {
  var_ts <- ts(macro_data_inspection[[var]], start = c(1996, 2), frequency = 4)
  
  # Perform X-13ARIMA-SEATS adjustment with classical decomposition fallback
  sa_ts <- tryCatch({
    final(seas(var_ts))
  }, error = function(e) {
    # Fallback to classical additive decomposition if X-13 model fitting fails
    decomp <- decompose(var_ts, type = "additive")
    var_ts - decomp$seasonal
  })
  
  # Update dataset variable with seasonally adjusted values
  macro_data_sa[[var]] <- as.numeric(sa_ts)
}

# 2.5 Export finalized seasonally adjusted dataset for BSVAR modeling
write_csv(macro_data_sa, "2b_seasonally_adjusted_data.csv")

#============================================================================#
#                          3. Outlier Detection
#============================================================================#

# 3.1 Load seasonally adjusted macroeconomic dataset for outlier detection
macro_data_inspection_outliers <- read_csv("2b_seasonally_adjusted_data.csv")

# 3.2 Extract numeric column names for outlier scanning
numeric_vars <- names(macro_data_inspection)[sapply(macro_data_inspection, is.numeric)]

# 3.3 Scan all numeric time series for additive and innovational outliers using tso()
all_outliers_list <- lapply(numeric_vars, function(col_name) {
  
  # Convert column to quarterly time series object starting at 1996Q1
  x_ts <- stats::ts(macro_data_inspection[[col_name]], start = c(1996, 1), frequency = 4)
  
  # Fit automatic outlier detection model via tsoutliers::tso
  outlier_fit <- suppressWarnings(tso(x_ts))
  
  # Export individual outlier diagnostic plot to PNG
  png(paste0("2_Data_Inspection/Outliers/Figure_Outlier_", col_name, ".png"), 
      width = 10, height = 6, units = "in", res = 300)
  plot(outlier_fit)
  dev.off()

  # Extract identified outlier summary statistics if present
  if (!is.null(outlier_fit$outliers) && nrow(outlier_fit$outliers) > 0) {
    return(cbind(series = col_name, outlier_fit$outliers))
  } else {
    return(NULL)
  }
})

# 3.4 Aggregate detected outliers across all series into a single data frame
outlier_summary_df <- bind_rows(all_outliers_list)

# 3.5 Export aggregated outlier summary table to CSV
write.csv(outlier_summary_df, "2_Data_Inspection/Outlier_Summary.csv")

#============================================================================#
#                       4. Summary Statistics & Visuals
#============================================================================#

# 4.1 Load seasonally adjusted dataset for final inspection and reporting
macro_data_inspection_final <- read_csv("2b_seasonally_adjusted_data.csv")

# 4.2 Generate descriptive summary statistics across all dataset variables
stats <- describe(macro_data_inspection_final)

# 4.3 Format descriptive statistics table selecting target summary indicators
stats_df <- as.data.frame(stats) %>%
  select(n, mean, sd, median, min, max, skew, kurtosis)

# 4.4 Export summary statistics table to CSV
write.csv(stats_df, "2_Data_Inspection/Summary_Post_Process.csv", row.names = TRUE)

# ----------------------------------------------------------------------------#
# 5. Visual Inspection Plotting
# ----------------------------------------------------------------------------#

# 5.1 Reshape dataset into long format for panel plotting with ggplot2
macro_plot <- macro_data_inspection_final %>%
  mutate(
    quarter = as.yearqtr(
      gsub("K", " Q", quarter),
      format = "%Y Q%q"
    )
  ) %>%
  pivot_longer(
    cols = -quarter,
    names_to = "variable",
    values_to = "value"
  )

# 5.2 Construct multi-panel time series overview plot
Figure_1_Final_Plot <- ggplot(
  macro_plot,
  aes(quarter, value)
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(
    ~variable,
    scales = "free_y",
    ncol = 2
  ) +
  theme_bw() +
  labs(
    title = "Swedish Household Macroeconomic Variables",
    x = "",
    y = ""
  )

# 5.3 Export final multi-panel plot to high-resolution PNG file
ggsave(
  filename = "2_Data_Inspection/Figure_1_Final_Plot.png", 
  plot = Figure_1_Final_Plot,
  width = 10, 
  height = 8, 
  dpi = 300
)
