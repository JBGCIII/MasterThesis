###############################################################################
############################# 2.A DATA_SET_INSPECTION #########################
###############################################################################

dir.create("2_Data_Inspection/Outliers", recursive = TRUE, showWarnings = FALSE)

macro_data_inspection <- read_csv("1d_pre_inspection_data_narrowed.csv")

#============================================================================#
#                              2. Seasonal Adjustment
#============================================================================#

# Initialize data frame for results
results <- data.frame(Variable = character(), Is_Seasonal = logical(), stringsAsFactors = FALSE)

# 1. Select ONLY numeric columns (ignores Date, Quarter, etc.)
numeric_vars <- names(macro_data_inspection)[sapply(macro_data_inspection, is.numeric)]

# 2. Loop through numeric variables only
for (var_name in numeric_vars) {
  
  # Extract clean numeric vector (removing any NAs)
  vec <- na.omit(macro_data_inspection[[var_name]])
  
  # Convert to quarterly ts object
  var_ts <- ts(vec, start = c(1996, 1), frequency = 4)
  
  # Run test with tryCatch to skip problematic series cleanly
  is_seas <- tryCatch({
    isSeasonal(var_ts, test = "combined", freq = 4)
  }, error = function(e) {
    NA # If test fails, return NA instead of breaking the loop
  })
  
  # Append result
  results <- rbind(results, data.frame(Variable = var_name, Is_Seasonal = is_seas))
}


write.csv(results, "2_Data_Inspection/Seasonality.csv")

# Load seasonality inspection output
seasonality <- read_csv("2_Data_Inspection/Seasonality.csv")

# Identify variables that need adjustment
vars_to_adjust <- seasonality %>% 
  filter(Is_Seasonal == TRUE) %>% 
  pull(Variable)

macro_data_sa <- macro_data_inspection

# Apply X-13-ARIMA-SEATS adjustment to flagged variables
for (var in vars_to_adjust) {
  var_ts <- ts(macro_data_inspection[[var]], start = c(1996, 2), frequency = 4)
  
  # Run X-13-ARIMA-SEATS with fallback handling
  sa_ts <- tryCatch({
    final(seas(var_ts))
  }, error = function(e) {
    # Fallback to classical multiplicative/additive decomposition if X-13 fails
    decomp <- decompose(var_ts, type = "additive")
    var_ts - decomp$seasonal
  })
  
  macro_data_sa[[var]] <- as.numeric(sa_ts)
}

# Export the seasonally adjusted dataset for BSVAR estimation
write_csv(macro_data_sa, "2b_seasonally_adjusted_data.csv")


#============================================================================#
#                                      2.Outliers
#============================================================================#

macro_data_inspection_outliers <- read_csv("2b_seasonally_adjusted_data.csv")

numeric_vars <- names(macro_data_inspection)[sapply(macro_data_inspection, is.numeric)]

# 2. Process all series in a single loop
all_outliers_list <- lapply(numeric_vars, function(col_name) {
  
  # Convert column to time series directly
  x_ts <- stats::ts(macro_data_inspection[[col_name]], start = c(1996, 1), frequency = 4)
  
  # Fit tso
  outlier_fit <- suppressWarnings(tso(x_ts))
  
  # Save plot automatically with unique filename
  png(paste0("2_Data_Inspection/Outliers/Figure_Outlier_", col_name, ".png"), 
      width = 10, height = 6, units = "in", res = 300)
  plot(outlier_fit)
  dev.off()


# Extract outlier summary if any were found
  if (!is.null(outlier_fit$outliers) && nrow(outlier_fit$outliers) > 0) {
    return(cbind(series = col_name, outlier_fit$outliers))
  } else {
    return(NULL)
  }
})

# 4. Combine all detected outliers into one clean dataframe
outlier_summary_df <- bind_rows(all_outliers_list)


# 5. Export as CSV
write.csv(outlier_summary_df, 
"2_Data_Inspection/Outlier_Summary.csv")



#============================================================================#
#                              3. Final Data Set
#============================================================================#
# Inspecting the final data set. And now you are asking, Joaquim you do not
# need to re-read the data from CSV file multiple times! I know but let me
# do this this way for safety!
macro_data_inspection_final <- read_csv("2b_seasonally_adjusted_data.csv")

stats <- describe(macro_data_inspection_final)

# 2. Convert to data frame and clean up columns
stats_df <- as.data.frame(stats) %>%
  select(n, mean, sd, median, min, max, skew, kurtosis)

# 3. Export directly to CSV
write.csv(stats_df, "2_Data_Inspection/Summary_Post_Process.csv", row.names = TRUE)

#----------------------------------------------------------------------------#

#Figure 1: Preliminary Plot
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

# 3. Create and assign plot
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

# 4. Save to the 2_Data_Inspection folder
ggsave(
  filename = "2_Data_Inspection/Figure_1_Final_Plot.png", 
  plot = Figure_1_Final_Plot,
  width = 10, 
  height = 8, 
  dpi = 300
)
