###############################################################################
############################# 2.A DATA_SET_INSPECTION #########################
###############################################################################

dir.create("2_Data_Inspection/Outliers", recursive = TRUE, showWarnings = FALSE)

macro_data_inspection <- read_csv("1_Processed_Data/1c_Processed_Data_Set.csv")


#============================================================================#
#                             1.Preliminary Analysis
#============================================================================#

# Create a summary CSV
summary_df <- summary(macro_data_inspection)
write.csv(summary_df, 
"2_Data_Inspection/Summary_Pre_Process.csv")

#----------------------------------------------------------------------------#

#Figure 1: Preliminary Plot
macro_plot <- macro_data_inspection %>%
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
Figure_1_Preliminary_Plot <- ggplot(
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
  filename = "2_Data_Inspection/Figure_1_Preliminary_Plot.png", 
  plot = Figure_1_Preliminary_Plot,
  width = 10, 
  height = 8, 
  dpi = 300
)


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
  var_ts <- ts(vec, start = c(1996, 2), frequency = 4)
  
  # Run test with tryCatch to skip problematic series cleanly
  is_seas <- tryCatch({
    isSeasonal(var_ts, test = "combined", freq = 4)
  }, error = function(e) {
    NA # If test fails, return NA instead of breaking the loop
  })
  
  # Append result
  results <- rbind(results, data.frame(Variable = var_name, Is_Seasonal = is_seas))
}


#                 Variable Is_Seasonal
#1               gdp_growth       FALSE
#2       consumption_growth       FALSE
#3              debt_growth        TRUE
#4    asset_liability_ratio       FALSE
#5              saving_rate        TRUE
#6              debt_income       FALSE
#7          interest_burden        TRUE
#8  real_house_price_growth        TRUE
#9               cpi_growth        TRUE
#10             policy_rate       FALSE
#11    exchange_rate_growth       FALSE
#12           export_growth       FALSE



# 1. List of variables requiring seasonal adjustment
vars_to_deseason <- c("cpi_growth", "debt_growth", "interest_burden", 
                      "real_house_price_growth", "saving_rate")

# 2. Create a clean copy of your dataset
bvar_df <- macro_data_inspection

# 3. Apply X-13ARIMA-SEATS to the 5 seasonal series
for (var_name in vars_to_deseason) {
  
  # Extract series as ts object starting 1996 Q2
  vec <- na.omit(macro_data_inspection[[var_name]])
  var_ts <- ts(vec, start = c(1996, 2), frequency = 4)
  
  # Run X-13 seasonal adjustment
  # Note: If X-13 errors on a specific series, fallback to STL decomposition:
  # sa_vec <- as.numeric(seasadj(stl(var_ts, s.window = "periodic")))
  sa_series <- final(seas(var_ts))
  
  # Replace raw series with seasonally adjusted values
  bvar_df[[var_name]] <- as.numeric(sa_series)
}

# 4. Extract numeric matrix ready for your BVAR package
# (Removes non-numeric date/quarter columns)
numeric_cols <- names(bvar_df)[sapply(bvar_df, is.numeric)]
bvar_matrix <- as.matrix(bvar_df[, numeric_cols])

# Verify dimensions and clean matrix
print(dim(bvar_matrix))
head(bvar_matrix)




library(seastests)

# Initialize data frame for results
results <- data.frame(Variable = character(), Is_Seasonal = logical(), stringsAsFactors = FALSE)

# 1. Select ONLY numeric columns (ignores Date, Quarter, etc.)
numeric_vars <- names(macro_data_inspection)[sapply(macro_data_inspection, is.numeric)]

# 2. Loop through numeric variables only
for (var_name in numeric_vars) {
  
  # Extract clean numeric vector (removing any NAs)
  vec <- na.omit(macro_data_inspection[[var_name]])
  
  # Convert to quarterly ts object
  var_ts <- ts(vec, start = c(1996, 2), frequency = 4)
  
  # Run test with tryCatch to skip problematic series cleanly
  is_seas <- tryCatch({
    isSeasonal(var_ts, test = "combined", freq = 4)
  }, error = function(e) {
    NA # If test fails, return NA instead of breaking the loop
  })
  
  # Append result
  results <- rbind(results, data.frame(Variable = var_name, Is_Seasonal = is_seas))
}






# 1. List of variables requiring seasonal adjustment
vars_to_deseason <- c("cpi_growth", "debt_growth", "interest_burden", 
                      "real_house_price_growth", "saving_rate")

# 2. Create a clean copy of your dataset
bvar_df <- macro_data_inspection

# 3. Apply X-13ARIMA-SEATS to the 5 seasonal series
for (var_name in vars_to_deseason) {
  
  # Extract series as ts object starting 1996 Q2
  vec <- na.omit(macro_data_inspection[[var_name]])
  var_ts <- ts(vec, start = c(1996, 2), frequency = 4)
  
  # Run X-13 seasonal adjustment
  # Note: If X-13 errors on a specific series, fallback to STL decomposition:
  # sa_vec <- as.numeric(seasadj(stl(var_ts, s.window = "periodic")))
  sa_series <- final(seas(var_ts))
  
  # Replace raw series with seasonally adjusted values
  bvar_df[[var_name]] <- as.numeric(sa_series)
}

# 4. Extract numeric matrix ready for your BVAR package
# (Removes non-numeric date/quarter columns)
numeric_cols <- names(bvar_df)[sapply(bvar_df, is.numeric)]
bvar_matrix <- as.matrix(bvar_df[, numeric_cols])




R
# -----------------------------------------------------------------------------
# 1. ADD / FORMAT QUARTER COLUMN
# -----------------------------------------------------------------------------
# Option A: If your original data already has a Date/Quarter column:
# Keep the existing date/quarter column from macro_data_inspection

# Option B: If you need to generate a precise Quarter column starting 1996 Q2:
n_obs <- nrow(bvar_df)
quarter_sequence <- seq(from = as.Date("1996-04-01"), by = "quarter", length.out = n_obs)

# Format as "1996Q2" (or "1996-Q2")
bvar_df$quarter <- paste0(format(quarter_sequence, "%Y"), "Q", quarter(quarter_sequence))

# -----------------------------------------------------------------------------
# 2. REORDER COLUMNS (Put Quarter first)
# -----------------------------------------------------------------------------
# Move 'quarter' column to the front of the data frame
bvar_df <- bvar_df[, c("quarter", setdiff(names(bvar_df), "quarter"))]

# -----------------------------------------------------------------------------
# 3. EXPORT TO CSV
# -----------------------------------------------------------------------------
# Save to your working directory (row.names = FALSE removes automatic R numbers)
write.csv(bvar_df, "swedish_macro_bvar_deseasonalized.csv", row.names = FALSE)

cat("Successfully exported dataset with", nrow(bvar_df), "observations to 'swedish_macro_bvar_deseasonalized.csv'\n")



# -----------------------------------------------------------------------------
# EXPORT DESEASONALIZED DATASET TO CSV
# -----------------------------------------------------------------------------

# 1. Identify your date/quarter column name (e.g., "date", "quarter", "time", "Date")
# If your column is named 'date', make sure it stays at the front:
date_col_name <- names(bvar_df)[sapply(bvar_df, function(x) !is.numeric(x))][1] 
# (Or explicitly set: date_col_name <- "date")

# 2. Ensure date/quarter is the first column
if (!is.na(quarter)) {
  bvar_df <- bvar_df[, c(quarter, setdiff(names(bvar_df), quarter))]
}

# 3. Export to CSV (row.names = FALSE removes automatic R row numbers)
write.csv(bvar_df, "swedish_macro_bvar_deseasonalized.csv", row.names = FALSE)

cat("Successfully exported dataset to 'swedish_macro_bvar_deseasonalized.csv'\n")





#============================================================================#
#                                      2.Outliers
#============================================================================#

# 1. Define target series
target_cols <- c(
  "gdp_growth", "consumption_growth", "debt_growth", 
  "asset_liability_ratio", "saving_rate", "debt_income", 
  "interest_burden", "real_house_price_growth", 
  "exchange_rate_growth", "cpi_growth", "export_growth"
)

# 2. Process all series in a single loop
all_outliers_list <- lapply(target_cols, function(col_name) {
  
  # Convert column to time series directly
  x_ts <- stats::ts(macro_data_inspection[[col_name]], start = c(1996, 2), frequency = 4)
  
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
#Due to large shift with the policy rate, the outlier commands fails with policy rate.
#You can still check it with an inividual time series but it is not as important.














#============================================================================#
#                              3. Final Data Set
#============================================================================#

macro_data_inspection_final <- read_csv("1_Processed_Data/1d_Final_Data_Set.csv")

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
