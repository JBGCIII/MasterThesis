# ==========================================================================================#
#                                       PRELIMINARY DATA INSPECTION 
# ==========================================================================================#
# ==========================================0.Directory======================================#

dir.create(
  "Figures",
  showWarnings = FALSE
)

dir.create(
  "Figures/Outliers",
  showWarnings = FALSE
)

macro_data_inspection <- read_csv("Processed_Data/1c_final_data.csv")

# ====================================1.Preliminary Analysis====================================#

summary(macro_data_inspection) 

#Figure 1: Preliminary Plot
macro_plot <- macro_data_overview %>%
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

# 4. Save to the Figures folder
ggsave(
  filename = "Figures/Figure_1_Preliminary_Plot.png", 
  plot = Figure_1_Preliminary_Plot,
  width = 10, 
  height = 8, 
  dpi = 300
)

# ====================================2.Outliers=======================================#

library(tsoutliers)
library(dplyr)

# 1. Define target series
target_cols <- c(
  "gdp_growth", "consumption_growth", "debt_growth", 
  "asset_liability_ratio", "saving_rate", "debt_income", 
  "interest_burden", "real_house_price_growth"
)


# 2. Ensure output directory exists
dir.create("Figures/Outliers", recursive = TRUE, showWarnings = FALSE)

# 3. Process all series in a single loop
all_outliers_list <- lapply(target_cols, function(col_name) {
  
  # Convert column to time series directly
  x_ts <- ts(macro_data_inspection[[col_name]], start = c(1996, 2), frequency = 4)
  
  # Fit tso
  outlier_fit <- suppressWarnings(tso(x_ts))
  
  # Save plot automatically with unique filename
  png(paste0("Figures/Outliers/Figure_Outlier_", col_name, ".png"), 
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

# View all detected outliers across all 9 variables
print(outlier_summary_df)

#Due to large shift with the policy rate, the outlier commands fails with policy rate.
#You can still check it with an inividual time series but it is not as important.
#The Outlier that is most concerning is the one with debt growth.

# Convert to quarterly ts
debt_growth_ts <- ts(
  macro_data_inspection$debt_growth,
  start = c(1996, 2),
  frequency = 4
)

# Save original value
debt_growth_original <- debt_growth_ts

# Replace 2001 Q1 with NA
debt_growth_clean <- debt_growth_ts
debt_growth_clean[time(debt_growth_clean) == 2001.00] <- NA

# Interpolate missing value
debt_growth_clean <- na.approx(
  debt_growth_clean,
  rule = 2
)

# Create cleaned debt growth column
macro_data_inspection$debt_growth_clean <- as.numeric(debt_growth_clean)

# Replace original debt_growth
macro_data_inspection$debt_growth <- macro_data_inspection$debt_growth_clean

macro_data_inspection$debt_growth_clean <- NULL
macro_data_inspection$date <- NULL

# Save cleaned CSV
write.csv(
  macro_data_inspection,
  "Processed_Data/1d_data_outliers.csv",
  row.names = FALSE
)

# ==========================================================================================#
#                                       Deseasoning
#Strong Seasonality inside the following variables (Savings Rate, Debt Growth)

macro_data_inspection_seasonality <- read_csv("Processed_Data/1d_data_outliers.csv")

# Convert vector to time series object 
saving_ts <- ts(macro_data_inspection_seasonality$saving_rate, start = c(1996, 2), frequency = 4)
debt_growth_ts <- ts(macro_data_inspection_seasonality$debt_growth, start = c(1996, 2), frequency = 4)

# Run X-13 seasonal adjustment
fit <- seas(saving_ts)
fit2 <- seas(debt_growth_ts)

# Extract seasonally adjusted series
saving_rate_sa <- final(fit)
debt_growth_sa <- final(fit2)

# Plot original vs. adjusted
# 4. Save the plot to the Figures directory
png("Figures/Figure_2_Seasonal_Adjustment_Savings_Rate.png", width = 10, height = 6, units = "in", res = 300)
plot(fit)
dev.off()

png("Figures/Figure_3_Seasonal_Adjustment_Debt_Growth.png", width = 10, height = 6, units = "in", res = 300)
plot(fit2)
dev.off()

macro_data <- macro_data_inspection_seasonality %>%
  mutate(
    saving_rate = as.numeric(saving_rate_sa),
    debt_growth = as.numeric(debt_growth_sa)
  )

  write_csv(
  macro_data,
  "Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)


#Figure 1: Preliminary Plot
macro_plot <- macro_data %>%
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

# 4. Save to the Figures folder
ggsave(
  filename = "Figures/Figure_4_Variable_Plot.png", 
  plot = Figure_1_Preliminary_Plot,
  width = 10, 
  height = 8, 
  dpi = 300
)

summary(macro_data) 
