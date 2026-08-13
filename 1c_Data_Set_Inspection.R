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
  x_ts <- stats::ts(macro_data_inspection[[col_name]], start = c(1996, 2), frequency = 4)
  
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

# ==========================================================================================#
#                                       Deseasoning
#Strong Seasonality inside the following variables (Savings Rate)

# 1. Deseasonalize ONLY saving_rate using stats::ts to avoid masking issues
saving_ts <- stats::ts(macro_data_inspection$saving_rate, start = c(1996, 2), frequency = 4)

# Run X-13ARIMA-SEATS
fit_saving     <- seas(saving_ts)
saving_rate_sa <- final(fit_saving)

# 3. Save diagnostic plot
png("Figures/Figure_2_Seasonal_Adjustment_Savings_Rate.png", width = 10, height = 6, units = "in", res = 300)
plot(fit_saving)
dev.off()

# 4. Create final dataset
macro_data <- macro_data_inspection %>%
  mutate(
    saving_rate = as.numeric(saving_rate_sa)
  )

# 5. Export processed data
write_csv(
  macro_data,
  "Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)
