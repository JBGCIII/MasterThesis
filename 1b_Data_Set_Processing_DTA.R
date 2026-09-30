###############################################################################
################################ Debt-to-Asset ################################
###############################################################################

# =============================================================================
# 1. Real Asset Quarterly Interpolation
# =============================================================================

# 1.1 Load annual household real assets dataset from SCB
yearly_asset <- read_csv("0_Raw_Data/4_SCB_household_balance_sheet_annual.csv")

# 1.2 Load quarterly house price index dataset from SCB
ha <- read_csv("0_Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv")

# 1.3 Inspect unique asset classifications in raw data
unique(yearly_asset$`type of asset`)

# ------------------------------------------------------------------------------
# 2. Extract & Aggregate Annual Housing Assets
# ------------------------------------------------------------------------------

# 2.1 Define housing items according to SCB national balance sheet standards
housing_items <- c(
  "Dwellings ",                           # [7] Structure value
  "Land underlying dwellings",           # [33] Land value
  "Other equity, tenant ownership rights"# [50] Tenant-owned apartments (bostadsrätter)
                                         # Note: tenant ownership rights are included as housing asset
                                         # (ESA 2010 treats it under financial assets if separate)
)

# 2.2 Filter and aggregate total annual housing asset values (SEK million)
housing_assets_annual <- yearly_asset %>%
  # Strip trailing spaces from asset names to ensure clean matching
  mutate(type_clean = trimws(`type of asset`)) %>%
  filter(
    type_clean %in% trimws(housing_items),
    year >= 1996,
    year <= 2026
  ) %>%
  group_by(year) %>%
  summarize(
    housing_assets = sum(`Closing balance, SEK million`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# 2.3 Convert aggregated annual values into an annual R time series object (ts)
housing_assets_ts <- ts(
  housing_assets_annual$housing_assets,
  start = min(housing_assets_annual$year),
  frequency = 1
)

housing_assets_ts

# ------------------------------------------------------------------------------
# 3. Prepare High-Frequency Indicator (Quarterly House Price Index)
# ------------------------------------------------------------------------------

# 3.1 Filter house price index for nationwide coverage from 1996 Q1 onwards
housing_index_filtered <- ha %>%
  filter(region == "Sweden", quarter >= "1996K1") %>%
  arrange(quarter)

# 3.2 Convert quarterly index into a quarterly time series object (ts)
housing_index_ts <- ts(
  housing_index_filtered$Index,
  start = c(1996, 1), # Year 1996, Quarter 1
  frequency = 4
)

# ------------------------------------------------------------------------------
# 4. Temporal Disaggregation (Annual to Quarterly Interpolation)
# ------------------------------------------------------------------------------

# 4.1 Perform Denton-Cholette temporal disaggregation (stock series: conversion = "last")
fit_nfa_denton_cholette <- td(
  housing_assets_ts ~ 0 + housing_index_ts,
  to = "quarterly",
  method = "denton-cholette",
  conversion = "last"
)

# 4.2 Extract interpolated quarterly Non-Financial Asset (NFA) series
nfa_quarterly <- predict(fit_nfa_denton_cholette)

# 4.3 Perform Chow-Lin maxlog temporal disaggregation as a robustness control
fit_nfa_chow_lin <- td(
  housing_assets_ts ~ 0 + housing_index_ts,
  to = "quarterly",
  method = "chow-lin-maxlog",
  conversion = "last"
)

# 4.4 Extract alternative Chow-Lin interpolated quarterly series
nfa_quarterly_controll <- predict(fit_nfa_chow_lin)

# ------------------------------------------------------------------------------
# 5. Method Comparison & Inspection
# ------------------------------------------------------------------------------

# 5.1 Calculate annualized quarterly log growth rates (%) for both methods
growth_dc <- 100 * diff(log(nfa_quarterly))
growth_cl <- 100 * diff(log(nfa_quarterly_controll))

# Note on Seasonality:
# isSeasonal(nfa_quarterly, test = "combined", freq = 4)
# NFA exhibits seasonality, but seasonality disappears when calculating the debt ratio.

# 5.2 Open PNG graphics device to save method comparison plot
png("2_Data_Inspection/Figure_1_interpolation_growth_comparison.png",
    width = 800, height = 600)

# 5.3 Plot growth rates from Denton-Cholette interpolation
plot(growth_dc, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     ylim = range(c(growth_dc, growth_cl), na.rm = TRUE),
     main = "Quarterly NFA Growth Comparison (Denton-Cholette/Chow-Lin-maxlog)",
     ylab = "Growth Rate (%)",
     xlab = "Time")

# 5.4 Overlay growth rates from Chow-Lin interpolation
lines(growth_cl, col = "red", lwd = 2)

# 5.5 Add legend to plot
legend("topright", 
       legend = c("NFA Growth", "NFA Growth (Controlled)"), 
       col = c("blue", "red"), 
       lwd = 2)

# 5.6 Close PNG device and save file
dev.off()

# =============================================================================
# 6. Debt-to-Asset Ratio Calculation
# =============================================================================

# 6.1 Extract total household loan liabilities from financial accounts (fa)
household_loans <- fa %>%
  filter(item == "Loans, total") %>%
  group_by(quarter) %>%
  # Filter out small asset series by taking maximum balance value (primary liability)
  summarize(
    Loans_total = max(Balances, na.rm = TRUE), # Keeps the large liability series
    .groups = "drop"
  )

# 6.2 Convert interpolated NFA ts object to a tibble with matching SCB quarter format ("YYYYK1")
nfa_dates <- zoo::as.yearqtr(time(nfa_quarterly))

nfa_df <- tibble(
  quarter = gsub("Q", "K", format(nfa_dates, "%YQ%q")), # Format match for SCB "1996K1"
  nfa_dc = as.numeric(nfa_quarterly)
)

# 6.3 Merge liabilities with interpolated real assets and compute ratio (%)
debt_to_asset <- nfa_df %>%
  left_join(household_loans, by = "quarter") %>%
  arrange(quarter) %>%
  mutate( 
    debt_to_asset_ratio = (Loans_total / nfa_dc) * 100
  ) %>%
  dplyr::select(
    quarter, 
    debt_to_asset_ratio
  )

# ------------------------------------------------------------------------------
# 7. Save Processed Dataset
# ------------------------------------------------------------------------------

# 7.1 Export completed Debt-to-Real-Asset ratio series to CSV
write_csv(
  debt_to_asset, 
  "1_Processed_Data/Data_Set_Columns/3-5_B_debt_to_real_asset.csv"
)