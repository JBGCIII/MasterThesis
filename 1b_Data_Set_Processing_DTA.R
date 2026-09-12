###############################################################################
############################# 1.b DATA_SET_PROCESSING #########################
###############################################################################
#                   Real Asset Quarterly Interpolation

# Assets Yearly,
yearly_asset <- read_csv("0_Raw_Data/4_SCB_household_balance_sheet_annual.csv")
# House Price Index, Quarterly
ha <- read_csv("0_Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv")

unique(yearly_asset$`type of asset`)


#----------------------------------------------------------------------------#

# Define housing items according to SCB national balance sheet standards
housing_items <- c(
  "Dwellings ",                                    # [7] Structure value
  "Land underlying dwellings",                    # [33] Land value
  "Other equity, tenant ownership rights"        # [50] Tenant-owned apartments (bostadsrätter)
                                                   # The item needs to be be removed if using financial assets as tenant ownership rights"
                                                   # are already included in financial asset (Thank you ESA 2010)
                                                   # I don't so it's ok!
)
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

# Convert to annual time series
housing_assets_ts <- ts(
  housing_assets_annual$housing_assets,
  start = min(housing_assets_annual$year),
  frequency = 1
)

housing_assets_ts
#----------------------------------------------------------------------------#

housing_index_filtered <- ha %>%
  filter(region == "Sweden", quarter >= "1996K1") %>%
  arrange(quarter)

housing_index_ts <- ts(
  housing_index_filtered$Index,
  start = c(1996, 1), # Year 1996, Quarter 1
  frequency = 4
)
#----------------------------------------------------------------------------#

fit_nfa_denton_cholette <- td(
  housing_assets_ts ~ 0 + housing_index_ts,
  to = "quarterly",
  method = "denton-cholette",
  conversion = "last"
)

nfa_quarterly <- predict(fit_nfa_denton_cholette)

#----------------------------------------------------------------------------#

fit_nfa_chow_lin <- td(
  housing_assets_ts ~ 0 + housing_index_ts,
  to = "quarterly",
  method = "chow-lin-maxlog",
  conversion = "last"
)

nfa_quarterly_controll <- predict(fit_nfa_chow_lin)

#----------------------------------------------------------------------------#

growth_dc <- 100 * diff(log(nfa_quarterly))
growth_cl <- 100 * diff(log(nfa_quarterly_controll))

#----------------------------------------------------------------------------#

#isSeasonal(nfa_quarterly, test = "combined", freq = 4)
#NFA is seasonal, but seasonality dissapears in the calculation for the ratio.

#============================================================================#

# 1. Open PNG graphics device
png("2_Data_Inspection/Figure_1_interpolation_growth_comparison.png",
 width = 800, height = 600)

#----------------------------------------------------------------------------#
# 2. Plot the first series
plot(growth_dc, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     ylim = range(c(growth_dc, growth_cl), na.rm = TRUE),
     main = "Quarterly NFA Growth Comparison (Denton-Cholette/Chow-Lin-maxlog)",
     ylab = "Growth Rate (%)",
     xlab = "Time")

#----------------------------------------------------------------------------#
# 3. Add the second series
lines(growth_cl, col = "red", lwd = 2)

#----------------------------------------------------------------------------#
# 4. Add a legend
legend("topright", 
       legend = c("NFA Growth", "NFA Growth (Controlled)"), 
       col = c("blue", "red"), 
       lwd = 2)
#----------------------------------------------------------------------------#
# 5. Save and close the PNG file
dev.off()

#============================================================================#
#                            Debt to Asset
#============================================================================#

#---------------------------------------------------------------
household_loans <- fa %>%
  filter(item == "Loans, total") %>%
  group_by(quarter) %>%
  # Filter out the small asset series by keeping the primary liability sum
  summarize(
    Loans_total = max(Balances, na.rm = TRUE), # Keeps the large liability series
    .groups = "drop"
  )

# Convert interpolated NFA ts object to a data frame matching SCB quarters
nfa_dates <- zoo::as.yearqtr(time(nfa_quarterly))

nfa_df <- tibble(
  quarter = gsub("Q", "K", format(nfa_dates, "%YQ%q")), # Ensures format matches SCB "1996K1"
  nfa_dc = as.numeric(nfa_quarterly)
)

# 5. Merge, Adjust Liabilities, and Compute Asset-Liability Ratios
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

#---------------------------------------------------------------------------#
# 6. Save Processed Dataset
#---------------------------------------------------------------------------#
write_csv(
  debt_to_asset, 
  "1_Processed_Data/Data_Set_Columns/3-5_B_debt_to_real_asset.csv"
)

