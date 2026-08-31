###############################################################################
############################# 1.b DATA_SET_PROCESSING #########################
###############################################################################

dir.create("1_Processed_Data/Data_Set_Columns",
 recursive = TRUE,
 showWarnings = FALSE)

#============================================================================#
#                           
#============================================================================#


# Financial Assets, Quarterly
fa <- read_csv("0_Raw_Data/5_SCB_household_financial_accounts_full_quarterly.csv")


items_to_subtract <- c(
  "Life insurance and annuity entitlements",
  "Pension funds reserves, total",
  "Other insurance technical reserves",
  "Entitlements to non-pension benefits"
)

fa_quarterly_adjusted <- fa %>%
  group_by(quarter) %>% # Grouping by quarter ensures calculations stay per quarter
  summarize(
    FA_adjusted = sum(Balances[item == "Financial assets (FA)"], na.rm = TRUE) - 
                  sum(Balances[item %in% items_to_subtract], na.rm = TRUE),
    .groups = "drop"
  )


#============================================================================#
#                   Real Asset Quarterly Interpolation
#============================================================================#

# Assets Yearly,
ya <- read_csv("0_Raw_Data/4_SCB_household_balance_sheet_annual.csv")
# House Price Index, Quarterly
ha <- read_csv("0_Raw_Data/8_SCB_house_price_index_quarterly_all_regions.csv")


#----------------------------------------------------------------------------#

# Annual non-financial assets
All_non_financial_assets <- ya %>%
  filter(
    `type of asset` == "All non-financial assets",
    year >= 1996,
    year <= 2026
  ) %>%
  select(
    year,
    `Closing balance, SEK million`
  ) %>%
  arrange(year)

All_non_financial_assets_ts <- ts(
  All_non_financial_assets$`Closing balance, SEK million`,
  start = min(All_non_financial_assets$year),
  frequency = 1
)

#----------------------------------------------------------------------------#

# Quarterly real house-price index
housing_index <- ha %>%
  filter(region == "Sweden") %>%
  select(quarter, Index) %>%
  arrange(quarter)

housing_index_ts <- ts(
  housing_index$Index,
  start = c(1986, 1),
  frequency = 4
)

#----------------------------------------------------------------------------#

fit_nfa_denton_cholette <- td(
  All_non_financial_assets_ts ~ 0 + housing_index_ts,
  to = "quarterly",
  method = "denton-cholette",
  conversion = "last"
)

nfa_quarterly <- predict(fit_nfa_denton_cholette)

#----------------------------------------------------------------------------#

fit_nfa_chow_lin <- td(
  All_non_financial_assets_ts ~ 0 + housing_index_ts,
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
#         Asset-Liability Ratio (With Structural Break Fix & NFA)
#============================================================================#

# 1. Extract Liabilities
household_liab <- fa %>%
  filter(item == "Liabilities (FL)", Balances > 80000) %>%
  dplyr::select(quarter, Balances) %>%
  rename(liabilities = Balances)

#----------------------------------------------------------------------------#
# 2. Extract Adjusted Financial Assets (FA minus insurance & pensions)
items_to_subtract <- c(
  "Life insurance and annuity entitlements",
  "Pension funds reserves, total",
  "Other insurance technical reserves",
  "Entitlements to non-pension benefits"
)

fa_adjusted <- fa %>%
  group_by(quarter) %>%
  summarize(
    fa_adj = sum(Balances[item == "Financial assets (FA)"], na.rm = TRUE) - 
             sum(Balances[item %in% items_to_subtract], na.rm = TRUE),
    .groups = "drop"
  )

#----------------------------------------------------------------------------#
# 3. Extract Quarterly Non-Financial Assets (Denton-Cholette)
nfa_df <- tibble(
  quarter = paste0(floor(as.numeric(time(nfa_quarterly))), "K", cycle(nfa_quarterly)),
  nfa_dc = as.numeric(nfa_quarterly)
)

#----------------------------------------------------------------------------#
# 4. Calculate ratio at level shift (2001K1 break)
liab_2001K1 <- household_liab %>% filter(quarter == "2001K1") %>% pull(liabilities)
liab_2000K4 <- household_liab %>% filter(quarter == "2000K4") %>% pull(liabilities)
shift_factor <- liab_2001K1 / liab_2000K4  # ~1.1227

#----------------------------------------------------------------------------#
# 5. Merge, Adjust Liabilities, and Compute Asset-Liability Ratios
financial_balance_sheet <- fa_adjusted %>%
  left_join(nfa_df, by = "quarter") %>%
  left_join(household_liab, by = "quarter") %>%
  arrange(quarter) %>%
  mutate(
    # Scale pre-2001 liabilities UP to bridge structural shift
    liabilities_adj = if_else(
      quarter < "2001K1",
      liabilities * shift_factor,
      liabilities
    ),
    
    # 1. Adjusted Financial Assets to Liabilities Ratio
    financial_asset_liability_ratio = (fa_adj / liabilities_adj) * 100,
    
    # 2. Total Assets (FA_adj + NFA_dc) to Liabilities Ratio
    total_asset_liability_ratio = ((fa_adj + nfa_dc) / liabilities_adj) * 100
  ) %>%
  dplyr::select(
    quarter, 
    financial_asset_liability_ratio, 
    total_asset_liability_ratio
  )

#----------------------------------------------------------------------------#
# 6. Save Processed Dataset
write_csv(
  financial_balance_sheet, 
  "1_Processed_Data/Data_Set_Columns/4b_financial_asset_liability_ratio.csv"
)


