###############################################################################
############################# 1.b DATA_SET_PROCESSING #########################
###############################################################################


#============================================================================#
#         Column (1-2; 10) SCB Real Growth Rate (GDP,Consumption, Export) 
#============================================================================#

# One could have named 0_Raw_Data but to it leads to Permission Issues.
gdp_growth_seasonal_adjust <- read_csv("0_Raw_Data/1_SCB_gdp_growth_quarterly.csv")
consumption_growth_seasonal_adjust <- read_csv("0_Raw_Data/2_SCB_household_consumption_real_quarterly.csv")
export_growth_seasonal_adjust <- read_csv("0_Raw_Data/5_SCB_Export_Growth.csv")
asset_liability_ratio <- read_csv("1_Processed_Data/Data_Set_Columns/4b_financial_asset_liability_ratio.csv")




#============================================================================#
#         Column (5-8) Household Financial Indicators
#============================================================================#

sector <- read_csv("0_Raw_Data/3_SCB_household_sector_indicators.csv")
#----------------------------------------------------------------------------#

household_indicators <- sector %>%
  rename(value = `Key indicators for income growth`) %>%
  filter(
    sector == "Households",
    indicator %in% c(
      "Net saving rate,  seasonally adjusted",
      "Debt, per cent of disposable income, net, four quarter",
      "Interest payments, gross, as a percentage of disposable income, net"
    )
  ) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(
    saving_rate = `Net saving rate,  seasonally adjusted`,
    debt_income = `Debt, per cent of disposable income, net, four quarter`,
    interest_burden = `Interest payments, gross, as a percentage of disposable income, net`
  )
  

#----------------------------------------------------------------------------#
write_csv(household_indicators, "1_Processed_Data/Data_Set_Columns/5b_household_indicators.csv")



#============================================================================#
#                     Column (10) CPI Quarterly Inflation
#============================================================================#

cpi <- read_csv("0_Raw_Data/6_SCB_CPI_monthly.csv")
#----------------------------------------------------------------------------#
cpi_quarterly <- cpi %>%
  mutate(
    year = substr(month, 1, 4),
    month_number = as.numeric(substr(month, 6, 7)),
    quarter = paste0(year, "K", ceiling(month_number / 3))
  ) %>%
  group_by(quarter) %>%
  summarise(
    cpi_index = mean(`CPI, Fixed Index numbers`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter) %>%
  mutate(
    cpi_growth = 100 * (log(cpi_index) - lag(log(cpi_index)))
  )
#----------------------------------------------------------------------------#

write_csv(cpi_quarterly, "1_Processed_Data/Data_Set_Columns/6b_inflation_quarterly.csv")



# ==============================================================================
#                      Column (11) Riksbank Policy Rate
# ==============================================================================

repo_rate <- read_csv("0_Raw_Data/8_Riksbank_policy_rate_daily.csv")


policy_rate_quarterly <- repo_rate %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date) %>%
  
#----------------------------------------------------------------------------#
  # Create observations for every calendar day
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%
  
  # Carry the most recent policy rate forward
  fill(value, .direction = "down") %>%
  
  mutate(
    year = format(date, "%Y"),
    month = as.numeric(format(date, "%m")),
    quarter = paste0(year, "K", ceiling(month / 3))
  ) %>%
#----------------------------------------------------------------------------#

  group_by(quarter) %>%
  summarise(
    policy_rate = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  arrange(quarter)
#----------------------------------------------------------------------------#

write_csv(
  policy_rate_quarterly,
  "1_Processed_Data/Data_Set_Columns/8b_policy_rate_quarterly.csv"
)


# ==============================================================================
#                         Column (12)  KIX Exchange Rate Index
# ==============================================================================

KIX92_quarterly <- read_csv("0_Raw_Data/9_KIX_Exchange_Rate_Index.csv")

#----------------------------------------------------------------------------#

KIX92_quarterly <- KIX92_quarterly %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date) %>%
  
#----------------------------------------------------------------------------#
  # Create observations for every calendar day
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%
  
  # Carry the most recent value forward (fixed variable reference)
  fill(value, .direction = "down") %>%
  
  mutate(
    year = format(date, "%Y"),
    month = as.numeric(format(date, "%m")),
    quarter = paste0(year, "K", ceiling(month / 3)) 
  ) %>%
  
#----------------------------------------------------------------------------#
  group_by(quarter) %>%
  summarise(
    kix_index = mean(value, na.rm = TRUE), # Named properly instead of policy_rate
    .groups = "drop"
  ) %>%
  arrange(quarter) %>%
  
#----------------------------------------------------------------------------#
  # Now calculate quarterly percentage log-growth correctly
  mutate(
    exchange_rate_growth = 100 * (log(kix_index) - log(lag(kix_index)))
  )

# Write out the processed CSV
write_csv(
  KIX92_quarterly,
  "1_Processed_Data/Data_Set_Columns/9b_KIX92_quarterly.csv"
)

# ==============================================================================
                            # Data Set Creation
# ==============================================================================

# Final Dataset Merge
macro_households <- list(
  gdp_growth_seasonal_adjust %>% dplyr::select(quarter, gdp_growth),
  consumption_growth_seasonal_adjust %>% dplyr::select(quarter, consumption_growth),
  household_debt %>% dplyr::select(quarter, debt_growth),
  financial_balance_sheet %>% dplyr::select(quarter, asset_liability_ratio),
  household_indicators %>% dplyr::select(quarter, saving_rate, debt_income, interest_burden),
  real_house_prices,
  cpi_quarterly %>% dplyr::select(quarter, cpi_growth),
  policy_rate_quarterly,
  KIX92_quarterly %>% dplyr::select(quarter, exchange_rate_growth),
  export_growth_seasonal_adjust %>% dplyr::select(quarter, export_growth)
) %>%
  reduce(full_join, by = "quarter") %>%
  arrange(quarter) %>%
  # Filter out pre-1996 quarters and drop incomplete baseline lag rows
  filter(quarter >= "1996K1") %>%
  filter(complete.cases(.))

# Save master dataset
write_csv(macro_households, "1_Processed_Data/1c_Processed_Data_Set.csv")







