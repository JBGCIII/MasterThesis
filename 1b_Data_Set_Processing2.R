
#####################################1.DATA_SET_PROCESSING####################################
##                                                                                           #

# ==========================================0.Directory======================================#

# 0. Setup Directories
dir.create("Processed_Data/Data_Set_Columns", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. SCB Real Growth Rate (GDP & Consumption)
# ==============================================================================
gdp_growth_seasonal_adjust <- read_csv("Raw_Data/1_SCB_gdp_growth_quarterly.csv")
consumption_growth_seasonal_adjust <- read_csv("Raw_Data/2_SCB_household_consumption_real_quarterly.csv")

# ==============================================================================
# 3. Household Debt Growth ("Loans, total")
# ==============================================================================
fa <- read_csv("Raw_Data/4_SCB_household_financial_accounts_full_quarterly.csv")

household_debt <- fa %>%
  filter(
    item == "Loans, total",
    Balances > 80000
  ) %>%
  dplyr::select(quarter, Balances) %>%
  arrange(quarter) %>%
  mutate(
    debt_growth = 100 * (log(Balances) - lag(log(Balances)))
  )

write_csv(household_debt, "Processed_Data/Data_Set_Columns/3b_household_debt.csv")

# ==============================================================================
# 4. Asset-Liability Ratio (With 2001K1 Structural Break Fix)
# ==============================================================================
household_liab <- fa %>%
  filter(item == "Liabilities (FL)", Balances > 80000) %>%
  dplyr::select(quarter, Balances) %>%
  rename(liabilities = Balances)

financial_assets <- fa %>%
  filter(item == "Financial assets (FA)", Balances > 80000) %>%
  dplyr::select(quarter, Balances) %>%
  rename(assets = Balances)

# Calculate ratio at level shift
liab_2001K1 <- household_liab %>% filter(quarter == "2001K1") %>% pull(liabilities)
liab_2000K4 <- household_liab %>% filter(quarter == "2000K4") %>% pull(liabilities)
shift_factor <- liab_2001K1 / liab_2000K4  # ~1.1227

financial_balance_sheet <- financial_assets %>%
  left_join(household_liab, by = "quarter") %>%
  arrange(quarter) %>%
  mutate(
    liabilities_adj = if_else(
      quarter < "2001K1",
      liabilities * shift_factor, # Scale pre-2001 liabilities UP
      liabilities
    ),
    asset_liability_ratio = assets / liabilities_adj
  ) %>%
  dplyr::select(quarter, asset_liability_ratio)

write_csv(financial_balance_sheet, "Processed_Data/Data_Set_Columns/4b_financial_asset_liability_ratio.csv")

# ==============================================================================
# 5. Household Financial Indicators
# ==============================================================================
sector <- read_csv("Raw_Data/3_SCB_household_sector_indicators.csv")

household_indicators <- sector %>%
  rename(value = `Key indicators for income growth`) %>%
  filter(
    sector == "Households",
    indicator %in% c(
      "Net disposable income, real values, percentage growth, annual rate",
      "Net saving rate",
      "Debt, per cent of disposable income, net, four quarter",
      "Interest payments, gross, as a percentage of disposable income, net"
    )
  ) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(
    income_growth = `Net disposable income, real values, percentage growth, annual rate`,
    saving_rate = `Net saving rate`,
    debt_income = `Debt, per cent of disposable income, net, four quarter`,
    interest_burden = `Interest payments, gross, as a percentage of disposable income, net`
  )

write_csv(household_indicators, "Processed_Data/Data_Set_Columns/5b_household_indicators.csv")

# ==============================================================================
# 6. CPI Quarterly Inflation
# ==============================================================================
cpi <- read_csv("Raw_Data/5_SCB_CPI_monthly.csv")

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

write_csv(cpi_quarterly, "Processed_Data/Data_Set_Columns/6b_inflation_quarterly.csv")

# ==============================================================================
# 7. Real House Price Growth
# ==============================================================================
hpi <- read_csv("Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv")

real_house_prices <- hpi %>%
  filter(region == "Sweden") %>%
  dplyr::select(quarter, Index) %>%
  left_join(cpi_quarterly, by = "quarter") %>%
  mutate(
    nominal_house_price_growth = 100 * (log(Index) - lag(log(Index))),
    real_house_price_growth = nominal_house_price_growth - cpi_growth
  ) %>%
  dplyr::select(quarter, real_house_price_growth)

write_csv(real_house_prices, "Processed_Data/Data_Set_Columns/7b_real_house_price_growth.csv")

# ==============================================================================
# 8. Riksbank Policy Rate
# ==============================================================================
repo_rate <- read_csv("Raw_Data/7_Riksbank_policy_rate_daily.csv")

policy_rate_quarterly <- repo_rate %>%
  mutate(
    date = as.Date(date),
    year = format(date, "%Y"),
    month = as.numeric(format(date, "%m")),
    quarter = paste0(year, "K", ceiling(month / 3))
  ) %>%
  group_by(quarter) %>%
  summarise(
    policy_rate = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter)

write_csv(policy_rate_quarterly, "Processed_Data/Data_Set_Columns/8b_policy_rate_quarterly.csv")


# ==============================================================================
# Final Dataset Merge
# ==============================================================================
macro_households <- list(
  gdp_growth_seasonal_adjust %>% dplyr::select(quarter, gdp_growth),
  consumption_growth_seasonal_adjust %>% dplyr::select(quarter, consumption_growth),
  household_debt %>% dplyr::select(quarter, debt_growth),
  financial_balance_sheet %>% dplyr::select(quarter, asset_liability_ratio),
  household_indicators %>% dplyr::select(quarter, saving_rate, debt_income, interest_burden),
  real_house_prices,
  cpi_quarterly %>% dplyr::select(quarter, cpi_growth), # Added Inflation Here
  policy_rate_quarterly
) %>%
  reduce(full_join, by = "quarter") %>%
  arrange(quarter) %>%
  # Filter out pre-1996 quarters and drop incomplete baseline lag rows
  filter(quarter >= "1996K1") %>%
  filter(complete.cases(.))

# Save master dataset
write_csv(macro_households, "Processed_Data/1c_final_data.csv")