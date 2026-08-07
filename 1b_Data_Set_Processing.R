
#####################################1.DATA_SET_PROCESSING####################################
##                                                                                           #

# ==========================================0.Directory======================================#


dir.create(
  "Processed_Data",
  showWarnings = FALSE
)

dir.create(
  "Processed_Data/Data_Set_Columns",
  showWarnings = FALSE
)



# ==========================================================================================#
#                   1_SCB_Real_Growth_Rate (GDP & Consumption) Seasonal_Adjusted            #

# Load GDP and Consumption Growth (Already Transformed by SCB. No need to process)
gdp_growth_seasonal_adjust <- read_csv("Raw_Data/1_SCB_gdp_growth_quarterly.csv")
consumption_growth_seasonal_adjust <- read_csv("Raw_Data/2_SCB_household_consumption_real_quarterly.csv"
)


# ==========================================================================================#
#                                       3_household_debt_growth                             #

#There are two options here
#(1) Using Total Liabilities
#(2) Use Total Loans
#I am torn with regard to which of the two so I should used.
#Using total loans will be more in line with Svenson, while
#Total liabilities will help ascertain how the recent growth of
#consumer credit might shake the economy.


fa <- read_csv(
  "Raw_Data/4_SCB_household_financial_accounts_full_quarterly.csv"
)


household_debt <- fa %>%
  filter(
    item == "Liabilities (FL)"
  )

household_debt <- household_debt %>%
  select(
    quarter,
    Balances
  ) %>%
  rename(
    debt_sek_million = Balances
  )

  household_debt <- household_debt %>%
  mutate(
    debt_growth = 100 * (
      log(debt_sek_million) -
      lag(log(debt_sek_million))
    )
  )


  write_csv(
  household_debt,
  "Processed_Data/Data_Set_Columns/3b_household_debt.csv"
)


# ==========================================================================================#
#                                   4_Asset_liability_ratio                                                 

# Extract assets and liabilities
financial_balance_sheet <- fa %>%
  filter(
    item %in% c(
      "Financial assets (FA)",
      "Liabilities (FL)"
    )
  ) %>%
  select(
    quarter,
    item,
    Balances
  )

# Reshape
financial_balance_sheet <- financial_balance_sheet %>%
  pivot_wider(
    names_from = item,
    values_from = Balances
  )

# Calculate financial asset/liability ratio
financial_asset_ratio <- financial_balance_sheet %>%
  mutate(
    asset_liability_ratio =
      `Financial assets (FA)` /
      `Liabilities (FL)`
  ) %>%
  select(
    quarter,
    asset_liability_ratio
  )

# Save
write_csv(
  financial_asset_ratio,
  "Processed_Data/Data_Set_Columns/4_financial_asset_liability_ratio.csv"
)



# ==========================================================================================#
#                                  5_Asset_liability_ratio                                                 


sector <- read_csv(
  "Raw_Data/3_SCB_household_sector_indicators.csv"
)

household_indicators <- sector %>%
  rename(
    value = `Key indicators for income growth`
  ) %>%
  filter(
    sector == "Households",
    indicator %in% c(
      "Net disposable income, real values, percentage growth, annual rate",
      "Net saving rate",
      "Debt, per cent of disposable income, net, four quarter",
      "Interest payments, gross, as a percentage of disposable income, net"
    )
  ) %>%
  pivot_wider(
    names_from = indicator,
    values_from = value
  )


  household_indicators <- household_indicators %>%
  rename(
    income_growth = `Net disposable income, real values, percentage growth, annual rate`,
    saving_rate = `Net saving rate`,
    debt_income = `Debt, per cent of disposable income, net, four quarter`,
    interest_burden = `Interest payments, gross, as a percentage of disposable income, net`
  )

  write_csv(
  household_indicators,
  "Processed_Data/Data_Set_Columns/5b_household_indicators.csv"
)



# ==========================================================================================#
#                                  6_CPI quarterly inflation                                                

cpi <- read_csv(
  "Raw_Data/5_SCB_CPI_monthly.csv"
)

cpi_quarterly <- cpi %>%
  mutate(
    year = substr(month,1,4),
    month_number = as.numeric(substr(month,6,7)),
    quarter = paste0(
      year,
      "K",
      ceiling(month_number/3)
    )
  ) %>%
  group_by(quarter) %>%
  summarise(
    cpi_index = mean(
      `CPI, Fixed Index numbers`,
      na.rm = TRUE
    )
  ) %>%
  arrange(quarter)


cpi_quarterly <- cpi_quarterly %>%
  mutate(
    cpi_growth =
      100 * (
        log(cpi_index) -
        lag(log(cpi_index))
      )
  )

write_csv(
  cpi_quarterly,
  "Processed_Data/Data_Set_Columns/6_inflation_quarterly.csv"
)


# ==========================================================================================#
#                                  7_real_house_price_growth                                               

hpi <- read_csv(
  "Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv"
)

# Keep Sweden only
hpi_sweden <- hpi %>%
  filter(
    region == "Sweden"
  ) %>%
  select(
    quarter,
    Index
  )


# Calculate quarterly growth
hpi_sweden <- hpi_sweden %>%
  mutate(
    house_price_growth =
      100 * (
        log(Index) -
        lag(log(Index))
      )
  )

real_house_prices <- hpi_sweden %>%
  left_join(
    cpi_quarterly,
    by="quarter"
  ) %>%
  mutate(
    nominal_house_price_growth =
      100 * (
        log(Index) -
        lag(log(Index))
      ),

    real_house_price_growth =
      nominal_house_price_growth -
      cpi_growth
  ) %>%
  select(
    quarter,
    real_house_price_growth
  )


write_csv(
  real_house_prices,
  "Processed_Data/Data_Set_Columns/7_real_house_price_growth.csv"
)



# ==========================================================================================#
#                                  7_Policy_Rate                                             


repo_rate <- read_csv(
  "Raw_Data/7_Riksbank_policy_rate_daily.csv"
)

policy_rate_quarterly <- repo_rate %>%
  mutate(
    date = as.Date(date),
    year = format(date, "%Y"),
    month = as.numeric(format(date, "%m")),
    quarter = paste0(
      year,
      "K",
      ceiling(month/3)
    )
  ) %>%
  group_by(quarter) %>%
  summarise(
    policy_rate =
      mean(value, na.rm = TRUE)
  ) %>%
  arrange(quarter)


write_csv(
  policy_rate_quarterly,
  "Processed_Data/Data_Set_Columns/8_policy_rate_quarterly.csv"
)


# ==========================================================================================#
#                                       Final_Data_Set           
# ==========================================================================================#

macro_households <-

  list(
    gdp_growth_seasonal_adjust %>%
    select(
      quarter,
      gdp_growth
    ),

  consumption_growth_seasonal_adjust %>%
    select(
      quarter,
      consumption_growth
    ),

    household_debt %>%
      select(
        quarter,
        debt_growth
      ),

    financial_asset_ratio,

    household_indicators %>%
      select(
        quarter,
        saving_rate,
        debt_income,
        interest_burden
      ),

    real_house_prices,

    policy_rate_quarterly

  ) %>%

  reduce(
    full_join,
    by = "quarter"
  ) %>%

  arrange(quarter)

  macro_households <- macro_households %>%
  filter(
    complete.cases(.)
  )

write_csv(
  macro_households,
  "Processed_Data/1c_final_data.csv"
)



