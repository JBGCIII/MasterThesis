

dir.create(
  "Processed_Data",
  showWarnings = FALSE
)

dir.create(
  "Processed_Data/Data_Set_Columns",
  showWarnings = FALSE
)


library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)



# ============================================================
# 1_real_gdp_growth
# ============================================================

# Load GDP
gdp <- read_csv(
  "Raw_Data/1_SCB_gdp_expenditures_real_quarterly.csv"
)

# Extract GDP at market prices
gdp_real <- gdp %>%
  filter(
    `type of use` == "- GDP at market prices"
  ) %>%
  select(
    quarter,
    `Constant prices, reference year 2025, SEK million`
  ) %>%
  rename(
    gdp_real_sek_million =
      `Constant prices, reference year 2025, SEK million`
  )

# Calculate growth
gdp_real <- gdp_real %>%
  mutate(
    gdp_growth =
      100 * (
        log(gdp_real_sek_million) -
        lag(log(gdp_real_sek_million))
      )
  )

# Save
write_csv(
  gdp_real,
  "Processed_Data/Data_Set_Columns/1b_real_gdp_growth.csv"
)


# ============================================================
# 2_household_consumption
# ============================================================

consumption <- read_csv(
  "Raw_Data/2_SCB_household_consumption_real_quarterly.csv"
)

household_consumption <- consumption %>%
  filter(
    purpose == "household total consumption expenditure"
  ) %>%
  select(
    quarter,
    `Constant prices reference year 2025, SEK million`
  ) %>%
  rename(
    consumption_real_sek_million =
      `Constant prices reference year 2025, SEK million`
  ) %>%
  arrange(quarter) %>%
  mutate(
    consumption_growth = 100 * (
      log(consumption_real_sek_million) -
      lag(log(consumption_real_sek_million))
    )
  )

write_csv(
  household_consumption,
  "Processed_Data/Data_Set_Columns/2b_household_consumption.csv"
)


# ============================================================
# 3_household_debt
# ============================================================

#There are two options here
#One is using Total Liabilities
#The other is to use Total Loans
#I am torn with regard to which of the two so I should used.


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
  "Processed_Data/Data_Set_Columns/4b_financial_asset_liability_ratio.csv"
)



# ============================================================
# 5_household_indicators
# ============================================================

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




# ============================================================
# 6_CPI quarterly inflation
# ============================================================

cpi <- read_csv(
  "Raw_Data/SCB_CPI_monthly.csv"
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
  "Processed_Data/Data_Set_Columns/6b_inflation_quarterly.csv"
)


# ============================================================
# 7b_real_house_price_growth
# ============================================================

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
  "Processed_Data/Data_Set_Columns/7b_real_house_price_growth.csv"
)


# ============================================================
# 8_policy_rate
# ============================================================

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
  "Processed_Data/Data_Set_Columns/8b_policy_rate_quarterly.csv"
)

# ============================================================
# Final Data Set
# ============================================================

macro_households <-

  list(
    gdp_real %>%
      select(
        quarter,
        gdp_growth
      ),

    household_consumption %>%
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