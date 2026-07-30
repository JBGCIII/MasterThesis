

library(dplyr)
library(readr)
library(lubridate)

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
  "Processed_Data/bhousehold_debt.csv"
)











sector <- read_csv(
  "Raw_Data/3_SCB_household_sector_indicators.csv"
)

glimpse(sector)

unique(sector$indicator)




library(dplyr)
library(tidyr)

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
  "Processed_Data/household_indicators.csv"
)





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
  "Processed_Data/household_consumption.csv"
)












fa <- read_csv(
  "Raw_Data/4_SCB_household_financial_accounts_full_quarterly.csv"
)

glimpse(fa)

unique(fa$item)