###############################################################################
############################# 1.b DATA_SET_PROCESSING #########################
###############################################################################

library(tidyverse)
library(tsibble)

# Helper function to enforce clean YYYYQ# string format
to_yyyyq <- function(x) {
  x <- as.character(x)
  x <- gsub(" ", "", x)       # Remove spaces
  x <- gsub("-", "", x)       # Remove hyphens
  x <- gsub("K", "Q", x)      # Replace SCB 'K' with 'Q'
  return(x)
}

# ==============================================================================
# [1] SWE GDP (Preserving QoQ Growth, Log Diff, and Cumulative Log Level)
# ==============================================================================
gdp_sweden_log <- read_csv("0_Raw_Data/1_SCB_gdp_growth_quarterly.csv") %>%
  dplyr::select(quarter, gdp_se_qoq_pct = gdp_growth) %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  mutate(
    gdp_se_log_diff = log(1 + gdp_se_qoq_pct / 100),
    gdp_se_log      = log(100) + cumsum(coalesce(gdp_se_log_diff, 0))
  )

write_csv(gdp_sweden_log, "1_Processed_Data/Data_Set_Columns/1_gdp_sweden_log.csv")

# ==============================================================================
# [2] Consumption Categories (Preserving Raw Nominal/Constant SEK & Aggregates)
# ==============================================================================
target_categories <- c(
  "1110 varaktiga varor",
  "1130 icke varaktiga varor",
  "1120 delvis varaktiga varor",
  "1220 övriga tjänster"
)

consumption_processed <- read_csv("0_Raw_Data/2_SCB_household_consumption_categorized_quarterly.csv") %>%
  filter(varaktighet %in% target_categories) %>%
  rename(
    category       = varaktighet,
    quarter        = kvartal,
    value_constant = `Fasta priser, referensår 2025, mnkr`
  ) %>%
  mutate(
    category_group = case_when(
      category == "1110 varaktiga varor" ~ "cons_durable",
      category %in% c("1130 icke varaktiga varor", "1120 delvis varaktiga varor", "1220 övriga tjänster") ~ "cons_habitual"
    )
  ) %>%
  group_by(quarter, category_group) %>%
  summarise(value_constant = sum(value_constant, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = category_group, values_from = value_constant) %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  mutate(
    cons_total        = cons_durable + cons_habitual,
    cons_durable_log  = log(cons_durable),
    cons_habitual_log = log(cons_habitual),
    cons_total_log    = log(cons_total)
  )

write_csv(consumption_processed, "1_Processed_Data/Data_Set_Columns/2_consumption_categories_log.csv")

# ==============================================================================
# [4-5] Household Financials
# ==============================================================================
debt_to_real_asset     <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_B_debt_to_real_asset.csv") %>% mutate(quarter = to_yyyyq(quarter))
debt_service_ratio     <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_A_debt_service_ratio.csv") %>% rename_with(~ "quarter", 1) %>% mutate(quarter = to_yyyyq(quarter))
liquid_asset_to_income <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_C_liquid_asset_to_income_ratio.csv") %>% mutate(quarter = to_yyyyq(quarter))

household_indicators <- read_csv("0_Raw_Data/3_SCB_household_sector_a_indicators.csv") %>%
  rename(value = `Key indicators for income growth`) %>%
  filter(
    sector == "Households",
    indicator %in% c("Net saving ratio excl. pension funds reserves", "Debt, per cent of disposable income, net, four quarter")
  ) %>%
  rename_with(~ "quarter", matches("quarter|kvartal|period", ignore.case = TRUE)) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(
    saving_rate = `Net saving ratio excl. pension funds reserves`,
    debt_income = `Debt, per cent of disposable income, net, four quarter`
  ) %>%
  mutate(quarter = to_yyyyq(quarter))

write_csv(household_indicators, "1_Processed_Data/Data_Set_Columns/3-5_D_Savings_and_DTI.csv")

# ==============================================================================
# [9] Policy Rate
# ==============================================================================
policy_rate_quarterly <- read_csv("0_Raw_Data/9_Riksbank_policy_rate_daily.csv") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(value, .direction = "down") %>%
  mutate(quarter = paste0(format(date, "%Y"), "Q", ceiling(as.numeric(format(date, "%m")) / 3))) %>%
  group_by(quarter) %>%
  summarise(policy_rate = mean(value, na.rm = TRUE), .groups = "drop")

write_csv(policy_rate_quarterly, "1_Processed_Data/Data_Set_Columns/9_policy_rate.csv")

# ==============================================================================
# [7] Domestic Inflation (Preserving CPI Index, Log Level, QoQ & YoY)
# ==============================================================================
cpi_quarterly <- read_csv("0_Raw_Data/7_SCB_CPI_monthly.csv") %>%
  mutate(
    year         = substr(month, 1, 4),
    month_number = as.numeric(substr(month, 6, 7)),
    quarter      = paste0(year, "Q", ceiling(month_number / 3))
  ) %>%
  group_by(quarter) %>%
  summarise(cpi_index = mean(`CPI, Fixed Index numbers`, na.rm = TRUE), .groups = "drop") %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  mutate(
    cpi_log        = log(cpi_index),
    cpi_growth_qoq = 100 * (cpi_log - lag(cpi_log)),
    cpi_growth_yoy = 100 * (cpi_log - lag(cpi_log, 4))
  )

write_csv(cpi_quarterly, "1_Processed_Data/Data_Set_Columns/7_a_Inflation.csv")

# ==============================================================================
#                   [7] Foreign Inflation (KIX-Weighted-CPI)
# ==============================================================================
# KIX-weighted quarterly headline CPI inflation
# The input file contains quarterly KIX-weighted CPI inflation rates,
# not CPI index levels.
# I therefore must the reconstruct a synthetic KIX CPI log level by:
#   Δlog(CPI*_t) = log(1 + inflation*_t / 100)
# and cumulating the log differences.
# The resulting CPI index is normalized to 100 in the first observation.
kix_cpi_quarterly <- read_csv(
  "1_Processed_Data/Data_Set_Columns/7_KIX_CPI_inflation_qoq4.csv"
) %>%
  mutate(
    quarter = to_yyyyq(quarter),
    KIX_CPI_inflation_qoq = as.numeric(KIX_CPI_inflation_qoq)
  ) %>%
  arrange(quarter) %>%
  mutate(
    kix_cpi_log_diff = log(1 + KIX_CPI_inflation_qoq / 100),
    kix_cpi_log      = log(100) +
                       cumsum(coalesce(kix_cpi_log_diff, 0))
  ) %>%
  select(
    quarter,
    KIX_CPI_inflation_qoq,
    kix_cpi_log_diff,
    kix_cpi_log
  )

write_csv(
  kix_cpi_quarterly,
  "1_Processed_Data/Data_Set_Columns/7_b_foreign_inflation_log.csv"
)

# ==============================================================================
#                           [10] Real Exchange Rate
# ==============================================================================
# Exchange Rates
# Formula:
#   q_t = s_t + p*_t - p_t
# where:
#   s_t   = log nominal exchange rate
#   p*_t  = log foreign (KIX) CPI
#   p_t   = log Swedish CPI
# A rise in q therefore corresponds to a real depreciation of SEK
# under this exchange-rate convention.
kix_real_quarterly <- read_csv(
  "1_Processed_Data/Data_Set_Columns/10_kix_exchange_rates.csv"
) %>%
  mutate(
    quarter = to_yyyyq(quarter)
  ) %>%
  left_join(
    dplyr::select(
      cpi_quarterly,
      quarter,
      cpi_log
    ),
    by = "quarter"
  ) %>%
  left_join(
    dplyr::select(
      kix_cpi_quarterly,
      quarter,
      kix_cpi_log
    ),
    by = "quarter"
  ) %>%
  mutate(
    kix_real_log =
      kix_nominal_log +
      kix_cpi_log -
      cpi_log
  )

write_csv(
  kix_real_quarterly,
  "1_Processed_Data/Data_Set_Columns/10_kix_exchange_rates.csv"
)

# ==============================================================================
# [12-15] Labor, Energy, Global Rates, Confidence, Housing
# ==============================================================================
unemployment_quarterly <- read_csv("0_Raw_Data/12_FRED_Sweden_unemployment_rate_quarterly.csv") %>%
  rename(quarter_raw = observation_date, unemployment_rate = Unemployment_Rate) %>%
  mutate(quarter = to_yyyyq(quarter_raw))

write_csv(unemployment_quarterly, "1_Processed_Data/Data_Set_Columns/12_unemployment_rates.csv")

brent_quarterly <- read_csv("0_Raw_Data/13_FRED_Brent_crude_oil_price_quarterly.csv") %>%
  rename(quarter_raw = observation_date, brent_price = Brent_Price) %>%
  mutate(quarter = to_yyyyq(quarter_raw), oil_price_log = log(brent_price))

write_csv(brent_quarterly, "1_Processed_Data/Data_Set_Columns/13_Oil_log.csv")

fedfunds_quarterly <- read_csv("0_Raw_Data/14_FRED_US_federal_funds_rate_monthly.csv") %>%
  rename_with(~ "date_col", 1) %>%
  rename_with(~ "fed_rate", 2) %>%
  mutate(
    date_parsed = as.Date(date_col),
    quarter     = paste0(format(date_parsed, "%Y"), "Q", ceiling(as.numeric(format(date_parsed, "%m")) / 3))
  ) %>%
  group_by(quarter) %>%
  summarise(fed_funds_rate = mean(fed_rate, na.rm = TRUE), .groups = "drop")

write_csv(fedfunds_quarterly, "1_Processed_Data/Data_Set_Columns/14_fed_policy_rate.csv")

consumer_confidence_quarterly <- read_csv("0_Raw_Data/15_NIER_Consumer_Confidence.csv") %>%
  rename(indicator = Indikator, period = Period, value = `The Economic Tendency Indicator and other indicators`) %>%
  filter(str_detect(indicator, "Consumer confidence")) %>%
  mutate(quarter = paste0(substr(period, 1, 4), "Q", ceiling(as.numeric(substr(period, 6, 7)) / 3))) %>%
  group_by(quarter) %>%
  summarise(consumer_confidence = mean(value, na.rm = TRUE), .groups = "drop")

write_csv(consumer_confidence_quarterly, "1_Processed_Data/Data_Set_Columns/15_consumer_confidence.csv")

# House Prices (Preserving Index Level, Log Nominal, and Deflated Log Real)
real_house_price_quarterly <- read_csv("0_Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv") %>%
  filter(region == "Sweden") %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  dplyr::select(quarter, house_price_index = Index) %>%
  mutate(house_price_nominal_log = log(house_price_index)) %>%
  left_join(dplyr::select(cpi_quarterly, quarter, cpi_log), by = "quarter") %>%
  mutate(house_price_real_log = house_price_nominal_log - cpi_log)

write_csv(real_house_price_quarterly, "1_Processed_Data/Data_Set_Columns/6_real_house_price.csv")

# ==============================================================================
# Full Pre-Inspection Master Join (Retains Raw & Transformed Side-by-Side)
# ========================================================================
master_bsvar_data <- gdp_sweden_log %>%
  full_join(KIX_GDP_log_level, by = "quarter") %>%
  full_join(cpi_quarterly, by = "quarter") %>%
  full_join(kix_cpi_quarterly, by = "quarter") %>%
  full_join(policy_rate_quarterly, by = "quarter") %>%
  full_join(kix_real_quarterly, by = "quarter") %>%
  full_join(household_indicators, by = "quarter") %>%
  full_join(debt_to_real_asset, by = "quarter") %>%
  full_join(debt_service_ratio, by = "quarter") %>%
  full_join(liquid_asset_to_income, by = "quarter") %>%
  full_join(consumption_processed, by = "quarter") %>%
  full_join(unemployment_quarterly, by = "quarter") %>%
  full_join(brent_quarterly, by = "quarter") %>%
  full_join(fedfunds_quarterly, by = "quarter") %>%
  full_join(consumer_confidence_quarterly, by = "quarter") %>%
  full_join(real_house_price_quarterly, by = "quarter")

# Filter directly using standard string comparison
# (Since "YYYYQ#" formatted strings sort naturally in chronological order)
final_output <- master_bsvar_data %>%
  filter(quarter >= "1996Q1" & quarter <= "2024Q4") %>%
  arrange(quarter)

write_csv(final_output, "1_Processed_Data/1_a_pre_inspection_data.csv")

# 1. Load Pre-Inspection Output
pre_inspection_data <- read_csv("1_Processed_Data/1_a_pre_inspection_data.csv")


# 2. Extract and resolve duplicate CPI/HICP columns
inspection_data <- pre_inspection_data %>%
  transmute(
    quarter,
    
    # --- Global / Foreign Block (Exogenous) ---
    oil_price_log,                 # Brent Oil Price
    fed_funds_rate,                # US Federal Funds Rate
    kix_gdp_log,                   # Foreign GDP Aggregate (KIX Weighted)
    kix_cpi = kix_cpi_log.x,       # Foreign CPI (KIX Weighted)
    
    # --- Domestic Real Economy Block ---
    gdp_se_log,                    # Swedish Real GDP Level
    cons_total_log,                # Total Household Consumption
    cons_durable_log,              # Durable Consumption
    cons_habitual_log,             # Habitual Consumption
    unemployment_rate,             # Unemployment Rate
    consumer_confidence,           # NIER Consumer Confidence Indicator
    
    # --- Housing & Financial Balance Sheet Block ---
    house_price_real_log,          # Real House Price Index
    debt_income,                   # Household Debt to Income Ratio
    debt_to_asset_ratio,           # Debt to Real Asset Ratio
    dsr_value,                     # Debt Service Ratio
    liquid_asset_to_income_ratio,  # Liquid Assets to Income Ratio
    saving_rate,                   # Net Savings Rate
    
    # --- Domestic Nominal & Policy Block ---
    cpi_log = cpi_log.x,           # Domestic CPI Level
    policy_rate,                   # Riksbank Policy Rate
    kix_real_log                   # Real Effective Exchange Rate
  )

# 3. Export Clean Estimation Vector
write_csv(
  inspection_data, 
  "1d_pre_inspection_data_narrow.csv"
)
