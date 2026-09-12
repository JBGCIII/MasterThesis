

# Helper function to enforce clean YYYYQ# string format
to_yyyyq <- function(x) {
  x <- as.character(x)
  x <- gsub(" ", "", x)       # Remove spaces
  x <- gsub("-", "", x)       # Remove hyphens
  x <- gsub("K", "Q", x)      # Replace SCB 'K' with 'Q'
  return(x)
}



# ==============================================================================
#                        [1]   SWE GDP to Log Level 
# ==============================================================================
# [number] --> Denotes From 0_Raw_Data

gdp_sweden_log <- read_csv("0_Raw_Data/1_SCB_gdp_growth_quarterly.csv") %>%
  dplyr::select(quarter, gdp_se_qoq_pct = gdp_growth) %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  mutate(
    gdp_se_log_diff = log(1 + gdp_se_qoq_pct / 100),
    gdp_se_log      = log(100) + cumsum(coalesce(gdp_se_log_diff, 0))
  )


write_csv(gdp_sweden_log, 
  "1_Processed_Data/Data_Set_Columns/1_gdp_sweden_log.csv")


# ==============================================================================
#                       [8]   KIX-GDP to Log Level
# ==============================================================================

KIX_GDP_log_level <- read_csv("1_Processed_Data/Data_Set_Columns/8_b_KIX_gdp.csv") %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  mutate(
    # 1. Exact quarterly log difference (ln(1 + g/100))
    kix_gdp_log_diff = log(1 + KIX_GDP_growth_qoq / 100),
    
    # 2. Cumulative log index starting at ln(100)
    kix_gdp_log = log(100) + cumsum(coalesce(kix_gdp_log_diff, 0))
  )

write_csv(KIX_GDP_log_level, 
  "1_Processed_Data/Data_Set_Columns/8_C_kix_gdp_log.csv")




# ==============================================================================
#                           [2] Consumption Categories
# ==============================================================================


# 1. Load Raw Data
consumption_raw <- read_csv("0_Raw_Data/2_SCB_household_consumption_categorized_quarterly.csv")

# 2. Target Categories
target_categories <- c(
  "1110 varaktiga varor",
  "1130 icke varaktiga varor",
  "1120 delvis varaktiga varor",
  "1220 övriga tjänster"
)

# 3. Process, Aggregate Levels, and Take Log
consumption_processed <- consumption_raw %>%
  filter(varaktighet %in% target_categories) %>%
  rename(
    category       = varaktighet,
    quarter        = kvartal,
    value_constant = `Fasta priser, referensår 2025, mnkr`
  ) %>%
  mutate(
    # Classify into Durable vs. Habitual (Non-durable + Semi-durable + Services)
    category_group = case_when(
      category == "1110 varaktiga varor" ~ "cons_durable",
      category %in% c(
        "1130 icke varaktiga varor",
        "1120 delvis varaktiga varor",
        "1220 övriga tjänster"
      ) ~ "cons_habitual"
    )
  ) %>%
  # STEP 1: Sum raw constant SEK values by quarter and group FIRST
  group_by(quarter, category_group) %>%
  summarise(
    value_constant = sum(value_constant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # STEP 2: Pivot wide FIRST to keep raw monetary totals clear
  pivot_wider(
    names_from  = category_group,
    values_from = value_constant
  ) %>%
  # STEP 3: Standardize quarter date format
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  # STEP 4: Calculate log levels on aggregated SEK totals
  mutate(
    cons_durable_log  = log(cons_durable),
    cons_habitual_log = log(cons_habitual),
    # Optional: Log total consumption aggregate
    cons_total_log    = log(cons_durable + cons_habitual)
  )

# 4. Export safely
write_csv(
  consumption_processed, 
  "1_Processed_Data/Data_Set_Columns/2_consumption_categories_log.csv"
)


# ==============================================================================
#                         [4-5]  Household Financials
# ==============================================================================

debt_to_real_asset <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_B_debt_to_real_asset.csv") %>% 
  mutate(quarter = to_yyyyq(quarter))

debt_service_ratio <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_A_debt_service_ratio.csv") %>% 
  rename_with(~ "quarter", 1) %>%
  mutate(quarter = to_yyyyq(quarter))

liquid_asset_to_income <- read_csv("1_Processed_Data/Data_Set_Columns/3-5_C_liquid_asset_to_income_ratio.csv") %>% 
  mutate(quarter = to_yyyyq(quarter))


household_indicators <- read_csv("0_Raw_Data/3_SCB_household_sector_a_indicators.csv") %>%
  rename(value = `Key indicators for income growth`) %>%
  filter(
    sector == "Households",
    indicator %in% c(
      "Net saving ratio excl. pension funds reserves",
      "Debt, per cent of disposable income, net, four quarter"
    )
  ) %>%
  # Fix: Ensure quarter column is cleanly standardized prior to pivot
  rename_with(~ "quarter", matches("quarter|kvartal|period", ignore.case = TRUE)) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(
    saving_rate = `Net saving ratio excl. pension funds reserves`,
    debt_income = `Debt, per cent of disposable income, net, four quarter`
  ) %>%
  mutate(quarter = to_yyyyq(quarter)) %>%
  dplyr::select(quarter, saving_rate, debt_income)

  
write_csv(household_indicators, 
  "1_Processed_Data/Data_Set_Columns/3-5_D_Savings_and_DTI.csv")





#============================================================================#
#                            [9]   SWE POLICY RATE
#============================================================================#

policy_rate_quarterly <- read_csv("0_Raw_Data/9_Riksbank_policy_rate_daily.csv") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(value, .direction = "down") %>%
  mutate(quarter = paste0(format(date, "%Y"), "Q", ceiling(as.numeric(format(date, "%m")) / 3))) %>%
  group_by(quarter) %>%
  summarise(policy_rate = mean(value, na.rm = TRUE), .groups = "drop")


# 4. Export
write_csv(
  policy_rate_quarterly, 
  "1_Processed_Data/Data_Set_Columns/9_policy_rate.csv"
)


# ==============================================================================
#                          [7]  Domestic Inflation
# ==============================================================================

cpi_quarterly <- read_csv("0_Raw_Data/7_SCB_CPI_monthly.csv") %>%
  # 1. Clean monthly dates safely
  mutate(
    year         = substr(month, 1, 4),
    month_number = as.numeric(substr(month, 6, 7)),
    quarter      = paste0(year, "Q", ceiling(month_number / 3))
  ) %>%
  # 2. Average monthly price index across each quarter
  group_by(quarter) %>%
  summarise(
    # Note: If raw CSV contains CPIF, use `CPIF` column here.
    cpi_index = mean(`CPI, Fixed Index numbers`, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  # 3. Standardize quarter key to tsibble yyyyq format
  mutate(quarter = to_yyyyq(quarter)) %>%
  arrange(quarter) %>%
  # 4. Construct log levels and inflation metrics
  mutate(
    cpi_log        = log(cpi_index),
    cpi_growth_qoq = 100 * (cpi_log - lag(cpi_log)),      # Quarterly log inflation
    cpi_growth_yoy = 100 * (cpi_log - lag(cpi_log, 4))   # Annual log inflation
  )

# Export processed domestic inflation
write_csv(
  cpi_quarterly, 
  "1_Processed_Data/Data_Set_Columns/7_a_Inflation.csv"
)


# ==============================================================================
#                          [16 -> 7] Foreign Inflation
# ==============================================================================

ea_hicp_quarterly <- read_csv("0_Raw_Data/16_ECB_Euro_Area_HICP_monthly_raw.csv") %>%
  # Clean date: ensure obstime is converted to YYYYQx format
  mutate(
    # Handles strings like "1996-01", "1996M01", or numeric 199601
    obstime_str  = as.character(obstime),
    year         = substr(obstime_str, 1, 4),
    month_num    = as.numeric(gsub("[^0-9]", "", substr(obstime_str, 5, 7))),
    quarter_str  = paste0(year, "Q", ceiling(month_num / 3))
  ) %>%
  # Aggregate monthly observations to quarterly averages
  group_by(quarter_str) %>%
  summarise(
    ea_hicp_index = mean(as.numeric(obsvalue), na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(
    quarter     = to_yyyyq(quarter_str),
    ea_hicp_log = log(ea_hicp_index)
  ) %>%
  dplyr::select(quarter, ea_hicp_index, ea_hicp_log) %>%
  arrange(quarter)

write_csv(
  ea_hicp_quarterly, 
  "1_Processed_Data/Data_Set_Columns/7_b_foreign_inflation_log.csv"
)


# ==============================================================================
#                        [10]  KIX Exchange Rate (Quarterly & Log Real)
# ==============================================================================

kix_real_quarterly <- KIX92_quarterly %>%
  left_join(dplyr::select(cpi_quarterly, quarter, cpi_log), by = "quarter") %>%
  left_join(dplyr::select(ea_hicp_quarterly, quarter, ea_hicp_log), by = "quarter") %>%
  mutate(
    # Real Exchange Rate (Log Level): q_t = e_t + p_t - p*_t
    kix_real_log = kix_nominal_log + cpi_log - ea_hicp_log
  ) %>%
  dplyr::select(quarter, kix_index, kix_nominal_log, kix_real_log) %>%
  # Filter to start from 1996Q1 where HICP data is valid
  filter(!is.na(kix_real_log))

write_csv(
  kix_real_quarterly, 
  "1_Processed_Data/Data_Set_Columns/10_kix_exchange_rates.csv"
)



# ==============================================================================
#                                [12]  Unemployment
# ==============================================================================

unemployment_quarterly <- read_csv("0_Raw_Data/12_FRED_Sweden_unemployment_rate_quarterly.csv") %>%
  rename(quarter_raw = observation_date, unemployment_rate = Unemployment_Rate) %>%
  mutate(quarter = to_yyyyq(quarter_raw)) %>%
  dplyr::select(quarter, unemployment_rate)


write_csv(
  unemployment_quarterly, 
  "1_Processed_Data/Data_Set_Columns/12_unemployment_rates.csv"
)


# ==============================================================================
#                             [13]   Brent Oil
# ==============================================================================


brent_quarterly <- read_csv("0_Raw_Data/13_FRED_Brent_crude_oil_price_quarterly.csv") %>%
  rename(quarter_raw = observation_date, brent_price = Brent_Price) %>%
  mutate(
    quarter = to_yyyyq(quarter_raw),
    oil_price_log = log(brent_price)
  ) %>%
  dplyr::select(quarter, oil_price_log)

write_csv(
  brent_quarterly, 
  "1_Processed_Data/Data_Set_Columns/13_Oil_log.csv"
)



# ==============================================================================
#                             [14]   Fed Fund
# ==============================================================================


# K. Fed Funds Rate
fedfunds_quarterly <- read_csv("0_Raw_Data/14_FRED_US_federal_funds_rate_monthly.csv") %>%
  rename_with(~ "date_col", 1) %>%
  rename_with(~ "fed_rate", 2) %>%
  mutate(
    date_parsed = as.Date(date_col),
    quarter     = paste0(format(date_parsed, "%Y"), "Q", ceiling(as.numeric(format(date_parsed, "%m")) / 3))
  ) %>%
  group_by(quarter) %>%
  summarise(fed_funds_rate = mean(fed_rate, na.rm = TRUE), .groups = "drop")


write_csv(
  fedfunds_quarterly, 
  "1_Processed_Data/Data_Set_Columns/14_fed_policy_rate.csv"
)



# ==============================================================================
#                             [15]  Consumer Confidence
# ==============================================================================

consumer_confidence_quarterly <- read_csv("0_Raw_Data/15_NIER_Consumer_Confidence.csv") %>%
  rename(indicator = Indikator, period = Period, value = `The Economic Tendency Indicator and other indicators`) %>%
  filter(str_detect(indicator, "Consumer confidence")) %>%
  mutate(quarter = paste0(substr(period, 1, 4), "Q", ceiling(as.numeric(substr(period, 6, 7)) / 3))) %>%
  group_by(quarter) %>%
  summarise(consumer_confidence = mean(value, na.rm = TRUE), .groups = "drop")



write_csv(
  consumer_confidence_quarterly, 
  "1_Processed_Data/Data_Set_Columns/15_consumer_confidence.csv"
)




# ==============================================================================
#                              [6]  Real house price
# ==============================================================================

# ==============================================================================
#                            Real House Prices
# ==============================================================================

real_house_price_quarterly <- read_csv("0_Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv") %>%
  filter(region == "Sweden") %>%
  # 1. Standardize quarter index
  mutate(quarter = to_yyyyq(quarter)) %>%
  dplyr::select(quarter, house_price_index = Index) %>%
  # 2. Calculate Nominal Log
  mutate(house_price_nominal_log = log(house_price_index)) %>%
  # 3. Deflate using Domestic CPI
  left_join(dplyr::select(cpi_quarterly, quarter, cpi_log), by = "quarter") %>%
  # 4. Correct Log Real Price Formula: p_h,t - p_t
  mutate(house_price_real_log = house_price_nominal_log - cpi_log) %>%
  dplyr::select(quarter, house_price_index, house_price_nominal_log, house_price_real_log) %>%
  arrange(quarter)

# Export column
write_csv(
  real_house_price_quarterly, 
  "1_Processed_Data/Data_Set_Columns/6_real_house_price.csv"
)


# ==============================================================================
# 2. Final Clean Master Dataset Join
# ==============================================================================

master_bsvar_data <- gdp_sweden_log %>%
  dplyr::select(quarter, gdp_se_log) %>%
  full_join(dplyr::select(g7_log_level, quarter, g7_gdp_log), by = "quarter") %>%
  full_join(dplyr::select(cpi_quarterly, quarter, cpi_log, CPIF_Growth), by = "quarter") %>%
  full_join(policy_rate_quarterly, by = "quarter") %>%
  full_join(dplyr::select(KIX92_quarterly, quarter, kix_nominal_log, kix_real_proxy_log), by = "quarter") %>%
  full_join(household_indicators, by = "quarter") %>%
  full_join(debt_to_real_asset, by = "quarter") %>%
  full_join(debt_service_ratio, by = "quarter") %>%
  full_join(liquid_asset_to_income, by = "quarter") %>%
  full_join(consumption_processed, by = "quarter") %>%
  full_join(unemployment_quarterly, by = "quarter") %>%
  full_join(brent_quarterly, by = "quarter") %>%
  full_join(fedfunds_quarterly, by = "quarter") %>%
  full_join(consumer_confidence_quarterly, by = "quarter") %>%
  full_join(real_house_price_quarterly, by = "quarter") %>%
  arrange(quarter)

  final_output <- master_bsvar_data %>%
  filter(quarter >= "1996Q1" & quarter <= "2025Q4")

# Save Master Output
write_csv(final_output, "1_Processed_Data/1_a_Pre_Inspection_data.csv")