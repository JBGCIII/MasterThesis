###############################################################################
###################### Liquid Asset to Income Ratio ###########################
###############################################################################

# ------------------------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------------------------
# 1.1 Read raw SCB household disposable income quarterly dataset
disp_inc <- read_csv("0_Raw_Data/3_SCB_household_sector_b_income_raw.csv")

# 1.2 Read raw SCB household financial accounts quarterly dataset
ga <- read_csv("0_Raw_Data/5_SCB_household_financial_accounts_full_quarterly.csv")

# ------------------------------------------------------------------------------
# 2. Prepare Quarterly Disposable Income
# ------------------------------------------------------------------------------
inc_q <- disp_inc %>%
  # 2.1 Select target quarter identifier and raw disposable income column
  dplyr::select(
    quarter, 
    disp_income = `Disposable income, S14, SEK million`
  ) %>%
  mutate(
    # 2.2 Annualize quarterly flow (income x 4) to match end-of-quarter asset stock scale
    disp_income_annualized = disp_income * 4
  )

# ------------------------------------------------------------------------------
# 3. Extract Liquid Assets from Financial Accounts
# ------------------------------------------------------------------------------
# 3.1 Define core liquid asset categories (excludes illiquid items like pensions & equity)
liquid_items <- c(
  "Currency", 
  "Transferable deposits", 
  "Other deposits",
  "Listed shares",
  "Investment fund shares or units"
)

# 3.2 Filter financial accounts and calculate aggregate quarterly liquid asset balance
liquid_assets_q <- ga %>%
  filter(item %in% liquid_items) %>%
  group_by(quarter) %>%
  summarize(
    liquid_assets = sum(Balances, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. Merge Series & Compute Ratio
# ------------------------------------------------------------------------------
liquid_asset_income_ratio <- liquid_assets_q %>%
  # 4.1 Inner join liquid asset stocks with annualized disposable income by quarter
  inner_join(inc_q, by = "quarter") %>%
  # 4.2 Sort dataset chronologically
  arrange(quarter) %>%
  mutate(
    # 4.3 Compute liquid assets as percentage of annualized quarterly income
    liquid_asset_to_income_ratio = (liquid_assets / disp_income_annualized) * 100
  ) %>%
  # 4.4 Select final output columns
  dplyr::select(
    quarter,
    liquid_assets,
    disp_income_annualized,
    liquid_asset_to_income_ratio
  )

# ------------------------------------------------------------------------------
# 5. Save Processed Series
# ------------------------------------------------------------------------------
# 5.1 Export finalized liquid asset-to-income ratio dataset to CSV
write_csv(
  liquid_asset_income_ratio,
  "1_Processed_Data/Data_Set_Columns/3-5_C_liquid_asset_to_income_ratio.csv"
)
