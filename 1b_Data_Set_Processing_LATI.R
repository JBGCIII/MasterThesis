# ------------------------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------------------------

disp_inc <- read_csv("0_Raw_Data/3_SCB_household_sector_b_income_raw.csv")
ga <- read_csv("0_Raw_Data/5_SCB_household_financial_accounts_full_quarterly.csv")

# 1. Prepare Quarterly Disposable Income
inc_q <- disp_inc %>%
  dplyr::select(
    quarter, 
    disp_income = `Disposable income, S14, SEK million`
  ) %>%
  mutate(
    # Annualize quarterly flow to compare with end-of-quarter asset stock
    disp_income_annualized = disp_income * 4
  )

# 2. Extract Liquid Assets from Quarterly Financial Accounts (ga)
# Exclude illiquid assets (pensions, life insurance, noncorporate equity)
liquid_items <- c(
  "Currency", 
  "Transferable deposits", 
  "Other deposits",
  "Listed shares",
  "Investment fund shares or units"
)

liquid_assets_q <- ga %>%
  filter(item %in% liquid_items) %>%
  group_by(quarter) %>%
  summarize(
    liquid_assets = sum(Balances, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Merge Quarterly Series & Compute Ratio
liquid_asset_income_ratio <- liquid_assets_q %>%
  inner_join(inc_q, by = "quarter") %>%
  arrange(quarter) %>%
  mutate(
    # Liquid Assets as % of Annualized Quarterly Income
    liquid_asset_to_income_ratio = (liquid_assets / disp_income_annualized) * 100
  ) %>%
  dplyr::select(
    quarter,
    liquid_assets,
    disp_income_annualized,
    liquid_asset_to_income_ratio
  )


# 4. Save Processed Series
write_csv(
  liquid_asset_income_ratio,
  "1_Processed_Data/Data_Set_Columns/3-5_C_liquid_asset_to_income_ratio.csv"
)



