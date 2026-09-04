


# ------------------------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------------------------
scb_raw <- read_csv("0_Raw_Data/3_SCB_household_sector_indicators.csv")
bis_dsr <- read_csv("0_Raw_Data/10_Debt_Service_Ratio.csv")


# ------------------------------------------------------------------------------
# 2. Clean & Process SCB Data
# ------------------------------------------------------------------------------
col_names <- colnames(scb_raw)
val_col <- col_names[length(col_names)] # dynamic selection of last column

scb_clean <- scb_raw %>%
  rename(
    sector = 1,
    indicator = 2,
    quarter_raw = 3,
    value = all_of(val_col)
  ) %>%
  mutate(
    date = str_replace(quarter_raw, "K", "-Q"),
    # Replace Swedish decimal commas if present before converting to numeric
    value = as.numeric(str_replace(as.character(value), ",", "."))
  ) %>%
  # Filter explicitly for 'Households and NPISH' (or fallback to 'Households')
  # to prevent duplicate rows per quarter
  filter(
    sector %in% c("Households"),
    indicator %in% c(
      "Debt, per cent of disposable income, net, four quarter",
      "Interest payments, gross, as a percentage of disposable income, net"
    )
  ) %>%
  # If both sectors exist for a quarter
  group_by(date, indicator) %>%
  arrange(desc(sector)) %>% 
  slice(1) %>%
  ungroup() %>%
  # Reshape to wide format: 1 row per quarter
  pivot_wider(
    id_cols = date,
    names_from = indicator,
    values_from = value
  ) %>%
  rename(
    dti_ratio = `Debt, per cent of disposable income, net, four quarter`,
    interest_ratio = `Interest payments, gross, as a percentage of disposable income, net`
  ) %>%
  arrange(date)

# ------------------------------------------------------------------------------
# 3. Model DSR for Historical Quarters (BIS 18-Year Annuity Formula)
# ------------------------------------------------------------------------------
scb_dsr_calc <- scb_clean %>%
  filter(!is.na(dti_ratio) & !is.na(interest_ratio)) %>%
  mutate(
    # 1. Effective annual interest rate (r) decimal: (Interest / DTI)
    r_annual = interest_ratio / dti_ratio,
    
    # 2. Quarterly interest rate (i) decimal
    i_qtr = r_annual / 4,
    
    # 3. BIS Fixed Remaining Maturity s = 72 quarters (18 years)
    s = 72,
    
    # 4. Quarterly Annuity Factor
    annuity_factor = i_qtr / (1 - (1 + i_qtr)^(-s)),
    
    # 5. Model-estimated annual Debt Service Ratio (%)
    #    DSR = 4 * Quarterly Payment / Annual Income
    dsr_model = 4 * (dti_ratio / 100) * annuity_factor * 100
  )


  # ------------------------------------------------------------------------------
# 4. Level-Adjustment & Splice (1996 Q2 - 1998 Q4)
# ------------------------------------------------------------------------------
# Calculate splicing adjustment ratio at 1999-Q1 overlap
overlap_bis <- bis_dsr %>% filter(date == "1999-Q1") %>% pull(dsr_value)
overlap_model <- scb_dsr_calc %>% filter(date == "1999-Q1") %>% pull(dsr_model)

splice_factor <- overlap_bis / overlap_model

# Apply adjustment factor to pre-1999 model estimates
historical_spliced <- scb_dsr_calc %>%
  filter(date < "1999-Q1" & date >= "1996-Q2") %>%
  mutate(dsr_value = round(dsr_model * splice_factor, 1)) %>%
  select(date, dsr_value)

# Combine historical spliced data with official BIS data
final_dsr_series <- bind_rows(historical_spliced, bis_dsr) %>%
  arrange(date)

# ------------------------------------------------------------------------------
# 5. Export Complete Combined Series
# ------------------------------------------------------------------------------
write_csv(final_dsr_series, "1_Processed_Data/Data_Set_Columns/10b_Debt_Service_Ratio_1996_2026_Full.csv")
