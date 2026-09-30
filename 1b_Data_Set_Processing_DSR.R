###############################################################################
############################## Debt Service Ratio #############################
###############################################################################

# ------------------------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------------------------
# 1.1 Read raw Statistics Sweden (SCB) household indicator CSV file
scb_raw <- read_csv("0_Raw_Data/3_SCB_household_sector_a_indicators.csv")

# 1.2 Read official BIS Debt Service Ratio CSV file
bis_dsr <- read_csv("0_Raw_Data/11_Debt_Service_Ratio.csv")

# ------------------------------------------------------------------------------
# 2. Clean & Process SCB Data
# ------------------------------------------------------------------------------
# 2.1 Dynamically target the last column name which holds the observation values
col_names <- colnames(scb_raw)
val_col <- col_names[length(col_names)] # dynamic selection of last column

# 2.2 Clean and transform raw SCB structure
scb_clean <- scb_raw %>%
  # Standardize column headers by index position
  rename(
    sector = 1,
    indicator = 2,
    quarter_raw = 3,
    value = all_of(val_col)
  ) %>%
  mutate(
    # Convert period format (e.g., "1996K1" -> "1996-Q1") and parse Swedish numeric format
    date = str_replace(quarter_raw, "K", "-Q"),
    value = as.numeric(str_replace(as.character(value), ",", "."))
  ) %>%
  # Keep only relevant household sector categories and target variables
  filter(
    sector %in% c("Households and NPISH", "Households"),
    indicator %in% c(
      "Debt, per cent of disposable income, net, four quarter",
      "Interest payments, gross, as a percentage of disposable income, net"
    )
  ) %>%
  # Resolve potential duplicate records across differing household definition labels
  group_by(date, indicator) %>%
  arrange(desc(sector)) %>% 
  slice(1) %>%
  ungroup() %>%
  # Transform data from long to wide format so variables become columns
  pivot_wider(
    id_cols = date,
    names_from = indicator,
    values_from = value
  ) %>%
  # Assign clean, working variable names
  rename(
    dti_ratio = `Debt, per cent of disposable income, net, four quarter`,
    interest_ratio = `Interest payments, gross, as a percentage of disposable income, net`
  ) %>%
  # Sort chronologically by quarter
  arrange(date)

# ------------------------------------------------------------------------------
# 3. Model DSR for Historical Quarters (BIS 18-Year Annuity Formula)
# ------------------------------------------------------------------------------
scb_dsr_calc <- scb_clean %>%
  # Ensure complete observations exist for both target variables
  filter(!is.na(dti_ratio) & !is.na(interest_ratio)) %>%
  mutate(
    # 3.1 Calculate implicit annual effective interest rate (r = Interest % / DTI %)
    r_annual = interest_ratio / dti_ratio,
    
    # 3.2 Convert annual interest rate to a quarterly decimal rate (i = r / 4)
    i_qtr = r_annual / 4,
    
    # 3.3 Set standard BIS fixed remaining maturity parameter (s = 72 quarters / 18 years)
    s = 72,
    
    # 3.4 Calculate quarterly annuity factor: [i / (1 - (1 + i)^(-s))]
    annuity_factor = i_qtr / (1 - (1 + i_qtr)^(-s)),
    
    # 3.5 Derive model-estimated annual Debt Service Ratio percentage (DSR)
    dsr_model = 4 * (dti_ratio / 100) * annuity_factor * 100
  )

# ------------------------------------------------------------------------------
# 4. Level-Adjustment & Splice (1996 Q2 - 1998 Q4)
# ------------------------------------------------------------------------------
# 4.1 Extract overlapping values at 1999-Q1 to construct a level-matching ratio
overlap_bis <- bis_dsr %>% filter(date == "1999-Q1") %>% pull(dsr_value)
overlap_model <- scb_dsr_calc %>% filter(date == "1999-Q1") %>% pull(dsr_model)

# 4.2 Compute level adjustment factor to match historical estimates to official BIS scale
splice_factor <- overlap_bis / overlap_model

# 4.3 Scale historical estimates (1996 Q1 to 1998 Q4) using the adjustment factor
historical_spliced <- scb_dsr_calc %>%
  filter(date < "1999-Q1" & date >= "1996-Q1") %>%
  mutate(dsr_value = round(dsr_model * splice_factor, 1)) %>%
  select(date, dsr_value)

# 4.4 Merge historical spliced estimates with official BIS series sequentially
final_dsr_series <- bind_rows(historical_spliced, bis_dsr) %>%
  arrange(date)

# ------------------------------------------------------------------------------
# 5. Format Quarter String
# ------------------------------------------------------------------------------
# 5.1 Standardize quarter identifier format (convert "1996-Q1" to "1996Q1")
debt_service_ratio <- final_dsr_series %>%
  rename(date = 1, dsr_value = 2) %>%
  mutate(quarter = gsub("-", "", date)) %>%
  dplyr::select(quarter, dsr_value)

# ------------------------------------------------------------------------------
# 6. Export Complete Combined Series
# ------------------------------------------------------------------------------
# 6.1 Save final processed Debt Service Ratio dataset to CSV
write_csv(
  debt_service_ratio, 
  "1_Processed_Data/Data_Set_Columns/3-5_A_debt_service_ratio.csv"
)

