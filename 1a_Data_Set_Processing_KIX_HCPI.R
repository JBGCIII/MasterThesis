
###############################################################################
############################# 1.b KIX Weighted CPI ############################
###############################################################################

cpi_japan_end_2021Q2 <- read.csv("0_Raw_Data/8_B_HICP_Values.csv")
cpi_monthly_raw_jpn <- read.csv("0_Raw_Data_Static/CPI_JAPAN_FU.csv")
kix_weights_format <- read.csv("0_Raw_Data/8_D_KIX_weights.csv")
select <- dplyr::select

#============================================================================#
#            Dealing with missing obsvervation (Japan)
#============================================================================#
# Japan series is not readily available and must be spliced (くたばれ!!!!!)
# Source for Japanese Insult [https://www.sljfaq.org/afaq/insults.html]


# ------------------------------------------------------------------------------
# 1. Process Monthly Japan CPI Data into Quarterly Averages

cpi_quarterly_new <- cpi_monthly_raw_jpn %>%
  select(Year, Jan:Dec) %>%
  mutate(across(Jan:Dec, as.character)) %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "Month",
    values_to = "cpi_val"
  ) %>%
  filter(!is.na(cpi_val), !cpi_val %in% c("—", "-", "")) %>%
  mutate(
    cpi_val = as.numeric(cpi_val),
    month_num = match(Month, month.abb),
    quarter = as.yearqtr(paste(Year, month_num), format = "%Y %m")
  ) %>%
  group_by(quarter) %>%
  summarise(
    cpi_new_q = mean(cpi_val, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  filter(n_months == 3) %>%
  dplyr::select(quarter, cpi_new_q)  # <--- Explicit namespace call fixes masking

# ------------------------------------------------------------------------------
# 2. Extract Japan Series & Compute Splice Factor
japan_old_series <- cpi_japan_end_2021Q2 %>%
  filter(iso_code == "JPN") %>%
  pivot_longer(
    cols = -c(country, iso_code),
    names_to = "quarter_str",
    values_to = "value"
  ) %>%
  # Strip non-numeric characters (removes the 'X' prefix if present)
  mutate(
    clean_qtr_str = gsub("[^0-9]", "", quarter_str),
    year_part     = substr(clean_qtr_str, 1, 4),
    qtr_part      = substr(clean_qtr_str, 5, 5),
    quarter       = as.yearqtr(paste(year_part, "Q", qtr_part))
  ) %>%
  filter(!is.na(value), !is.na(quarter)) %>%
  arrange(quarter)

overlap_q <- as.yearqtr("2021 Q2")

# Extract overlapping values at 2021Q2
old_target_val <- japan_old_series %>%
  filter(quarter == overlap_q) %>%
  pull(value)

new_overlap_val <- cpi_quarterly_new %>%
  filter(quarter == overlap_q) %>%
  pull(cpi_new_q)

# Calculate splicing scale factor
splicing_factor <- old_target_val / new_overlap_val

# ------------------------------------------------------------------------------
# 3. Splice Forward, Cap at 2024Q4, & Combine
end_q <- as.yearqtr("2024 Q4")

japan_spliced_extension <- cpi_quarterly_new %>%
  filter(quarter > overlap_q, quarter <= end_q) %>%
  mutate(
    country = "Japan",
    iso_code = "JPN",
    value = cpi_new_q * splicing_factor
  ) %>%
  dplyr::select(country, iso_code, quarter, value)

japan_complete_series <- japan_old_series %>%
  filter(quarter <= overlap_q) %>%
  dplyr::select(country, iso_code, quarter, value) %>%
  bind_rows(japan_spliced_extension) %>%
  arrange(quarter)

# Save processed spliced CSV
write_csv(japan_complete_series, "1_Processed_Data/Data_Set_Columns/16_a_japan_cpi_spliced.csv")

# ------------------------------------------------------------------------------
# 4. Format Spliced Japan Series to Match Main Dataset Naming Style
# Detect if the original dataset uses an "X" prefix (e.g. X19954 vs 19954)
has_x_prefix <- any(grepl("^X[0-9]", names(cpi_japan_end_2021Q2)))
prefix <- if (has_x_prefix) "X" else ""

japan_spliced_wide <- japan_complete_series %>%
  mutate(
    quarter_str = paste0(prefix, format(quarter, "%Y"), cycle(quarter))
  ) %>%
  dplyr::select(country, iso_code, quarter_str, value) %>%
  pivot_wider(
    names_from = quarter_str,
    values_from = value
  )

# ------------------------------------------------------------------------------
# 5. Merge Back into Full Country Dataset
cpi_japan_spliced_all <- cpi_japan_end_2021Q2 %>%
  # Remove old truncated Japan row
  filter(iso_code != "JPN") %>%
  # Append the newly updated Japan row
  bind_rows(japan_spliced_wide)

# Standardize column ordering: country, iso_code, then time columns in order
quarter_cols <- setdiff(names(cpi_japan_spliced_all), c("country", "iso_code"))

cpi_japan_spliced_all <- cpi_japan_spliced_all %>%
  dplyr::select(country, iso_code, all_of(sort(quarter_cols)))

# ------------------------------------------------------------------------------
# 6. Export Updated Master Dataset
write_csv(cpi_japan_spliced_all, "1_Processed_Data/Data_Set_Columns/16_A_cpi_merged.csv")




#============================================================================#
#                       Calculating Weighted CPI
#============================================================================#

#1. Load Weights again
kix_weights_format %>%
  select(country, X1996:X2000) %>%
  head()

# ------------------------------------------------------------------------------

kix_country_map <- c(
  "Australien"         = "AUS",
"Belgien-Luxemburg"  = "BEL_LUX",
  "Brasilien"          = "BRA",
  "Danmark"            = "DNK",
  "Finland"            = "FIN",
  "Frankrike"          = "FRA",
  "Grekland"           = "GRC",
  "Indien"              = "IND",
  "Irland"              = "IRL",
  "Island"              = "ISL",
  "Italien"             = "ITA",
  "Japan"               = "JPN",
  "Kanada"              = "CAN",
  "Kina"                = "CHN",
  "Mexiko"              = "MEX",
  "Nederländerna"       = "NLD",
  "Norge"               = "NOR",
  "Nya Zeeland"         = "NZL",
  "Polen"               = "POL",
  "Portugal"            = "PRT",
  "Ryssland"            = "RUS",
  "Schweiz"             = "CHE",
  "Slovakien"           = "SVK",
  "Spanien"             = "ESP",
  "Storbritannien"      = "GBR",
  "Sydkorea"            = "KOR",
  "Tjeckien"            = "CZE",
  "Turkiet"             = "TUR",
  "Tyskland"            = "DEU",
  "Ungern"              = "HUN",
  "USA"                 = "USA",
  "Österrike"           = "AUT",
  "Euroområdet**"       = "EA"
)

# ------------------------------------------------------------------------------

# 3. Format the Thing
hicp_long <- cpi_japan_spliced_all %>%
  pivot_longer(
    cols = -c(country, iso_code),
    names_to = "quarter",
    values_to = "cpi_index"
  ) %>%
  mutate(
    quarter = sub("^X", "", quarter),
    year = as.integer(substr(quarter, 1, 4)),
    q = as.integer(substr(quarter, 5, 5)),
    quarter = paste0(year, "-Q", q)
  ) %>%
  select(country, iso_code, quarter, year, cpi_index)

# ------------------------------------------------------------------------------

# 4. Calculate inflation
hicp_q <- hicp_long %>%
  group_by(country, iso_code) %>%
  arrange(quarter) %>%
  mutate(
    cpi_inflation_qoq = 100 * (cpi_index / lag(cpi_index) - 1)
  ) %>%
  ungroup() %>%
  filter(
    year >= 1996,
    year <= 2024
  )


# ------------------------------------------------------------------------------
hicp_long <- hicp_format %>%
  group_by(iso_code) %>%
  mutate(series = row_number()) %>%
  ungroup() %>%
  pivot_longer(
    cols = -c(country, iso_code, series),
    names_to = "quarter",
    values_to = "cpi_index"
  ) %>%
  mutate(
    quarter = sub("^X", "", quarter),
    year = as.integer(substr(quarter, 1, 4)),
    q = as.integer(substr(quarter, 5, 5)),
    quarter = paste0(year, "-Q", q)
  ) %>%
  select(country, iso_code, series, quarter, year, cpi_index)


# ------------------------------------------------------------------------------
hicp_inflation <- hicp_long %>%
  group_by(iso_code, series) %>%
  arrange(quarter, .by_group = TRUE) %>%
  mutate(
    cpi_inflation_qoq = 100 * (cpi_index / lag(cpi_index) - 1)
  ) %>%
  ungroup() %>%
  filter(year >= 1996, year <= 2024)



# ------------------------------------------------------------------------------
  hicp_q <- hicp_inflation %>%
  group_by(iso_code, quarter, year) %>%
  summarise(
    cpi_inflation_qoq = mean(cpi_inflation_qoq, na.rm = TRUE),
    .groups = "drop"
  )



# ------------------------------------------------------------------------------
  euro_kix <- c(
  "AUT", "BEL_LUX", "FIN", "FRA", "DEU",
  "GRC", "IRL", "ITA", "NLD", "PRT", "ESP"
)

# ------------------------------------------------------------------------------

kix_cpi_basket <- hicp_q %>%
  filter(
    iso_code != "RUS",
    (
      year < 2002 & iso_code != "EA"
    ) |
    (
      year >= 2002 &
        (
          iso_code == "EA" |
          !iso_code %in% euro_kix
        )
    )
  )


kix_cpi_weighted_q <- kix_cpi_weighted_q %>%
  mutate(
    weighted_inflation =
      weight * cpi_inflation_qoq / 100
  )

kix_cpi_inflation <- kix_cpi_weighted_q %>%
  group_by(quarter) %>%
  summarise(
    KIX_CPI_inflation_qoq = sum(weighted_inflation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter)



# ============================================================
# KIX-WEIGHTED QUARTERLY HEADLINE CPI INFLATION
# Sample: 1996Q1–2024Q4
# ============================================================



# Country name -> ISO code mapping
kix_country_map <- c(
  "Australien"         = "AUS",
  "Belgien-Luxemburg"  = "BEL_LUX",
  "Brasilien"          = "BRA",
  "Danmark"            = "DNK",
  "Finland"            = "FIN",
  "Frankrike"          = "FRA",
  "Grekland"            = "GRC",
  "Indien"              = "IND",
  "Irland"              = "IRL",
  "Island"              = "ISL",
  "Italien"             = "ITA",
  "Japan"               = "JPN",
  "Kanada"              = "CAN",
  "Kina"                = "CHN",
  "Mexiko"              = "MEX",
  "Nederländerna"       = "NLD",
  "Norge"               = "NOR",
  "Nya Zeeland"         = "NZL",
  "Polen"               = "POL",
  "Portugal"            = "PRT",
  "Ryssland"            = "RUS",
  "Schweiz"              = "CHE",
  "Slovakien"            = "SVK",
  "Spanien"              = "ESP",
  "Storbritannien"       = "GBR",
  "Sydkorea"             = "KOR",
  "Tjeckien"             = "CZE",
  "Turkiet"              = "TUR",
  "Tyskland"             = "DEU",
  "Ungern"               = "HUN",
  "USA"                  = "USA",
  "Österrike"             = "AUT",
  "Euroområdet**"         = "EA"
)

# Reshape annual weights to long format
kix_weights_long <- kix_weights_format %>%
  mutate(
    country_code = recode(country, !!!kix_country_map)
  ) %>%
  pivot_longer(
    cols = starts_with("X19") | starts_with("X20"),
    names_to = "year",
    values_to = "weight"
  ) %>%
  mutate(
    year = as.integer(sub("^X", "", year))
  ) %>%
  select(
    country,
    country_code,
    year,
    weight
  )

# ------------------------------------------------------------
# 2. Define euro-area countries in the KIX basket
# ------------------------------------------------------------

euro_countries <- c(
  "Belgien-Luxemburg",
  "Finland",
  "Frankrike",
  "Grekland",
  "Irland",
  "Italien",
  "Nederländerna",
  "Portugal",
  "Spanien",
  "Tyskland",
  "Österrike"
)

# Exclude Russia and switch from individual euro countries
# to the Euro Area aggregate from 2002 onward
kix_weights <- kix_weights_long %>%
  filter(
    country_code != "RUS",
    (
      year < 2002 &
        country_code != "EA"
    ) |
    (
      year >= 2002 &
        (
          !country %in% euro_countries |
          country_code == "EA"
        )
    )
  )

# Normalize weights to sum to 100 in each year
kix_weights <- kix_weights %>%
  group_by(year) %>%
  mutate(
    weight = weight / sum(weight, na.rm = TRUE) * 100
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 3. Load headline CPI index data
# ------------------------------------------------------------

hicp_format <- read.csv(
  "1_Processed_Data/Data_Set_Columns/16_A_CPI.csv"
)

# ------------------------------------------------------------
# 4. Reshape CPI data to quarterly long format
# ------------------------------------------------------------

# BEL_LUX appears as two component series.
# Create a series identifier before calculating inflation
# so that the lag() operation never crosses between them.

hicp_long <- hicp_format %>%
  group_by(iso_code) %>%
  mutate(
    series = row_number()
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = -c(country, iso_code, series),
    names_to = "quarter_raw",
    values_to = "cpi_index"
  ) %>%
  mutate(
    quarter_raw = sub("^X", "", quarter_raw),
    year = as.integer(substr(quarter_raw, 1, 4)),
    q = as.integer(substr(quarter_raw, 5, 5)),
    quarter = paste0(year, "-Q", q)
  ) %>%
  select(
    country,
    iso_code,
    series,
    quarter,
    year,
    cpi_index
  )


# ------------------------------------------------------------
# 5. Calculate quarterly headline CPI inflation
# ------------------------------------------------------------

# QoQ inflation:
#
#   inflation_t = 100 * (CPI_t / CPI_(t-1) - 1)
#
# 1995Q4 is retained in the underlying data because it is
# needed to calculate inflation for 1996Q1.

hicp_inflation <- hicp_long %>%
  group_by(iso_code, series) %>%
  arrange(quarter, .by_group = TRUE) %>%
  mutate(
    cpi_inflation_qoq =
      100 * (cpi_index / lag(cpi_index) - 1)
  ) %>%
  ungroup() %>%
  filter(
    year >= 1996,
    year <= 2024
  )


# ------------------------------------------------------------
# 6. Collapse BEL_LUX to one series
# ------------------------------------------------------------

# BEL_LUX contains two component CPI series.
# Calculate their inflation separately and then take their
# equal-weighted average.
#
# This is preferable to averaging the CPI index levels first,
# because the object being aggregated is the inflation rate.
#
# For all other countries there is only one component series.

hicp_q <- hicp_inflation %>%
  group_by(
    iso_code,
    quarter,
    year
  ) %>%
  summarise(
    country = first(country),
    cpi_inflation_qoq =
      mean(cpi_inflation_qoq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(
    country,
    iso_code,
    quarter,
    year,
    cpi_inflation_qoq
  )


# ------------------------------------------------------------
# 7. Define the KIX euro-area basket
# ------------------------------------------------------------

euro_kix <- c(
  "AUT",
  "BEL_LUX",
  "FIN",
  "FRA",
  "DEU",
  "GRC",
  "IRL",
  "ITA",
  "NLD",
  "PRT",
  "ESP"
)


# ------------------------------------------------------------
# 8. Construct the KIX CPI basket
# ------------------------------------------------------------

# 1996–2001:
#   Individual euro-area countries are included.
#   Euro Area aggregate is excluded.
#
# 2002 onward:
#   Euro Area aggregate is included.
#   Individual euro-area countries are excluded.
#
# Russia is excluded throughout.

kix_cpi_basket <- hicp_q %>%
  filter(
    iso_code != "RUS",
    (
      year < 2002 &
        iso_code != "EA"
    ) |
    (
      year >= 2002 &
        (
          iso_code == "EA" |
          !iso_code %in% euro_kix
        )
    )
  )


# ------------------------------------------------------------
# 9. Check that the required KIX entities are present
# ------------------------------------------------------------

kix_cpi_basket %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(iso_code),
    .groups = "drop"
  ) %>%
  print(n = Inf)


# ------------------------------------------------------------
# 10. Merge CPI inflation with KIX weights
# ------------------------------------------------------------

kix_cpi_weighted_q <- kix_cpi_basket %>%
  left_join(
    kix_weights %>%
      select(
        country_code,
        year,
        weight
      ),
    by = c(
      "iso_code" = "country_code",
      "year" = "year"
    )
  )


# ------------------------------------------------------------
# 11. Check for missing KIX weights
# ------------------------------------------------------------

missing_weights <- kix_cpi_weighted_q %>%
  filter(is.na(weight)) %>%
  distinct(
    iso_code,
    year
  )

print(missing_weights)


# ------------------------------------------------------------
# 12. Check that KIX weights sum to 100
# ------------------------------------------------------------

weight_check <- kix_cpi_weighted_q %>%
  group_by(quarter) %>%
  summarise(
    weight_sum = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

print(
  weight_check %>%
    summarise(
      min_weight_sum = min(weight_sum),
      max_weight_sum = max(weight_sum)
    )
)


# ------------------------------------------------------------
# 13. Calculate weighted CPI inflation
# ------------------------------------------------------------

kix_cpi_weighted_q <- kix_cpi_weighted_q %>%
  mutate(
    weighted_inflation =
      weight * cpi_inflation_qoq / 100
  )


# ------------------------------------------------------------
# 14. Construct final KIX-weighted CPI inflation series
# ------------------------------------------------------------

kix_cpi_inflation <- kix_cpi_weighted_q %>%
  group_by(quarter) %>%
  summarise(
    KIX_CPI_inflation_qoq =
      sum(weighted_inflation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter)


# ------------------------------------------------------------
# 15. Final checks
# ------------------------------------------------------------

# Number of observations
nrow(kix_cpi_inflation)

# First and last observations
head(kix_cpi_inflation)
tail(kix_cpi_inflation)

# Summary statistics
summary(
  kix_cpi_inflation$KIX_CPI_inflation_qoq
)

# Check for missing final values
sum(
  is.na(kix_cpi_inflation$KIX_CPI_inflation_qoq)
)


# ------------------------------------------------------------
# 16. Optional: save final KIX CPI inflation series
# ------------------------------------------------------------

# Theoretically I could have kept it in levels but I don't want to break things!

write.csv(
  kix_cpi_inflation,
  "1_Processed_Data/Data_Set_Columns/7_KIX_CPI_inflation_qoq4.csv",
  row.names = FALSE
)

