dir.create("1_Processed_Data/Data_Set_Columns",
 recursive = TRUE,
 showWarnings = FALSE)

kix_gdp_raw <- read.csv("0_Raw_Data/8_A_OECD_KIX_real_gdp_growth_quarterly.csv")
kix_weights_format <- read.csv("0_Raw_Data/8_B_KIX_weights.csv")
china_rgdp <- read.csv("0_Raw_Data/8_C_FRED_china_real_GDP_annual.csv")


#============================================================================#
#            Dealing with missing obsvervation (Brazil and India)
#============================================================================#
# Check missing quarters
kix_gdp_raw %>%
 count(country) %>%
 arrange(n)

#1      CHN  60
#2      IND 118
#3      BRA 119

# I Filled backwards in period where trade with these 
# countries wasn't that large.

kix_gdp_raw_sourced  <- kix_gdp_raw  %>%
  mutate( source = "OECD"
  )


# Brazil: fill 1996-Q1 backwards from 1996-Q2
bra_fill <- kix_gdp_raw_sourced %>%
  filter(country == "BRA", quarter == "1996-Q2") %>%
  mutate(
    quarter = "1996-Q1",
    source = "OECD; backward-filled from 1996-Q2"
  )

# ------------------------------------------------------------------------------

# India: fill 1996-Q1 and 1996-Q2 backwards from 1996-Q3
ind_fill <- kix_gdp_raw_sourced %>%
  filter(country == "IND", quarter == "1996-Q3") %>%
  slice(rep(1, 2)) %>%
  mutate(
    quarter = c("1996-Q1", "1996-Q2"),
    source = "OECD; backward-filled from 1996-Q3"
  )


# ------------------------------------------------------------------------------

kix_gdp_Ind_Bra <- kix_gdp_raw_sourced %>%
  bind_rows(bra_fill, ind_fill) %>%
  arrange(country, quarter)

# Check how many are missing now!
#kix_gdp_Ind_Bra %>%
#  count(country) %>%
#  arrange(n)

#  country   n
#1      CHN  60

#============================================================================#
#            Dealing with missing obsvervation (China)
#============================================================================#
# China was the biggest problem. There is no clear Real GDP Growth or level
# from before 2012, unless if one wants to calculate it onself.
# Due to time constraint I felt it pertinent to splice linearly the Real GDP 
# at Constant National Prices for China (RGDPNACNA666NRUG)
# This is technically not entiterely correct, but was done for the sake of time
# in a period where China hadn't reached the high trading with Sweden Yet.
# The quarters after 2012 are the real OECD reports. I hade the option
# Between this or dropping China which I thought wasn't good considering
# how big they become!

china_rgdp <- china_rgdp %>%
  mutate(
    observation_date = as.Date(observation_date),
    year = as.integer(format(observation_date, "%Y"))
  ) %>%
  arrange(year) %>%
  mutate(
    GDP_Growth_Annual = 100 * (GDP_Real / lag(GDP_Real) - 1)
  )


china_quarters <- tibble(
  quarter_date = seq(
    from = as.Date("1996-01-01"),
    to   = as.Date("2010-10-01"),
    by   = "quarter"
  )
) %>%
  mutate(
    year = as.numeric(format(quarter_date, "%Y")),
    quarter = paste0(
      format(quarter_date, "%Y"),
      "-Q",
      ((as.integer(format(quarter_date, "%m")) - 1) %/% 3) + 1
    )
  )



# ------------------------------------------------------------------------------
# Select Missing Values

china_missing <- china_quarters %>%
  mutate(
    country = "CHN",

    # Match each quarter to its annual growth rate
    annual_growth = china_rgdp$GDP_Growth_Annual[
      match(year, china_rgdp$year)
    ],

    # Convert annual growth to equivalent quarterly growth
    gdp_growth_qoq = 100 * (
      (1 + annual_growth / 100)^(1/4) - 1
    ),

    source = "FRED annual GDP converted to quarterly"
  ) %>%
  select(
    country,
    quarter,
    gdp_growth_qoq,
    source
  )


# ------------------------------------------------------------------------------
# Select OECD Values

china_oecd <- kix_gdp_Ind_Bra %>%
   filter(country == "CHN") %>%
  transmute(
    country = country,
    quarter = quarter,
    gdp_growth_qoq = gdp_growth_qoq,
    source = source
  )

# ------------------------------------------------------------------------------
# Bind the two togheter


# ------------------------------------------------------------------------------
china_final <- bind_rows(
  china_missing,
  china_oecd
) %>%
  arrange(quarter)


kix_gdp_q <- kix_gdp_Ind_Bra %>%
  filter(country != "CHN") %>%
  bind_rows(china_final) %>%
  arrange(country, quarter)

  kix_gdp_q %>%
  count(country) %>%
  arrange(n)
# ------------------------------------------------------------------------------

# Write to CSV
write_csv(
  kix_gdp_q,
  "1_Processed_Data/Data_Set_Columns/8_a_Trading_Partners_gdp.csv"
)


#===============================================================================
# KIX-weighted quarterly GDP growth
# 1996-Q1 to 2025-Q4
#===============================================================================

kix_weights_format %>%
  select(country, X1996:X2000) %>%
  head()

#===============================================================================
# 1. KIX weights: country-name mapping
#===============================================================================

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


#===============================================================================
# 2. Convert KIX weights to long format
#===============================================================================

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
  select(country, country_code, year, weight)


#===============================================================================
# 3. Define euro-area countries
#
# From 2002 onward, the individual euro-area countries are replaced by
# the aggregate euro-area (EA) weight.
#===============================================================================

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


#===============================================================================
# 4. Select the appropriate KIX basket
#
# 1996-2001:
#   Individual country weights; exclude Russia and EA aggregate.
#
# 2002 onward:
#   Use EA aggregate; exclude individual euro-area countries and Russia.
#===============================================================================

kix_weights <- kix_weights_long %>%
  filter(
    country_code != "RUS",
    (
      year < 2002 & country_code != "EA"
    ) |
    (
      year >= 2002 &
        (!country %in% euro_countries | country_code == "EA")
    )
  )


#===============================================================================
# 5. Normalize weights to sum to 100 within each year
#===============================================================================

kix_weights <- kix_weights %>%
  group_by(year) %>%
  mutate(
    weight = weight / sum(weight, na.rm = TRUE) * 100
  ) %>%
  ungroup()


#===============================================================================
# 6. Check annual weight sums
#===============================================================================

kix_weights %>%
  group_by(year) %>%
  summarise(
    weight_sum = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )


#===============================================================================
# 7. Prepare quarterly GDP data
#
# Belgium-Luxembourg:
# OECD provides Belgium and Luxembourg separately, while the KIX weights
# contain a combined Belgium-Luxembourg entity. Belgian GDP growth is therefore
# used as a proxy for the combined entity.
# Let's compare Belgian and Luxembourg quarterly GDP growth

kix_gdp_q %>%
  filter(country %in% c("BEL", "LUX")) %>%
  group_by(quarter) %>%
  summarise(
    BEL = first(gdp_growth_qoq[country == "BEL"]),
    LUX = first(gdp_growth_qoq[country == "LUX"]),
    diff = LUX - BEL,
    .groups = "drop"
  ) %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff = max(abs(diff), na.rm = TRUE)
  )



#  mean_abs_diff max_abs_diff
#1          1.17         4.70
# A mean BEL–LUX growth difference of 1.17 percentage points
# is definitely not negligible in individual quarters, 
# but because Luxembourg is much smaller, using Belgian growth 
# as the proxy for the combined KIX entity is a reasonable 
# pragmatic approximation.
# I'll use Belgian GDP growth for BEL_LUX and remove Luxembourg

kix_gdp <- kix_gdp_q %>%
  mutate(
    country = if_else(country == "BEL", "BEL_LUX", country)
  ) %>%
  filter(country != "LUX") %>%
  mutate(
    year = as.integer(substr(quarter, 1, 4))
  )


#===============================================================================
# 8. Define the KIX quarterly basket
#
# This mirrors the weight selection:
#
# 1996-2001: individual countries, excluding EA.
# 2002 onward: EA aggregate replaces individual euro-area countries.
#===============================================================================

euro_kix <- c(
  "AUT", "BEL_LUX", "FIN", "FRA", "DEU",
  "GRC", "IRL", "ITA", "NLD", "PRT", "ESP"
)

kix_gdp_basket <- kix_gdp %>%
  filter(
    (
      year < 2002 &
        country != "EA"
    ) |
    (
      year >= 2002 &
        (country == "EA" | !country %in% euro_kix)
    )
  )


#===============================================================================
# 9. Merge quarterly GDP growth with annual KIX weights
#===============================================================================

kix_weighted_q <- kix_gdp_basket %>%
  left_join(
    kix_weights %>%
      select(country_code, year, weight),
    by = c(
      "country" = "country_code",
      "year" = "year"
    )
  )


#===============================================================================
# 10. Validate the merge
#===============================================================================

# Check for missing weights

missing_weights <- kix_weighted_q %>%
  filter(is.na(weight)) %>%
  distinct(country, year) %>%
  arrange(year, country)

missing_weights


# Check that weights sum to 100 in every quarter

weight_check <- kix_weighted_q %>%
  group_by(quarter) %>%
  summarise(
    weight_sum = sum(weight),
    .groups = "drop"
  )

weight_check %>%
  summarise(
    min_weight_sum = min(weight_sum),
    max_weight_sum = max(weight_sum)
  )


#===============================================================================
# 11. Calculate weighted GDP growth
#===============================================================================

kix_weighted_q <- kix_weighted_q %>%
  mutate(
    weighted_growth = weight * gdp_growth_qoq / 100
  )


#===============================================================================
# 12. Calculate KIX-weighted quarterly GDP growth
#===============================================================================

kix_growth <- kix_weighted_q %>%
  group_by(quarter) %>%
  summarise(
    KIX_GDP_growth_qoq = sum(weighted_growth),
    .groups = "drop"
  ) %>%
  arrange(quarter)


#===============================================================================
# 13. Final diagnostic summary
#===============================================================================

kix_growth %>%
  summarise(
    first_quarter = min(quarter),
    last_quarter = max(quarter),
    min_growth = min(KIX_GDP_growth_qoq),
    max_growth = max(KIX_GDP_growth_qoq),
    mean_growth = mean(KIX_GDP_growth_qoq)
  )


#===============================================================================
# 14. Optional: inspect the final series
#===============================================================================

kix_growth

write.csv(kix_growth, "1_Processed_Data/Data_Set_Columns/8_b_KIX_gdp.csv")