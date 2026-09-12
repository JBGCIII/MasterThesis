

#============================================================================#
#       [14] Riksbank KIX weights
#============================================================================#

# Extract Weights directly from XLSX file made available by Riksbank.

url_kix_weights <- paste0(
  "https://www.riksbank.se/globalassets/media/statistik/",
  "vikter/kix-vikter_sve2.xlsx"
)

# Create a temporary file
temp_kix <- tempfile(fileext = ".xlsx")
download.file(url_kix_weights, temp_kix, mode = "wb")

# Read the first sheet in the xlsx
kix_weights_raw <- read_excel(temp_kix, sheet = 1, col_names = FALSE)

# Years the weights refer to
kix_years <- as.numeric(kix_weights_raw[1, 2:33])
kix_years <- ifelse(
  kix_years > 30000,
  as.integer(format(as.Date(kix_years, origin = "1899-12-30"), "%Y")),
  kix_years
)

# Country names and weights
kix_weights <- kix_weights_raw[4:36, 1:33]

names(kix_weights)[1] <- "country"

names(kix_weights)[2:33] <- kix_years

write.csv( kix_weights, "0_Raw_Data/8_A_KIX_weights.csv")



#============================================================================#
#       [14] Riksbank KIX weights
#============================================================================#

  url_kix_gdp <- paste0(
  "https://sdmx.oecd.org/public/rest/data/",
  "OECD.SDD.NAD,",
  "DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.1/",
  "Q..PRT+AUS+RUS+BEL+LUX+BRA+DNK+FIN+FRA+GRC+IND+IRL+ISL+ITA+JPN+CAN+CHN+MEX+NLD+NOR+NZL+POL+CHE+SVK+ESP+GBR+KOR+CZE+TUR+DEU+HUN+USA+AUT+EA...B1GQ......G1.?",
  "startPeriod=1996-Q1&",
  "endPeriod=2025-Q4&",
  "dimensionAtObservation=AllDimensions&",
  "format=csvfilewithlabels"
)

kix_gdp_raw <- read_csv(url_kix_gdp)


kix_gdp_q <- kix_gdp_raw %>%
  transmute(
    country = REF_AREA,
    quarter = TIME_PERIOD,
    gdp_growth_qoq = OBS_VALUE
  ) %>%
  filter(country != "RUS") %>%
  arrange(country, quarter)

write_csv(
  kix_gdp_q,
  "0_Raw_Data/8_B_OECD_KIX_real_gdp_growth_quarterly.csv"
)


#============================================================================#
#       [12] FRED: China Annual Real GDP
#============================================================================#

url_china_rgdp <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=RGDPNACNA666NRUG"
)

china_rgdp <- read_csv(
  url_china_rgdp,
  show_col_types = FALSE
) %>%
  rename(
    GDP_Real = RGDPNACNA666NRUG
  ) %>%
  mutate(
    observation_date = as.Date(observation_date),
    year = as.integer(format(observation_date, "%Y"))
  ) %>%
  arrange(year)

write_csv(
  china_rgdp,
  "0_Raw_Data/8_C_FRED_china_real_GDP_annual.csv"
)



#============================================================================#
#       [12] FRED: China Annual Real GDP
#============================================================================#

url_china_rgdp <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=RGDPNACNA666NRUG"
)

china_rgdp <- read_csv(
  url_china_rgdp,
  show_col_types = FALSE
) %>%
  rename(
    GDP_Real = RGDPNACNA666NRUG
  ) %>%
  mutate(
    observation_date = as.Date(observation_date),
    year = as.integer(format(observation_date, "%Y"))
  ) %>%
  arrange(year) %>%
  mutate(
    GDP_Growth_Annual = 100 * (
      GDP_Real / lag(GDP_Real) - 1
    )
  )

write_csv(
  china_rgdp,
  "0_Raw_Data/8_C_FRED_china_real_GDP_annual.csv"
)








#============================================================================#
# [13] China Annual GDP Growth -> Quarterly
#============================================================================#

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


  china_missing %>%
  filter(quarter >= "2010-Q1")

#============================================================================#
#       Combine with OECD quarterly GDP growth
#============================================================================#

china_oecd <- kix_gdp_raw %>%
  filter(REF_AREA == "CHN") %>%
  transmute(
    country = REF_AREA,
    quarter = TIME_PERIOD,
    gdp_growth_qoq = OBS_VALUE,
    source = "OECD"
  )

china_final <- bind_rows(
  china_missing,
  china_oecd
) %>%
  arrange(quarter)


kix_gdp_q <- kix_gdp_q %>%
  filter(country != "CHN") %>%
  bind_rows(china_final) %>%
  arrange(country, quarter)


bra_fill <- kix_gdp_q %>%
  filter(country == "BRA", quarter == "1996-Q2") %>%
  mutate(
    quarter = "1996-Q1",
    source = "OECD; backward-filled from 1996-Q2"
  )

  # India: fill 1996-Q1 and 1996-Q2 backwards from 1996-Q3
ind_fill <- kix_gdp_q %>%
  filter(country == "IND", quarter == "1996-Q3") %>%
  slice(rep(1, 2)) %>%
  mutate(
    quarter = c("1996-Q1", "1996-Q2"),
    source = "OECD; backward-filled from 1996-Q3"
  )

  kix_gdp_q <- kix_gdp_q %>%
  bind_rows(bra_fill, ind_fill) %>%
  arrange(country, quarter)


