###############################################################################
############################# 1.a DATA_SET_CREATION ###########################
###############################################################################

dir.create( "0_Raw_Data", showWarnings = FALSE) # Create Directory
# Reminder to myself to change all Raw_Data to 0_Raw_Data at once.

#============================================================================#
#                  [1] SCB: GDP Expenditures Real Quarterly  
#============================================================================#
#Note you can explore the SCB API by the following
#pxweb_get(
#  "https://api.scb.se/OV0104/v1/doris/en/ssd/NR/NR0103/NR0103C"
#)

url_gdp_sa <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103S/NR0103ENS10SnabbStat"
)

#meta_gdp_sa <- pxweb_get(url_gdp_sa)
#meta_gdp_sa$variables
#This code allows you to explore the available variables
#We want the seasonally adjusted one.

gdp_sa <- download_pxweb(
  url_gdp_sa,
  list(
    EkoIndikator = "BNP10",
    # "BNP10"	- GDP at market prices
    ContentsCode = "NR0103A¤",
    #"NR0103A!" - "Change in volume corresponding quarter previous year, percent"
    #"NR0103A¤" - "Seasonally adjusted, change in volume, previous quarter, percent"
    Tid = "*" # "*" - All available data. We would cut this in data processing.
  )
)

gdp_sa <- gdp_sa %>%
  rename(
    gdp_growth = 
      `Seasonally adjusted, change in volume, previous quarter, percent`
  )

write_csv(
  gdp_sa,
  "0_Raw_Data/1_SCB_gdp_growth_quarterly.csv"
)



#============================================================================#
#          [2] SCB: Household Consumption by Durability Category
#============================================================================#

url_durable_q <- paste0(
  "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/",
  "NR/NR0103/NR0103A/NR0103ENS2010T03Kv"
)

durable_q <- download_pxweb(
  url_durable_q,
  list(
    Varaktighet = "*",
    ContentsCode = "NR0103B0",
    Tid = "*"
  )
)

write_csv(
  durable_q,
  "0_Raw_Data/2_SCB_household_consumption_categorized_quarterly.csv"
)

#============================================================================#
#                   [3] SCB: Household Sector Indicators   
#============================================================================#

url_sector_indicators <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103C/SektorENS2010KvKeyIn"
)

sector_indicators <- download_pxweb(
  url_sector_indicators,
  list(
    Sektor = "*",
    NRindikator = "*",
    ContentsCode = "000000ZF",
    Tid = "*" # Available as far back as 1980K1
  )
)

write_csv(
  sector_indicators,
  "0_Raw_Data/3_SCB_household_sector_a_indicators.csv"
)

#----------------------------------------------------------------------------#

url_income_q <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/START/",
  "NR/NR0103/NR0103C/HusDispInkENS2010Kv"
)

income_raw <- download_pxweb(
  url_income_q,
  list(
    Transaktionspost = "B6n",
    ContentsCode = "NR0103DV",
    Tid = "*"
  )
)

write_csv(
  income_raw,
  "0_Raw_Data/3_SCB_household_sector_b_income_raw.csv"
)

#============================================================================#
#                  [4] SCB: Household Annual Balance Sheet
#============================================================================#


url_balance_sheet <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR9999/NR9999NF/SektorENS2010ArBR"
)

annual_balance_sheet <- download_pxweb(
  url_balance_sheet,
  list(
    Sektor = "S14",
    Tillgangsslag = "*",
    ContentsCode = "000000KI",
    Tid = "*"
  )
)

write_csv(
  annual_balance_sheet,
  "0_Raw_Data/4_SCB_household_balance_sheet_annual.csv"
)


#============================================================================#
#              [5] SCB: Household Financial Accounts Quarterly 
#============================================================================#

url_fa_esa2010_quarterly <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "FM/FM0103/FM0103A/FirENS2010ofKv"
)

household_financial_accounts_raw <- download_pxweb(
  url_fa_esa2010_quarterly,
  list(
    Sektor = "S14",
    Kontopost = "*",
    ContentsCode = "FM0103AS",
    Tid = "*" # Available as back as 1996K1 
  )           # Kind of sad that this means losing more than 10 years of data
)

write_csv(
  household_financial_accounts_raw,
  "0_Raw_Data/5_SCB_household_financial_accounts_full_quarterly.csv"
)


#============================================================================#
#                       [6] SCB: House Price Index
#============================================================================#
                                          
url_hpi_quarterly <- 
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501/BO0501A/FastpiPSRegKv"

hpi_all <- download_pxweb(
  url_hpi_quarterly,
  list(
    Region = "*",
    ContentsCode = "BO0501K2",
    Tid = "*" #Available back to 1986K1 (Index Based)
  )
)

write_csv(
  hpi_all,
  "0_Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv"
)


#============================================================================#
#                        [7] SCB: CPI, Fixed Index numbers
#============================================================================#
                                          
url_cpi <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "PR/PR0101/PR0101A/KPItotM"
)

cpi <- download_pxweb(
  url_cpi,
  list(
    ContentsCode = "*",
    Tid = "*"
  )
)

write_csv(
  cpi,
  "0_Raw_Data/7_SCB_CPI_monthly.csv" # Will have to be adjusted to quarterly.
)

#============================================================================#
#                        [8] OECD: Real GDP Growth G7
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
  "0_Raw_Data/8_D_OECD_KIX_real_gdp_growth_quarterly.csv"
)


#============================================================================#
#                          [9] Riksbank Policy RAte
#============================================================================#

repo_rate <- get_riksbank_series("SECBREPOEFF")
write_csv(repo_rate,"0_Raw_Data/9_Riksbank_policy_rate_daily.csv")
# Daily rate, will have to be adjusted.

#------------------------------------------------------------------------------#
# Other interesting rates in case necessary.

#deposit_rate <- get_riksbank_series("SECBDEPOEFF")
#lending_rate <- get_riksbank_series("SECBLENDEFF")
#reference_rate <- get_riksbank_series("SECBREFEFF")
#write_csv(deposit_rate,"0_Raw_Data/8_Riksbank_deposit_rate_daily.csv")
#write_csv(lending_rate,"0_Raw_Data/9_Riksbank_lending_rate_daily.csv")
#write_csv(reference_rate,"0_Raw_Data/10_Riksbank_reference_rate_daily.csv")


#============================================================================#
#               [9] Riksbank: Krona Exchange Rate Index (KIX)
#============================================================================#

KIX92 <- get_riksbank_series("SEKKIX92")
write_csv(KIX92,"0_Raw_Data/10_KIX_Exchange_Rate_Index.csv")
# Effective exchange rate index - KIX and TCW
# The exchange rate index weights together different bilateral exchange rates 
# to create an effective (or average) exchange rate.


#============================================================================#
#                         Riksbank KIX weights [8_B]
#============================================================================#
#Used for KIX GDP 
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
write.csv( kix_weights, "0_Raw_Data/8_B_KIX_weights.csv")


#============================================================================#
#                       [10] IBS: Debt Service Ratio 
#============================================================================#

# API endpoint
url <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.SE.H"

# Read and parse automatically
sdmx_obj <- readSDMX(url)
df <- as.data.frame(sdmx_obj)

# Clean and write to CSV
df_clean <- df %>% 
  select(TIME_PERIOD, OBS_VALUE) %>% 
  rename(date = TIME_PERIOD, dsr_value = OBS_VALUE)

write_csv(df_clean, "0_Raw_Data/11_Debt_Service_Ratio.csv")



#============================================================================#
#              [11] FED: Swedish Quartely Unemployment Rate (SA)
#============================================================================#

# Sweden unemployment rate – quarterly, seasonally adjusted
# OECD harmonized unemployment rate via FRED
# Population: 15 years and over
# Available from 1983Q1

url_unemployment_quarterly <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=LRHUTTTTSEQ156S"
)

unemployment_rate_raw <- read_csv(
  url_unemployment_quarterly,
  show_col_types = FALSE
) %>%
rename(
   Unemployment_Rate = LRHUTTTTSEQ156S )%>%
mutate(
    observation_date = as.yearqtr(observation_date)
  )

write_csv(
  unemployment_rate_raw,
  "0_Raw_Data/12_FRED_Sweden_unemployment_rate_quarterly.csv"
)


#============================================================================#
#              [12] FED: Swedish Quartely Unemployment Rate (SA)
#============================================================================#

# Sweden unemployment rate – quarterly, seasonally adjusted
# OECD harmonized unemployment rate via FRED
# Population: 15 years and over
# Available from 1983Q1

url_unemployment_quarterly <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=LRHUTTTTSEQ156S"
)

unemployment_rate_raw <- read_csv(
  url_unemployment_quarterly,
  show_col_types = FALSE
) %>%
rename(
   Unemployment_Rate = LRHUTTTTSEQ156S )%>%
mutate(
    observation_date = as.yearqtr(observation_date)
  )

write_csv(
  unemployment_rate_raw,
  "0_Raw_Data/12_FRED_Sweden_unemployment_rate_quarterly.csv"
)


#============================================================================#
#              [13] FED: Brent Crude Oil Price (Quarterly)
#============================================================================#

# Brent crude oil price – quarterly
# FRED series: POILBREUSDQ
# Unit: US dollars per barrel

url_brent_quarterly <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=POILBREUSDQ"
)

brent_price_raw <- read_csv(
  url_brent_quarterly,
  show_col_types = FALSE
) %>%
  rename(
    Brent_Price = POILBREUSDQ
  ) %>%
  mutate(
    observation_date = as.yearqtr(observation_date)
  )

write_csv(
  brent_price_raw,
  "0_Raw_Data/13_FRED_Brent_crude_oil_price_quarterly.csv"
)


#============================================================================#
#              [14] FED: US Federal Funds Rate (Quarterly)
#============================================================================#

# US federal funds effective rate – monthly, aggregated to quarterly mean
# FRED series: FEDFUNDS
# Unit: percent

url_fedfunds <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=FEDFUNDS",
  "&cosd=1995-01-01"
)

fedfunds_monthly_raw <- read_csv(
  url_fedfunds,
  show_col_types = FALSE
) %>%
  rename(
    FedFunds_Rate = FEDFUNDS
  ) %>%
  mutate(
    observation_date = as.Date(observation_date)
  )

write_csv(
  fedfunds_monthly_raw,
  "0_Raw_Data/14_FRED_US_federal_funds_rate_monthly.csv"
)


#============================================================================#
#                   FRED: China Annual Real GDP  [8C]
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


#===========================================================================#
#               [15] NIER: Consumer Confidence Indicator
#===========================================================================#

# Base URL pointing directly to the Indikatorm.px matrix
url_nier <- "https://statistik.konj.se/PxWeb/api/v1/en/KonjBar/indikatorer/Indikatorm.px"

# Query the table
test <- download_pxweb(
  url = url_nier,
  query = list(
    Indikator = c("bhus"), # Consumer confidence indicator code
    Period = c("*")        # All available monthly periods
  )
)

# Write to CSV
write_csv(
  test,
  "0_Raw_Data/15_NIER_Consumer_Confidence.csv"
)



#===========================================================================#
#               [16] ECB: Monthly Euro Area HICP
#===========================================================================#

# ECB series:
# ICP.M.U2.N.000000.4.INX
# Monthly Euro Area HICP, Overall Index, 2015=100

ea_hicp_raw <- get_data("ICP.M.U2.N.000000.4.INX")

# Save raw data
dir.create(
  "0_Raw_Data",
  showWarnings = FALSE,
  recursive = TRUE
)

write_csv(
  ea_hicp_raw,
  "0_Raw_Data/16_ECB_Euro_Area_HICP_monthly_raw.csv"
)
