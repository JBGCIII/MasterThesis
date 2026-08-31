###############################################################################
############################# 1.a DATA_SET_CREATION ###########################
###############################################################################

dir.create( "0_Raw_Data", showWarnings = FALSE) # Create Directory
# Reminder to myself to change all Raw_Data to 0_Raw_Data at once.

#============================================================================#
#                            1_SCB_gdp_expenditures_real_quarterly  
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
#                       2_SCB_household_consumption_real_quarterly   
#============================================================================#

consumption_growth <- download_pxweb(
  url_gdp_sa,
  list(
    EkoIndikator = "BNP30",
    ContentsCode = "NR0103A¤",
    Tid = "*"
  )
)

consumption_growth <- consumption_growth %>%
  rename(
    consumption_growth =
      `Seasonally adjusted, change in volume, previous quarter, percent`
  )


write_csv(
  consumption_growth,
  "0_Raw_Data/2_SCB_household_consumption_real_quarterly.csv"
)


#============================================================================#
#                      3_SCB_household_sector_indicators   
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
  "0_Raw_Data/3_SCB_household_sector_indicators.csv"
)


#============================================================================#
#                  4_SCB_household_annual_balance_sheet
#============================================================================#


url_balance_sheet <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR9999/NR9999NF/SektorENS2010ArBR"
)

annual_balance_sheet <- download_pxweb(
  url_balance_sheet,
  list(
    Sektor = "S14",
    Tillgangsslag = c("A", "AN", "AN1", "AN2", "AFA", "AFL", "B90"),
    ContentsCode = "000000KI",
    Tid = "*"
  )
)

write_csv(
  annual_balance_sheet,
  "0_Raw_Data/4_SCB_household_balance_sheet_annual.csv"
)


#============================================================================#
#                  5_SCB_household_financial_accounts_full_quarterly 
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
#                             5_SCB_Export_Growth
#============================================================================#

# Note: "https://api.scb.se/OV0104/v1/doris/en/ssd/",
# "NR/NR0103/NR0103B/NR0103ENS2010T10SKv" and
# https://api.scb.se/OV0104/v1/doris/en/ssd/",
# "NR/NR0103/NR0103S/NR0103ENS10SnabbStat
# give the same values.

export_growth <- download_pxweb(
  url_gdp_sa,
  list(
    EkoIndikator = "BNP70",
    ContentsCode = "NR0103A¤",
    Tid = "*"
  )
)

export_growth <- export_growth %>%
  rename(
    export_growth =
      `Seasonally adjusted, change in volume, previous quarter, percent`
  )


write_csv(
  export_growth,
  "0_Raw_Data/6_SCB_Export_Growth.csv"
)



#============================================================================#
#                              6_SCB_CPI_monthly   
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
#                       7_SCB_house_price_index_quarterly_all_regions 
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
  "0_Raw_Data/8_SCB_house_price_index_quarterly_all_regions.csv"
)


#============================================================================#
#                        8_Riksbank interest rates 
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
#                        10_KIX_Exchange_Rate_Index
#============================================================================#

KIX92 <- get_riksbank_series("SEKKIX92")
write_csv(KIX92,"0_Raw_Data/10_KIX_Exchange_Rate_Index.csv")
# Effective exchange rate index - KIX and TCW
# The exchange rate index weights together different bilateral exchange rates 
# to create an effective (or average) exchange rate.



#============================================================================#
#                            10_Debt_Service_Ratio
#============================================================================#


# SDMX API endpoint
url <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.SE.H"

# Read and parse automatically
sdmx_obj <- readSDMX(url)
df <- as.data.frame(sdmx_obj)

# Clean and write to CSV
df_clean <- df %>% 
  select(TIME_PERIOD, OBS_VALUE) %>% 
  rename(date = TIME_PERIOD, dsr_value = OBS_VALUE)

write_csv(df_clean, "0_Raw_Data/11_Debt_Service_Ratio.csv")