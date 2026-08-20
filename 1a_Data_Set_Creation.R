
#####################################1.DATA_SET_CREATION######################################
##                                                                                           #


# ==========================================0.Directory=======================================#

dir.create(
  "Raw_Data",
  showWarnings = FALSE
)

##############################################################################################


# ==========================================================================================#
#                            1_SCB_gdp_expenditures_real_quarterly                          #

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
  "Raw_Data/1_SCB_gdp_growth_quarterly.csv"
)


# ==========================================================================================#
#                            2_SCB_household_consumption_real_quarterly                      


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
  "Raw_Data/2_SCB_household_consumption_real_quarterly.csv"
)




# ==========================================================================================#
#                            3_SCB_household_sector_indicators                     

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
    Tid = "*" # Available back to 1980K1
  )
)

write_csv(
  sector_indicators,
  "Raw_Data/3_SCB_household_sector_indicators.csv"
)



# ==========================================================================================#
#                            4_SCB_household_financial_accounts_full_quarterly                  


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
    Tid = "*" # Available back to 1996K1
  )
)

write_csv(
  household_financial_accounts_raw,
  "Raw_Data/4_SCB_household_financial_accounts_full_quarterly.csv"
)


# ==========================================================================================#
#                                       5_SCB_CPI_monthly                

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
  "Raw_Data/5_SCB_CPI_monthly.csv"
)



# ==========================================================================================#
#                                       6_SCB_house_price_index_quarterly_all_regions              

url_hpi_quarterly <- 
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501/BO0501A/FastpiPSRegKv"

hpi_all <- download_pxweb(
  url_hpi_quarterly,
  list(
    Region = "*",
    ContentsCode = "BO0501K2",
    Tid = "*" #Available back to 1986K1
  )
)

write_csv(
  hpi_all,
  "Raw_Data/6_SCB_house_price_index_quarterly_all_regions.csv"
)


# ==========================================================================================#
#                                       7_Riksbank interest rates            


repo_rate <- get_riksbank_series("SECBREPOEFF")
#deposit_rate <- get_riksbank_series("SECBDEPOEFF")
#lending_rate <- get_riksbank_series("SECBLENDEFF")
#reference_rate <- get_riksbank_series("SECBREFEFF")

write_csv(repo_rate,"Raw_Data/7_Riksbank_policy_rate_daily.csv")
#write_csv(deposit_rate,"Raw_Data/8_Riksbank_deposit_rate_daily.csv")
#write_csv(lending_rate,"Raw_Data/9_Riksbank_lending_rate_daily.csv")
#write_csv(reference_rate,"Raw_Data/10_Riksbank_reference_rate_daily.csv")


