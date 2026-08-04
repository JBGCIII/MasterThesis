



install.packages(c("pxweb", "httr", "httr2", "jsonlite", "dplyr", "tidyr", "lubridate","readr"))

# ============================================================
# Swedish macro database collection
# Raw data only
# ============================================================

# Packages
library(pxweb)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Settings
dir.create(
  "Raw_Data",
  showWarnings = FALSE
)

# Helper functions
# SCB PXWEB downloader
download_pxweb <- function(url, query){

  pxq <- pxweb_query(query)

  pxweb_get_data(
    url = url,
    query = pxq,
    column.name.type = "text",
    variable.value.type = "text"
  )
}


# Riksbank SWEA downloader
start_date <- "1995-01-01"
get_riksbank_series <- function(series_id,
                                from = start_date,
                                to = Sys.Date()) {

  url <- paste0(
    "https://api.riksbank.se/swea/v1/Observations/",
    series_id,
    "/",
    from,
    "/",
    to
  )

  response <- GET(url)

  stop_for_status(response)

  fromJSON(
    content(
      response,
      "text",
      encoding = "UTF-8"
    )
  ) |>
    as.data.frame()

}



# ============================================================
# 1_SCB_gdp_expenditures_real_quarterly
# ============================================================


#url_gdp_level <- paste0(
#  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
#  "NR/NR0103/NR0103A/NR0103ENS2010T01Kv"
#)


#Not seasonally adjusted
#gdp_all <- download_pxweb(
#  url_gdp_level,
#  list(
#    Anvandningstyp = "*",
#    ContentsCode = "NR0103BW",
#    Tid = "*" # Available from 1981K1
#  )
#)

#write_csv(
#  gdp_all,
#  "Raw_Data/1_SCB_gdp_expenditures_real_quarterly.csv"
#)


url_gdp_growth <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103A/NR0103ENS2010T01Kv"
)

gdp_growth <- download_pxweb(
  url_gdp_growth,
  list(
    Anvandningstyp = "BNPM",
    ContentsCode = "NR0103BX",
    Tid = "*"
  )
)


library(pxweb)

pxweb_interactive()


gdp_growth <- download_pxweb(
  url_gdp_growth,
  list(
    Anvandningstyp = "BNPM",
    ContentsCode = "...",   # quarter-on-quarter seasonally adjusted code
    Tid = "*"
  )
)






url_gdp_sa <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103A/NR0103ENS2010Kv"
)




url_gdp_sa <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103S/NR0103ENS10SnabbStat"
)


gdp_sa <- download_pxweb(
  url_gdp_sa,
  list(
    EkoIndikator = "BNP10",
    ContentsCode = "NR0103A¤",
    Tid = "*"
  )
)

gdp_sa <- gdp_sa %>%
  rename(
    gdp_growth = 
      `Seasonally adjusted, change in volume, previous quarter, percent`
  )

write_csv(
  gdp_sa,
  "Raw_Data/1_SCB_gdp_growth_quarterly2.csv"
)







#gdp_growth <- gdp_growth %>%
#  rename(
#    gdp_growth =
#      `Change in volume, corresponding period previous year, percent`
#  )


write_csv(
  gdp_growth,
  "Raw_Data/1_SCB_gdp_expenditures_real_quarterly2.csv"
)








# ============================================================
# 2_SCB_household_consumption_real_quarterly
# ============================================================


url_gdp_sa <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103S/NR0103ENS10SnabbStat"
)


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


head(consumption_growth)






url_consumption_level <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103A/NR0103ENS2010T02KvN"
)

meta_consumption <- pxweb_get(
  url_consumption_level
)

# All household consumption categories
consumption_all <- download_pxweb(
  url_consumption_level,
  list(
    Andamal = "*",
    ContentsCode = "0000079H",
    Tid = "*" # Available back to 1981K1
  )
)

write_csv(
  consumption_all,
  "Raw_Data/2_SCB_household_consumption_real_quarterly.csv"
)


# ============================================================
# 3_SCB_household_sector_indicators
# ============================================================


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



# ============================================================
# 4_SCB_household_financial_accounts_full_quarterly
# ============================================================

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
  "Raw_Data/4b_SCB_household_financial_accounts_full_quarterly.csv"
)



# ============================================================
# 5_SCB_CPI_monthly
# ============================================================

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




# ============================================================
# 6_SCB_house_price_index_quarterly_all_regions
# ============================================================

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




# ============================================================
# Riksbank interest rates
# ============================================================

repo_rate <- get_riksbank_series("SECBREPOEFF")
#deposit_rate <- get_riksbank_series("SECBDEPOEFF")
#lending_rate <- get_riksbank_series("SECBLENDEFF")
#reference_rate <- get_riksbank_series("SECBREFEFF")

write_csv(repo_rate,"Raw_Data/7_Riksbank_policy_rate_daily.csv")
#write_csv(deposit_rate,"Raw_Data/8_Riksbank_deposit_rate_daily.csv")
#write_csv(lending_rate,"Raw_Data/9_Riksbank_lending_rate_daily.csv")
#write_csv(reference_rate,"Raw_Data/10_Riksbank_reference_rate_daily.csv")


