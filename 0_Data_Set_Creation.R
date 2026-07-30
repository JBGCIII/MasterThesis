



install.packages(c("pxweb", "httr", "httr2", "jsonlite", "dplyr", "tidyr", "lubridate","readr"))

# ============================================================
# Swedish macro database collection
# Raw data only
# ============================================================


# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------

library(pxweb)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

dir.create(
  "Raw_Data",
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------


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
# SCB GDP
# ============================================================


url_gdp_level <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103A/NR0103ENS2010T01Kv"
)

gdp_all <- download_pxweb(
  url_gdp_level,
  list(
    Anvandningstyp = "*",
    ContentsCode = "NR0103BW",
    Tid = "*" # Available from 1981K1
  )
)

write_csv(
  gdp_all,
  "Raw_Data/1_SCB_gdp_expenditures_real_quarterly.csv"
)

# ============================================================
# SCB Household Consumption
# ============================================================

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
# SCB Household sector indicators
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
# SCB Household Financial Accounts
# Full raw dataset
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
  "Raw_Data/4_SCB_household_financial_accounts_full_quarterly.csv"
)


# ============================================================
# SCB Housing Price Index
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
  "Raw_Data/5_SCB_house_price_index_quarterly_all_regions.csv"
)


# ============================================================
# SCB CPI
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
  "Raw_Data/6_SCB_CPI_monthly.csv"
)



# ============================================================
# Riksbank interest rates
# ============================================================

repo_rate <- get_riksbank_series("SECBREPOEFF")
deposit_rate <- get_riksbank_series("SECBDEPOEFF")
lending_rate <- get_riksbank_series("SECBLENDEFF")
reference_rate <- get_riksbank_series("SECBREFEFF")

write_csv(repo_rate,"Raw_Data/7_Riksbank_policy_rate_daily.csv")
write_csv(deposit_rate,"Raw_Data/8_Riksbank_deposit_rate_daily.csv")
write_csv(lending_rate,"Raw_Data/9_Riksbank_lending_rate_daily.csv")
write_csv(reference_rate,"Raw_Data/10_Riksbank_reference_rate_daily.csv")









