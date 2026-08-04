


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
library(tidyr)
library(lubridate)
library(readr)


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

dir.create(
  "Raw_Data",
  showWarnings = FALSE
)

start_quarter <- "1995K1"
start_date <- "1995-01-01"



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


meta_gdp <- pxweb_get(url_gdp_level)


quarters <- meta_gdp$variables[[3]]$values[
  meta_gdp$variables[[3]]$values >= start_quarter
]


# GDP at market prices
real_gdp <- download_pxweb(
  url_gdp_level,
  list(
    Anvandningstyp = "BNPM",
    ContentsCode = "NR0103BW",
    Tid = quarters
  )
)


real_gdp <- real_gdp |>
  rename(
    real_gdp_sek_million =
      `Constant prices, reference year 2025, SEK million`
  ) |>
  select(
    quarter,
    real_gdp_sek_million
  )


write_csv(
  real_gdp,
  "Raw_Data/real_gdp_quarterly.csv"
)



# ------------------------------------------------------------
# GDP expenditure components
# ------------------------------------------------------------


gdp_all <- download_pxweb(
  url_gdp_level,
  list(
    Anvandningstyp =
      meta_gdp$variables[[1]]$values,

    ContentsCode =
      "NR0103BW",

    Tid =
      quarters
  )
)


write_csv(
  gdp_all,
  "Raw_Data/SCB_gdp_expenditure_real_quarterly.csv"
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
    Andamal =
      meta_consumption$variables[[1]]$values,

    ContentsCode =
      "0000079H",

    Tid =
      quarters
  )
)



write_csv(
  consumption_all,
  "Raw_Data/SCB_household_consumption_real_quarterly.csv"
)



# ============================================================
# SCB Household sector indicators
# ============================================================


url_sector_indicators <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103C/SektorENS2010KvKeyIn"
)



meta_sector_indicators <- pxweb_get(
  url_sector_indicators
)



sector_indicators <- download_pxweb(
  url_sector_indicators,
  list(

    Sektor =
      meta_sector_indicators$variables[[1]]$values,

    NRindikator =
      meta_sector_indicators$variables[[2]]$values,

    ContentsCode =
      "000000ZF",

    Tid =
      quarters
  )
)



write_csv(
  sector_indicators,
  "Raw_Data/SCB_household_sector_indicators.csv"
)



# ============================================================
# SCB Sector accounts
# ============================================================


url_sector_accounts <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "NR/NR0103/NR0103C/SektorENS2010Kv"
)



meta_sector_accounts <- pxweb_get(
  url_sector_accounts
)



sector_accounts_households <- download_pxweb(
  url_sector_accounts,
  list(

    Sektor = "S14",

    Transaktionspost =
      meta_sector_accounts$variables[[2]]$values,

    ContentsCode =
      "NR0103DT",

    Tid =
      quarters
  )
)



write_csv(
  sector_accounts_households,
  "Raw_Data/SCB_household_sector_accounts.csv"
)



# ============================================================
# Riksbank interest rates
# ============================================================


repo_rate <- get_riksbank_series(
  "SECBREPOEFF"
)


deposit_rate <- get_riksbank_series(
  "SECBDEPOEFF"
)


lending_rate <- get_riksbank_series(
  "SECBLENDEFF"
)


reference_rate <- get_riksbank_series(
  "SECBREFEFF"
)



write_csv(
  repo_rate,
  "Raw_Data/Riksbank_policy_rate_daily.csv"
)


write_csv(
  deposit_rate,
  "Raw_Data/Riksbank_deposit_rate_daily.csv"
)


write_csv(
  lending_rate,
  "Raw_Data/Riksbank_lending_rate_daily.csv"
)


write_csv(
  reference_rate,
  "Raw_Data/Riksbank_reference_rate_daily.csv"
)








# ============================================================
# SCB Household Financial Accounts
# Full raw dataset
# ESA2010 quarterly 1996K1-2026K1
# ============================================================

url_fa_esa2010_quarterly <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "FM/FM0103/FM0103A/FirENS2010ofKv"
)


all_items <- meta_fa_esa2010$variables[[2]]$values

all_quarters <- meta_fa_esa2010$variables[[5]]$values


household_financial_accounts_raw <- download_pxweb(
  url_fa_esa2010_quarterly,
  list(
    Sektor = "S14",
    Kontopost = all_items,
    ContentsCode = "FM0103AS",
    Tid = all_quarters
  )
)

head(household_financial_accounts_raw)

dim(household_financial_accounts_raw)


write_csv(
  household_financial_accounts_raw,
  "Raw_Data/SCB_household_financial_accounts_full_quarterly.csv"
)





library(httr)

url_bo <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BO"

content(
  GET(url_bo),
  "text",
  encoding="UTF-8"
)



url_bo0501 <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501"

content(
  GET(url_bo0501),
  "text",
  encoding="UTF-8"
)


url_hpi <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501/BO0501A"


content(
  GET(url_hpi),
  "text",
  encoding="UTF-8"
)


url_hpi_quarterly <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501/BO0501A/FastpiPSRegKv"
)

meta_hpi <- pxweb_get(url_hpi_quarterly)

meta_hpi$variables






library(pxweb)
library(dplyr)
library(readr)

url_hpi_quarterly <- paste0(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/",
  "BO/BO0501/BO0501A/FastpiPSRegKv"
)

# Download everything
hpi_all <- pxweb_get(url_hpi_quarterly) |>
  pxweb_query(
    list(
      Region = "*",
      ContentsCode = "*",
      Tid = "*"
    )
  ) |>
  pxweb_get_data()

# Inspect
head(hpi_all)
names(hpi_all)








library(httr)
library(jsonlite)

query_hpi_json <- list(
  query = list(
    list(
      code = "Region",
      selection = list(
        filter = "item",
        values = list("00")
      )
    ),
    list(
      code = "ContentsCode",
      selection = list(
        filter = "item",
        values = list("BO0501K2")
      )
    ),
    list(
      code = "Tid",
      selection = list(
        filter = "all",
        values = list("*")
      )
    )
  ),
  response = list(
    format = "json-stat2"
  )
)

res <- POST(
  url_hpi_quarterly,
  body = query_hpi_json,
  encode = "json"
)

hpi_raw <- fromJSON(
  content(res, "text", encoding="UTF-8")
)

hpi_df <- as.data.frame(hpi_raw)


hpi_df <- data.frame(
  quarter = names(hpi_raw$dimension$Tid$category$index),
  real_house_price_index = as.numeric(hpi_raw$value)
)

head(hpi_df)

write_csv(
  hpi_df,
  "Raw_Data/SCB_real_house_price_index_quarterly.csv"
)





















library(httr)
library(jsonlite)
library(readr)

url_hpi_quarterly <- 
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0501/BO0501A/FastpiPSRegKv"

query_hpi_json <- list(
  query = list(
    list(
      code = "Region",
      selection = list(
        filter = "all",
        values = list("*")
      )
    ),
    list(
      code = "ContentsCode",
      selection = list(
        filter = "item",
        values = list("BO0501K2")
      )
    ),
    list(
      code = "Tid",
      selection = list(
        filter = "all",
        values = list("*")
      )
    )
  ),
  response = list(
    format = "json-stat2"
  )
)

res <- POST(
  url_hpi_quarterly,
  body = query_hpi_json,
  encode = "json"
)

hpi_raw_all <- fromJSON(
  content(res, "text", encoding = "UTF-8")
)


regions <- names(hpi_raw_all$dimension$Region$category$index)

quarters <- names(hpi_raw_all$dimension$Tid$category$index)

hpi_all <- expand.grid(
  region = regions,
  quarter = quarters
) |>
  arrange(region, quarter) |>
  mutate(
    real_house_price_index = as.numeric(hpi_raw_all$value)
  )

  head(hpi_all)

  write_csv(
  hpi_all,
  "Raw_Data/SCB_real_house_price_index_quarterly_all_regions.csv"
)



