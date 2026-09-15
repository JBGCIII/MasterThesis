
#============================================================================#
#                         Riksbank KIX weights [8_B]
#============================================================================#
# This was used to extract files for KIX weights directly from riksbanken's
# XLSX file online. However, due to uncertainty regarding it's stability
# I've decided to have it as static file. You can still donwlowad it as follows

#Requires the package readxl
# install.packages(readxl)
library(readxl)
library(tidyverse)
library(tsibble)
library(readexcel)


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
write.csv( kix_weights, "0_Raw_Data/8_D_KIX_weights.csv")


#============================================================================#
#                         World HICP
#============================================================================#
HICP <- kix_weights_raw[4:36, 1:33]

local_HICP_path <- "0_Raw_Data_Static/HICP.xlsx"

# 1. Read Country names (C1:C203)
countries <- read_excel(
  local_HICP_path, 
  sheet = 1, 
  range = "C1:C203"
) %>% 
  pull(1)  # Extracts as a character vector

# 2. Read Values with Year headers (F1:DR203)
# Setting col_names = TRUE treats F1:DR1 as the column names (Years)
HICP_values <- read_excel(
  local_HICP_path, 
  sheet = 1, 
  range = "F1:DR203", 
  col_names = TRUE
)

# 3. Combine Country column with Values matrix
HICP_weights <- bind_cols(
  country = countries,
  HICP_values
)

# 1. Define the named lookup vector: English Name in Excel -> ISO Code
# Note: "Belgium" and "Luxembourg" in the source are combined to "BEL_LUX"
country_map <- c(
  "Australia"          = "AUS",
  "Belgium"            = "BEL_LUX",  # Mapped to BEL_LUX
  "Luxembourg"         = "BEL_LUX",  # Mapped to BEL_LUX if separate in sheet
  "Brazil"             = "BRA",
  "Denmark"            = "DNK",
  "Finland"            = "FIN",
  "France"             = "FRA",
  "Greece"             = "GRC",
  "India"              = "IND",
  "Ireland"            = "IRL",
  "Iceland"            = "ISL",
  "Italy"              = "ITA",
  "Japan"              = "JPN",
  "Canada"             = "CAN",
  "China"              = "CHN",
  "Mexico"             = "MEX",
  "Netherlands"        = "NLD",
  "Norway"             = "NOR",
  "New Zealand"        = "NZL",
  "Poland"             = "POL",
  "Portugal"           = "PRT",
  "Russian Federation" = "RUS",
  "Switzerland"        = "CHE",
  "Slovakia"           = "SVK",
  "Spain"              = "ESP",
  "United Kingdom"     = "GBR",
  "Korea, Rep."        = "KOR",
  "Czech Republic"     = "CZE",
  "Türkiye"            = "TUR",
  "Germany"            = "DEU",
  "Hungary"            = "HUN",
  "United States"      = "USA",
  "Austria"            = "AUT"
)

# 2. Filter and recode in your data processing pipeline
HICP_weights_filtered <- HICP_weights %>%
  filter(country %in% names(country_map)) %>%
  mutate(
    iso_code = country_map[country],
    # Replace English country names with your Swedish names
    country = case_match(
      country,
      "Australia"          ~ "Australien",
      "Belgium"            ~ "Belgien-Luxemburg",
      "Luxembourg"         ~ "Belgien-Luxemburg",
      "Brazil"             ~ "Brasilien",
      "Denmark"            ~ "Danmark",
      "Finland"            ~ "Finland",
      "France"             ~ "Frankrike",
      "Greece"             ~ "Grekland",
      "India"              ~ "Indien",
      "Ireland"            ~ "Irland",
      "Iceland"            ~ "Island",
      "Italy"              ~ "Italien",
      "Japan"              ~ "Japan",
      "Canada"             ~ "Kanada",
      "China"              ~ "Kina",
      "Mexico"             ~ "Mexiko",
      "Netherlands"        ~ "Nederländerna",
      "Norway"             ~ "Norge",
      "New Zealand"        ~ "Nya Zeeland",
      "Poland"             ~ "Polen",
      "Portugal"           ~ "Portugal",
      "Russian Federation" ~ "Ryssland",
      "Switzerland"        ~ "Schweiz",
      "Slovakia"           ~ "Slovakien",
      "Spain"              ~ "Spanien",
      "United Kingdom"     ~ "Storbritannien",
      "Korea, Rep."        ~ "Sydkorea",
      "Czech Republic"     ~ "Tjeckien",
      "Türkiye"            ~ "Turkiet",
      "Germany"            ~ "Tyskland",
      "Hungary"            ~ "Ungern",
      "United States"      ~ "USA",
      "Austria"            ~ "Österrike"
    )
  ) %>%
  select(country, iso_code, everything())

write_csv(HICP_weights_filtered, "0_Raw_Data/8_B_HICP_Values.csv")
