
#####################################0.SCRIPT INSTALATION#####################################
##                                                                                          ##

needed_pkgs <- c("tidyverse", "pxweb", "httr", "jsonlite", "zoo", "seasonal", "urca", "dynlm"
,"xts" )
missing_pkgs <- needed_pkgs[!(needed_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

# Data Gathering
library(pxweb)     # API data extraction (SCB)
library(httr)      # HTTP requests
library(jsonlite)  # Parsing JSON payloads

# Core Data Processing & Visualization (Loads dplyr, readr, tidyr, purrr, ggplot2)
library(tidyverse) 

# Econometric & Time Series Analysis
library(zoo)       # Infrastructure for regular/irregular time series
library(seasonal)  # Seasonal adjustment (X-13ARIMA-SEATS)
library(urca)  # Analysis of Integrated and Cointegrated Time Series
library(dynlm)  # Dynamic Linear Regression
library(xts) # Exstensible Time Series (I would have used build in Ts if not 
             #for having to split dates in ADF test)





install.packages("tsoutliers")
library(tsoutliers)
library(forecast)