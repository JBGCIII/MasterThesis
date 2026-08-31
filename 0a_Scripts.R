###############################################################################
############################    0.SCRIPT INSTALATION   ########################
###############################################################################

# List of required packages
needed_pkgs <- c(
  # Data Gathering
  "pxweb",      # API data extraction (SCB)
  "httr",       # HTTP requests
  "jsonlite",   # Parsing JSON payloads
  #============================================================================#
  # Core Data Processing & Visualization
  "tidyverse",  # Loads dplyr, readr, tidyr, purrr, ggplot2, etc.
  "psych",      # Descriptive statistics
  "zoo",        # Infrastructure for regular/irregular time series
  "xts",        # Extensible time series (used over base ts to facilitate date splitting in ADF tests)
  
  #============================================================================#
  # Econometric & Time Series Analysis
  "seastests",  # Simple seasonality testing tools
  "seasonal",   # Seasonal adjustment (X-13ARIMA-SEATS)
  "tsoutliers", # Helps identifying outliers in the dataset
  "urca",       # Analysis of integrated and cointegrated time series
  "dynlm",      # Dynamic linear regression
  "forecast",   # Forecasting functions for time series and linear models
  
  #============================================================================#
  # Bayesian Models & SVAR
  "coda",       # Used for formal MCMC convergence diagnostics
  "stats",      # Statistical calculations & random number generation
  "future",     # Parallel and distributed processing
  "abind",      # Combine multidimensional arrays
  "bsvars",     # Bayesian estimation of structural vector autoregressive models
  "bsvarSIGNs", # Add-on for BSVAR identified by sign, zero, and narrative restrictions
  "rlang"       # Core language features and metaprogramming
)

# Package citated using the function
# citation("package_name_here")
#============================================================================#
# Install any missing packages automatically
missing_pkgs <- needed_pkgs[!(needed_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

# Load all packages quietly
suppressPackageStartupMessages(
  lapply(needed_pkgs, library, character.only = TRUE)
)

#============================================================================#

# Note: The package for BSVAR and BSVARSIGN on github is often more up 
# to date and was originally installed as such. 
#remotes::install_github("bsvars/bsvarSIGNs", upgrade = "never")
#remotes::install_github("bsvars/bsvarSIGNs", upgrade = "never")

# However the version used for the thesis (3.0  and 4.0) are now available on CRAN
# Do make sure your are up to date as well!
# available.packages()["bsvars", "Version"]
# available.packages()["bsvarSIGNs", "Version"]

# Check installed version
# packageVersion("bsvars")
# packageVersion("bsvarSIGNs")
#============================================================================#




install.packages("tempdisagg")
# Load tempdisagg and zoo (useful for handling date objects)
library(tempdisagg)
library(zoo)



if (!require("rjson")) install.packages("rjson")


library(rsdmx)
library(dplyr)
library(readr)