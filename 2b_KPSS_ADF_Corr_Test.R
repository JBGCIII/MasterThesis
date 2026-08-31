###############################################################################
############################ 2b. KPSS/ADF/CORR TEST ###########################
###############################################################################

data_set_adf_kpss <- read_csv(
    "1_Processed_Data/1d_Final_Data_Set.csv"
)
#============================================================================#
#                              [1] Unit root Tests
#============================================================================#

#----------------------------------------------------------------------------#
#                                No Trend                                              

#Stationary Test for Gdp Growth
summary(ur.df(data_set_adf_kpss$gdp_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$gdp_growth))

#Stationary Test for Consumption Growth
summary(ur.df(data_set_adf_kpss$consumption_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$consumption_growth))

#Stationary Test for Debt Growth
summary(ur.df(data_set_adf_kpss$debt_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$debt_growth))

#Stationary Test for Real House Price
summary(ur.df(data_set_adf_kpss$real_house_price_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$real_house_price_growth))

#Stationary Test for Exchange Rate
summary(ur.df(data_set_adf_kpss$exchange_rate_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$exchange_rate_growth))

#Stationary Test for Inflation
summary(ur.df(data_set_adf_kpss$cpi_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$cpi_growth))


#Stationary Test for Export Growth
summary(ur.df(data_set_adf_kpss$export_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$export_growth))

#----------------------------------------------------------------------------#
#                                  Drift                                             

#Secondary Stationary Test for Debt Growth Seasonally Adjusted
summary(ur.df(data_set_adf_kpss$debt_growth, type = "drift", selectlags = "AIC"))

#Stationary Test for Savings Rate
summary(ur.df(data_set_adf_kpss$saving_rate, type = "drift", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$saving_rate))


#Stationary Test for Interest Burden
summary(ur.df(data_set_adf_kpss$interest_burden, type = "drift", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$interest_burden))


#Stationary Test for Policy Rate
summary(ur.df(data_set_adf_kpss$policy_rate, type = "drift", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$policy_rate))


#----------------------------------------------------------------------------#
#                                   Linear                                           

#Stationary Test for Asset-to-Liability Ratio
summary(ur.df(data_set_adf_kpss$asset_liability_ratio, type = "trend", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$asset_liability_ratio))

#Stationary Test for Debt-to-Income
summary(ur.df(data_set_adf_kpss$debt_income, type = "trend", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$debt_income))



#============================================================================#
#                            [2] Correlation Test
#============================================================================# 

df_numeric <- data_set_adf_kpss %>% dplyr::select(where(is.numeric))

write.csv(cor(df_numeric, use = "complete.obs"), 
"2_Data_Inspection/Correlation_matrix.csv")
pc_stat <- prcomp(df_numeric, scale. = TRUE)

# 1. Transform variables based on stationarity  
df_stationary <- data_set_adf_kpss %>%
  mutate(
    d_debt_growth            = debt_growth - lag(debt_growth),
    d_asset_liability_ratio  = asset_liability_ratio - lag(asset_liability_ratio),
    d_saving_rate            = saving_rate - lag(saving_rate),
    d_debt_income            = debt_income - lag(debt_income),
    d_interest_burden        = interest_burden - lag(interest_burden),
    d_policy_rate            = policy_rate - lag(policy_rate)
  ) %>%
  drop_na()

# 1. Select ONLY the 9 stationary variables (excluding un-differenced I(1) variables)
df_clean_pca <- df_stationary %>% 
  dplyr::select(
    gdp_growth, 
    consumption_growth, 
    d_debt_growth, 
    d_asset_liability_ratio, 
    d_saving_rate, 
    d_debt_income, 
    d_interest_burden, 
    real_house_price_growth,
    cpi_growth,
    d_policy_rate,
    exchange_rate_growth,
    export_growth
  )

write.csv(cor(df_clean_pca, use = "complete.obs"), 
"2_Data_Inspection/Correlation_Matrix_First_Diff.csv")

# Run PCA on standardized, stationarized data
pca_stationary <- prcomp(df_clean_pca, scale. = TRUE)
summary(pca_stationary)


pca_stationary