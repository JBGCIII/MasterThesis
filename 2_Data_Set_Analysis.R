#####################################2.DATA_SET_ANALYSIS#####################################
##                                                                                          #

data_set_adf_kpss <- read_csv("Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)

#############################################################################################
###                                 1. Unit root tests                                    ### 

# ==========================================================================================#
#                                   No Trend                                              

#Stationary Test for Gdp Growth
summary(ur.df(data_set_adf_kpss$gdp_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$gdp_growth))

#Stationary Test for Consumption Growth
summary(ur.df(data_set_adf_kpss$consumption_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$consumption_growth))

#Stationary Test for Debt Growth Seasonally Adjusted
summary(ur.df(data_set_adf_kpss$debt_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$debt_growth))


#Stationary Test for Real House Price
summary(ur.df(data_set_adf_kpss$real_house_price_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$real_house_price_growth))

# ==========================================================================================#
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


# ==========================================================================================#
#                                   Linear                                           

#Stationary Test for Asset-to-Liability Ratio
summary(ur.df(data_set_adf_kpss$asset_liability_ratio, type = "trend", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$asset_liability_ratio))

#Stationary Test for Debt-to-Income
summary(ur.df(data_set_adf_kpss$debt_income, type = "trend", selectlags = "AIC"))
summary(ur.kpss(data_set_adf_kpss$debt_income))














