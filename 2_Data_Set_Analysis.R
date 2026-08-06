#####################################2.DATA_SET_ANALYSIS####################################
##                                                                                         #


# Read processed macro data
macro_data <- read_csv("Processed_Data/1c_final_data.csv")

summary(macro_data)




household_debt %>%
  arrange(desc(debt_growth)) %>%
  head(10)

meta_consumption$variables

summary(
  macro$gdp_growth
)

head(
  macro %>% 
    select(quarter,gdp_growth),
  20
)





# Convert vector to time series object (assume quarterly data starting Q1 1995)
debt_growth_ts <- ts(macro_data$saving_rate, start = c(1996, 1), frequency = 4)

# Run X-13 seasonal adjustment
fit <- seas(saving_ts)

# Extract seasonally adjusted series
saving_rate_sa <- final(fit)

# Plot original vs. adjusted
plot(fit)








# Test unit root on seasonally adjusted data
summary(ur.df(saving_rate_sa, type = "drift", selectlags = "AIC"))
summary(ur.kpss(saving_rate_sa, type = "tau"))






##########################################################################################################
###                                 1. Unit root tests                                     ### 

# ==========================================================================================#
#                                  7_No Trend                                              

#Stationary Test for Gdp Growth
summary(ur.df(macro_data$gdp_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(macro_data$gdp_growth))

#Stationary Test for Consumption Growth
summary(ur.df(macro_data$consumption_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(macro_data$consumption_growth))

#Stationary Test for Debt Growth
summary(ur.df(macro_data$debt_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(macro_data$debt_growth))

#Stationary Test for Real House Price
summary(ur.df(macro_data$real_house_price_growth, type = "none", selectlags = "AIC"))
summary(ur.kpss(macro_data$real_house_price_growth))

# ==========================================================================================#
#                                  Drift                                             

#Stationary Test for Savings Rate
summary(ur.df(macro_data$saving_rate, type = "drift", selectlags = "AIC"))
summary(ur.kpss(macro_data$saving_rate))


#Stationary Test for Consumption Growth
summary(ur.df(macro_data$interest_burden, type = "drift", selectlags = "AIC"))
summary(ur.kpss(macro_data$interest_burden))


#Stationary Test for Policy Rate
summary(ur.df(macro_data$policy_rate, type = "drift", selectlags = "AIC"))
summary(ur.kpss(macro_data$policy_rate))


# ==========================================================================================#
#                                   Linear                                           

#Stationary Test for Asset-to-Liability Ratio
summary(ur.df(macro_data$asset_liability_ratio, type = "trend", selectlags = "AIC"))
summary(ur.kpss(macro_data$asset_liability_ratio))

#Stationary Test for Debt-to-Income
summary(ur.df(macro_data$debt_income, type = "trend", selectlags = "AIC"))
summary(ur.kpss(macro_data$debt_income))














