

library(tidyverse)

macro <- read_csv(
  "Processed_Data/1c_final_data.csv"
)

summary(macro)

str(macro)




meta_consumption$variables



summary(
  macro$gdp_growth
)

head(
  macro %>% 
    select(quarter,gdp_growth),
  20
)





library(ggplot2)

ggplot(
 macro,
 aes(
  x=quarter,
  y=gdp_growth
 )
)+
geom_line()+
theme_minimal()












library(tidyr)

plot_data <- macro %>%
 select(
  quarter,
  gdp_growth,
  consumption_growth,
  debt_growth,
  saving_rate,
  debt_income,
  real_house_price_growth,
  policy_rate
 ) %>%
 pivot_longer(
  -quarter,
  names_to="variable",
  values_to="value"
 )


ggplot(
 plot_data,
 aes(
  x=quarter,
  y=value
 )
)+
geom_line()+
facet_wrap(
 ~variable,
 scales="free_y",
 ncol=2
)+
theme_minimal()