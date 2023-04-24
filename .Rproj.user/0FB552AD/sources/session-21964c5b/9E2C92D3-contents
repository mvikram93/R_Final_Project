library(tidyverse)
library(readr)
library(dplyr)
library(plotly)
library(scatterplot3d)
library(GGally)
#data = read_parquet(file="data/cyber.parquet")
#val <- data[(1:5),]
#View(val)
#glimpse(val)
#usewarning

#data_climate_set = read_excel("data/climate_change_dataset.xls",sheet = "Data",col_types = 'text')
#data_climate_set


climate_change = read_csv("data/ghg-emissions.csv")
climate_change <- climate_change[,-2]
cli <- data.frame(lapply(climate_change,as.character) )
type(cli)
is.data.frame(cli)
glimpse(cli)
data_val <- cli %>% pivot_longer(!"Country.Region",names_to = "Year",values_to = "Range") %>% 
  mutate(.,Range=as.numeric(Range)) %>% mutate(.,Year=as.factor(Year))

str(data_val)
ggpairs(data=data_val, columns=1:3, title="trees data",cardinality_threshold=195)

fit_1 <- lm(Year ~ Range, data = trees)

library(forecast)
data_val_india <-  select(data)
time_series <- ts( )
print(time_series)
plot(time_series,xlab = "Year", ylab = "CO2 Emission")
decomposed_data <- decompose(time_series,"multiplicative")
plot(decomposed_data)
abline(reg=lm(time_series~time(time_series)))
summary(data_val)


# Forecasting 

model <- auto.arima(time_series)

model
plot.ts(model$residuals)
forecast_vale <- forecast(model,level = c(95),h=80*1)
plot(forecast_vale)


Box.test(model$residuals,lag=5,type="Ljung-Box")


Box.test(model$residuals,lag=10,type="Ljung-Box")


Box.test(model$residuals,lag=15,type="Ljung-Box")
