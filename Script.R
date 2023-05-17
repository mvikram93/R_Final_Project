if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, pacman, rio, tidyverse, corrplot,formattable,forecast)


column_names = c("Country","Year"," Total GHG Emissions Excluding Land-Use Change and Forestry")


ghg_data_from_excel <- read.csv(file = "data/CAIT_2_Country_GHG_Emissions.csv",skip=1)
socio_economic_data <- read.csv(file = "data/CAIT_2_Country_Socio_Economic_Data.csv")
names(socio_economic_data) <- socio_economic_data %>% gsub(x=names(socio_economic_data),
                                                           pattern="\\.",replacement="_")
names(ghg_data_from_excel)  <- ghg_data_from_excel %>% gsub(x=names(ghg_data_from_excel),
                                                            pattern="\\.",replacement="_")

#View(socio_economic_data)
#View(ghg_data_from_excel)
merged_data_emission_socio <- merge(ghg_data_from_excel,socio_economic_data, 
                                    by.x=c("Country","Year"),by.y=c("Country","Year"))
#View(merged_data_emission_socio)
str(ghg_data_from_excel)

write_csv(x = merged_data_emission_socio,file = "data/updated.csv")


#print(colnames(ghg_data_from_excel))

ghg_data_from_excel[is.na(ghg_data_from_excel)] <- 0

colum_ghg <- ghg_data_from_excel[,c(1,2)]

format_value <- function(data_v){
  formattable(data_v,format="f",digits=2)
}

formatted_data  <- lapply(ghg_data_from_excel[,c(-1,-2)],format_value)
#View(formatted_data)

merged_data <- data.frame(colum_ghg,formatted_data)
View(merged_data)


data_val <- merged_data_emission_socio[merged_data_emission_socio$Country == "World",]
time_series <- ts(data_val$Population__People_,start = c(1990),end=c(2011),frequency = 1)
time_series
plot(time_series,xlab="Year",ylab="CO2 Emission")
glimpse(data_val)


model <- auto.arima(time_series,trace = TRUE)
plot.ts(model$residuals)
forecast_vale <- forecast(model,level = c(95),h=15)
plot(forecast_vale)
