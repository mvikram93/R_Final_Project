if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, pacman, rio, tidyverse, corrplot,formattable,
               forecast,
               caTools,
               ggplot2)
set.seed(1)

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

data_split <- sample.split(Y=merged_data_emission_socio$Population__People_,SplitRatio = 0.7)
print(data_split)
train_data <- merged_data_emission_socio[data_split,]
test_data <-merged_data_emission_socio[!data_split,]
dim(train_data)
dim(test_data)

write.csv(test_data,file = "data/test.csv",row.names = FALSE)
write.csv(train_data,file = "data/train.csv",row.names = FALSE)

train_data_val <- window(x=read.csv(file = "data/train.csv"))
test_data_val <- window(x=read.csv(file = "data/test.csv"))

View(train_data_val)

world_value <- train_data_val[train_data_val$Country == "World",]
world_data <- world_value$Population__People_
world_data

time_series <- ts(world_data,frequency = 1,start = c("1990"))
time_series

plot(time_series,xlab="Year",ylab="CO2 Emission")
glimpse(data_val)


model <- auto.arima(time_series,trace = TRUE)
plot.ts(model$residuals)
forecast_vale <- forecast(model,level = c(95),h=10)
autoplot(forecast_vale)
anova(model)
world_value_test <- test_data_val[test_data_val$Country == "World",]
#accuracy(forecast_vale,test=world_value_test$Population__People_)
min(world_value_test$Population__People_)
max(world_value_test$Population__People_)

ggplot()
