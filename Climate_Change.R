# Author: Vikram Mageshkumar
# Project Name: Climate Change Temperature Prediction


# Installing packet manager if it is not available
if (!require("pacman")) install.packages("pacman")

# Loading the packages 
pacman::p_load(magrittr, 
               forecast,
               rio,
               agricolae,
               tidyverse,
               timetk,
               corrplot,
               formattable,
               forecast)

library(timetk)
#' Converting data from char into Float
#'
#' @param data_v Column data of type character
#' @return Column data of dbl datatype
#' @author Vikram Mageshkumar
format_value <- function(data_v){
  formattable(data_v,format="f",digits=2)
}


# Reading Greenhouse Gas emission data from CSV
ghg_data_from_excel <- read.csv(file = "data/CAIT_2_Country_GHG_Emissions.csv",skip=1)


# Reading Country Economic data data from CSV
socio_economic_data <- read.csv(file = "data/CAIT_2_Country_Socio_Economic_Data.csv")


# Reading annual surface temperate change data from CSV
temp_change_data <- read.csv(file = "data/Annual_Surface_Temperature_Change.csv")

# Removing unwanted column from the temperature data
temp_change_data <- temp_change_data[,c(-1,-3,-4,-5,-6,-7,-8,-9,-10)]


#View(temp_change_data)

# Conversion of data from wide to long
# And removing the F from the year column
temp_data_wide_long <- temp_change_data %>% 
  pivot_longer(!Country,names_to = "Year",values_to = "Temperature") %>% 
  mutate(.,Year=str_replace_all(Year,pattern="F",replacement=""))

# Replacing the space with underscore(_) in the column name
names(socio_economic_data) <- socio_economic_data %>% gsub(x=names(socio_economic_data),
                                                           pattern="\\.",replacement="_")

names(ghg_data_from_excel)  <- ghg_data_from_excel %>% gsub(x=names(ghg_data_from_excel),
                                                            pattern="\\.",replacement="_")

# Replacing the NA values with 0. Most of the countries does not have 
# any values for the columns, So not able to take mean of it. 
# Because of that replaced NA with 0. 
ghg_data_from_excel[is.na(ghg_data_from_excel)] <- 0


# Subsetting only Country and Year Columns
colum_ghg <- ghg_data_from_excel[,c(1,2)]

# Removed the Country and year column. And formatting the 
# character value into Float (dbl) values
formatted_data  <- lapply(ghg_data_from_excel[,c(-1,-2)],format_value)

# Converting the array into data frame after joining 
# formatted data and the sub-setted country and year column
merged_data <- data.frame(colum_ghg,formatted_data)

# Merging formatted greenhouse gas emission data and 
# social economic data based on country and year
merged_data_emission_socio <- merge(merged_data,socio_economic_data, 
                                    by.x=c("Country","Year"),
                                    by.y=c("Country","Year"))


# Merging the merged_data_emission_socio and converted 
# temperature data based on Country and year
temp_emission_socio_data <- merge(merged_data_emission_socio,
                                   temp_data_wide_long,
                                   by.x=c("Country","Year"),
                                   by.y=c("Country","Year"))

View(temp_emission_socio_data)


# Creating a column character vector
colnames_value <- c("Country",
                    "Year",
                    "GghExclLandForestry",
                    "GhgInclLandForestry",
                    "COExclLandForestry",
                    "GhgEmissionCH4",
                    "GhgEmissionN2O",
                    "GhgEmissionFgas",
                    "GhgEmissionEnergy",
                    "GhgEmissionIndustrial",
                    "GhgEmissionAgri",
                    "GhgEmissionWaste",
                    "CO2EmissionLUCF",
                    "CO2EmissionBunkerFuels",
                    "CO2EmissionElectricity",
                    "CO2EmissionManufacturing",
                    "CO2EmissionTransportation",
                    "GghEmissionFuel",
                    "GhgEmissionFugitive",
                    "Population",
                    "GDPUSDMillion2005",
                    "GDPUSDMillion2005",
                    "Energy",
                    "Temperature")

 
# Renaming the columns with the 
# character vector that is created
names(temp_emission_socio_data) <- colnames_value

# Save the data in the RData format
save(x = temp_emission_socio_data,file = "data/Temp_Emission_Socio.RData")

# Write the data back to CSV as a backup data
write_csv(x = temp_emission_socio_data,file = "data/Temp_Emission_Socio.csv")

# Print the structure of the data
str(temp_emission_socio_data)

# Creating a character vector for the
# values that will be filtered
country_to_retrieve <- c("World")

# Retrieving the data which is available 
# in the country_to_retrieve vector
world_data <- as.data.frame(temp_emission_socio_data[temp_emission_socio_data$Country %in% 
                                                       country_to_retrieve,])

# Selecting Only Year and Temperature data
temp_year_data <- world_data %>% select(Year,Temperature)

# Select Year, Temperature, GhgInclLandForestry and 
# population from the temp_emission_socio_data
all_temp_year_data <- temp_emission_socio_data %>% select(Year,Temperature,
                                                          GhgInclLandForestry,
                                                          Population)

# Box Plot for Temperature (Outlier)
ggplot(temp_year_data,aes(x=Year,y=Temperature))+
  geom_boxplot(outlier.size=5)



############### Plotting the COR Plot ###############

# Removing the NA data
data_mat <- na.omit(temp_emission_socio_data)


# Unlisting the data which is numeric 
# and converting it as a matrix 
data_cor<- as.matrix(data_mat[,unlist(lapply(data_mat,is.numeric))])


# COR is calculated based on the pearson method 
corr_plot_values <- cor(data_cor,method = c("pearson"))


# CORRPLOT used to plot the graph based on 
# the COR value calculate in the previous step
corrplot(corr_plot_values, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)




# Performing ANOVA for the Linear model formula
aov_summary <- aov(formula = Temperature ~ Population + GhgInclLandForestry,
                   data = world_data)

# Gives the summary of the ANOVA results
summary(aov_summary)

# Plotting the ANOVA residuals in Histogram
hist(aov_summary$residuals)


################## Forecasting #################
time_series_data_population <- ts(world_data$Population,frequency = 1,start = c("1990"))
time_series_data_population

#### Building a Population Forecasting Model ####
population_model <- auto.arima(time_series_data_population,trace = TRUE)

# Plotting the data that does not fit into the model
plot.ts(population_model$residuals)

# Forecasting the population with 
# 95% as the confidence till the year 2100
population_forecasting <- forecast(population_model,level = c(95),h=89)

# Display the summary of the population forecasting
summary(population_forecasting)

# Plotting the forecasted value using autoplot
autoplot(population_forecasting,ylab="Population (billions)",xlab="Year") + 
  ggtitle(" World Population Growth Forecasting till 2100")

# Converted the forecasted value into tibble 
# to retrieve the 2100 Population value
time_series_value <- as.tibble(population_forecasting)
population_forecast_value_2100 <- time_series_value[nrow(time_series_value),]['Point Forecast']

#### Green House Gas Emission by Land and Forestry Forecasting Model ####

time_series_data_GhgInclLandForestry <- ts(world_data$GhgInclLandForestry,frequency = 1,start = c("1990"))
time_series_data_GhgInclLandForestry

#### Building a Green House Gas Forecasting Model ####
GhgInclLandForestry_model <- auto.arima(time_series_data_GhgInclLandForestry,trace = TRUE)

# Plotting the data that does not fit into the model
plot.ts(GhgInclLandForestry_model$residuals)

# Forecasting the Green House Gas Emission by Land and Forestry with 
# 95% as the confidence till the year 2100
GhgInclLandForestry_forecasting <- forecast(GhgInclLandForestry_model,level = c(95),h=89)

# Display the summary of the population forecasting
summary(GhgInclLandForestry_forecasting)

# Plotting the forecasted value using autoplot
autoplot(GhgInclLandForestry_forecasting,ylab="GhgInclLandForestry (CO2e)",xlab="Year") + 
  ggtitle("GhgInclLandForestry Emission Forecasting till 2100")

# Converted the forecasted value into tibble 
# to retrieve the 2100 Green House Gas Emission by Land and Forestry  value
GhgInclLandForestry_forecasting <- as.tibble(GhgInclLandForestry_forecasting)
GhgInclLandForestry_forecasting_for_2100<-GhgInclLandForestry_forecasting[nrow(GhgInclLandForestry_forecasting),]['Point Forecast']


# Training a Linear model with the dependent variable being the Temperature 
# and the independant variables are Population and GhgInclLandForestry  
# Equation : Tempeature = Population + GhgInclLandForestry
regression <- lm(Temperature ~ Population + GhgInclLandForestry,
                 data = all_temp_year_data) 

# Provides the summary of the 
# regression model
summary(regression)


# Preparing the data to predict the temperature
new_test <- data.frame(Population=as.numeric(population_forecast_value_2100),
                       GhgInclLandForestry=as.numeric(GhgInclLandForestry_forecasting_for_2100))

print(new_test)
# Predicting temperature based 
# on the model trained
predict(regression,newdata = new_test)

# Save the model in RData format
save(regression,file = "models/lm.RData")

