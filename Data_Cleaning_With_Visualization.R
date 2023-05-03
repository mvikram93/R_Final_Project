# Name: Vikram Mageshkumar


if (!require("pacman")) install.packages("pacman")

pacman::p_load(magrittr, 
               pacman, 
               rio,
               agricolae,
               tidyverse, 
               corrplot,
               formattable,
               forecast,
               caTools,
               ggplot2,
               psych,
               outliers,
               PerformanceAnalytics)


format_value <- function(data_v){
  formattable(data_v,format="f",digits=2)
}


ghg_data_from_excel <- read.csv(file = "data/CAIT_2_Country_GHG_Emissions.csv",skip=1)

socio_economic_data <- read.csv(file = "data/CAIT_2_Country_Socio_Economic_Data.csv")

temp_change_data <- read.csv(file = "data/Annual_Surface_Temperature_Change.csv")

temp_change_data <- temp_change_data[,c(-1,-3,-4,-5,-6,-7,-8,-9,-10)]

View(temp_change_data)

temp_data_wide_long <- temp_change_data %>% 
  pivot_longer(!Country,names_to = "Year",values_to = "Temperature") %>% 
  mutate(.,Year=str_replace_all(Year,pattern="F",replacement=""))

names(socio_economic_data) <- socio_economic_data %>% gsub(x=names(socio_economic_data),
                                                           pattern="\\.",replacement="_")

names(ghg_data_from_excel)  <- ghg_data_from_excel %>% gsub(x=names(ghg_data_from_excel),
                                                            pattern="\\.",replacement="_")

ghg_data_from_excel[is.na(ghg_data_from_excel)] <- 0

colum_ghg <- ghg_data_from_excel[,c(1,2)]

formatted_data  <- lapply(ghg_data_from_excel[,c(-1,-2)],format_value)

merged_data <- data.frame(colum_ghg,formatted_data)

merged_data_emission_socio <- merge(merged_data,socio_economic_data, 
                                    by.x=c("Country","Year"),
                                    by.y=c("Country","Year"))

temp_emission_socio_data <- merge(merged_data_emission_socio,
                                   temp_data_wide_long,
                                   by.x=c("Country","Year"),
                                   by.y=c("Country","Year"))

View(temp_emission_socio_data)


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

names(temp_emission_socio_data) <- colnames_value

View(temp_emission_socio_data)

save(x = temp_emission_socio_data,file = "data/Temp_Emission_Socio.RData")

write_csv(x = temp_emission_socio_data,file = "data/Temp_Emission_Socio.csv")

str(temp_emission_socio_data)

country_to_retrieve <- c("World")

world_data <- as.data.frame(temp_emission_socio_data[temp_emission_socio_data$Country %in% 
                                                       country_to_retrieve,])
temp_year_data <- world_data %>% select(Year,Temperature)

all_temp_year_data <- temp_emission_socio_data %>% select(Year,Temperature,
                                                          GhgInclLandForestry,
                                                          Population)
View(all_temp_year_data)

#Box Plot for Temperature (Outlier)
ggplot(temp_year_data,aes(x=Year,y=Temperature))+
  geom_boxplot(outlier.size=5)

regression <- lm(Temperature ~ Population + GhgInclLandForestry,data = all_temp_year_data) 
summary(regression)

new_test <- data.frame(Population=10541000000,GhgInclLandForestry=45557.80961)

predict(regression,newdata = new_test)
save(regression,file = "models/lm.RData")



data_mat <- na.omit(temp_emission_socio_data)
data_cor<- as.matrix(data_mat[,unlist(lapply(data_mat,is.numeric))])

corr_plot_values <- cor(data_cor,method = c("pearson"))
corrplot(corr_plot_values, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)


ggplot(world_data) +
  aes(x="",y="Population") +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

test <- grubbs.test(world_data$Population)
print(test)


aov_summary <- aov(formula = Temperature ~ Population + GhgInclLandForestry,
                   data = world_data)
summarise(regression)

hist(aov_summary$residuals)

qqnorm(aov_summary$residuals)
qqline(aov_summary$residuals)

HSD.test(aov_summary,"Population", group = FALSE)$comparison
