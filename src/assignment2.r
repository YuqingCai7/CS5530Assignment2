library(tidyverse)
library(fastDummies)
library(dplyr)

raw_car_data <- read.csv("C:/Users/yuqin/Desktop/CS5530Assignment2/data_raw/train.csv")
glimpse(raw_car_data)

missing_values <- colSums(is.na(raw_car_data))
glimpse(missing_values)

no_missing_data <- raw_car_data %>% 
  mutate_all(~replace_na(., median(., na.rm = TRUE))) %>% 
  mutate(New_Price = ifelse(is.na(New_Price), "NA", New_Price))
View(no_missing_data)

no_missing_data$Mileage <- as.numeric(gsub("[^0-9.]", "", no_missing_data$Mileage))
no_missing_data$Engine <- as.numeric(gsub("[^0-9.]", "", no_missing_data$Engine))
no_missing_data$Power <- as.numeric(gsub("[^0-9.]", "",no_missing_data$Power))
no_missing_data$New_Price <- as.numeric(gsub("[^0-9.]", "",no_missing_data$New_Price))
no_unit_data <- no_missing_data %>% 
  rename(`Engine(cc)` = `Engine`) %>% 
  rename(`Mileage(kmpl)` = `Mileage`) %>% 
  rename(`Power(bhp)` = `Power`) %>% 
  rename(`New_Price(Lakh)` = `New_Price`) %>% 
  rename(`Price(Lakh)` = `Price`)
View(no_unit_data)

one_hot_data <- dummy_cols(no_unit_data, select_columns = c("Fuel_Type", "Transmission"))
View(one_hot_data)

current_year <- year(Sys.Date())
more_feature_data <- one_hot_data %>% 
  mutate(Car_Age = current_year - Year)
View(more_feature_data)  


select_practice <- more_feature_data %>% 
  select(Name)
head(select_practice)

filter_practice <- more_feature_data %>% 
  filter(`Transmission` == "Automatic")
head(filter_practice)

rename_practice <- more_feature_data %>% 
  rename(`Transmission_Type` = `Transmission`)
head(rename_practice)

mutate_practice <- more_feature_data %>% 
  mutate(Price_Drop = `New_Price(Lakh)` - `Price(Lakh)`)
head(mutate_practice)

arrange_practice <- more_feature_data %>% 
  arrange(desc(Year))
head(arrange_practice)

summarize_practice <- more_feature_data %>% 
  group_by(Fuel_Type) %>% 
  summarize(mean = mean(`Price(Lakh)`))
head(summarize_practice)

write_csv(more_feature_data, "C:/Users/yuqin/Desktop/CS5530Assignment2/data_clean/clean_train.csv")

