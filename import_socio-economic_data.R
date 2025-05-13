library(readxl)
library(dplyr)
library(sf)
library(writexl)
library(ggplot2)
library(readr)
library(plm)

#### IND WATER DEMAND MAIN MODEL ########
### DATA PREPROCESSING #######

#which one has read_html

vars <- c("Kreis", "bet_no", "wa", "eigeng", "fremdb","public", "andbet")
#2004
inddata_temp <- read_xls("Data_Non_Public_Supply_TLS/16102_2004_01.xls", sheet="Tab2", skip=9)%>%
  rename(Kreis=1, bet_no=3, wa=4, eigeng=5, fremdb=6, public=7, andbet=8)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2004)
assign("inddata_2004", inddata_temp)

#2007-2013
for (yrs in c("2007", "2010", "2013")) {
  inddata_temp <- read_xls(paste0("Data_Non_Public_Supply_TLS/16102_", yrs,"_01.xls"), sheet="Tab2", skip=9)%>%
    rename(Kreis=1, bet_no=6, wa=7, eigeng=8, fremdb=9, public=10, andbet=11)%>%
    select(all_of(vars))%>%na.omit(Kreis)%>%
    mutate(year=as.numeric(yrs))
  assign(paste0("inddata_", yrs), inddata_temp)
  
}

#2016-2019
for (yrs in c("2016", "2019")) {
inddata_temp <- read_xlsx(paste0("Data_Non_Public_Supply_TLS/16102_",yrs, "_00.xlsx"), sheet=paste0("Tab2_", yrs), skip=11)%>%#
  rename(Kreis=1, bet_no=6, wa=7, eigeng=8, fremdb=9, public=10, andbet=11)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
    mutate(year=as.numeric(yrs))
assign(paste0("inddata_", yrs), inddata_temp)
}

inddata<-rbind(inddata_2004,inddata_2007, inddata_2010, inddata_2013, inddata_2016,  inddata_2019)



#### WEATHER DATA #####

# URL of the text file
url <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/air_temperature_mean/regional_averages_tm_year.txt"
# Download the file
download.file(url, destfile = "regional_averages_tm_year.txt", mode = "wb")


# Read the file into a table
data <- read.table("regional_averages_tm_year.txt", header = TRUE, sep = ";", skip = 1)
# Display the first few rows of the table
weather_reg<-data%>%
  mutate(year=as.numeric(Jahr), mean_temp=Thueringen)%>%
  select(year, mean_temp)

###### GVA DATA #######

#import GVA Data
for (yrs in c("2004", "2007", "2010", "2013", "2016", "2019")) {
  gva_temp <- read_xlsx(paste0("Data_GVA_TLS/", yrs, ".xlsx"), skip=31) %>%#
    rename(Kreis=1, gva_ind_be=6, gva_ind_bf=5)%>%
    mutate(year=as.numeric(yrs))%>%
    select(Kreis, year, gva_ind_be, gva_ind_bf)%>%na.omit(Kreis)
  assign(paste0("gva_", yrs), gva_temp)
}

gvadata<-rbind(gva_2004,gva_2007, gva_2010, gva_2013, gva_2016,  gva_2019)





#Merge

inddata<-left_join(inddata, weather_reg)
inddata<-left_join(inddata, gvadata)
#

inddata<-inddata%>%
  filter(!Kreis=="Thüringen")

# List of Kreis values to filter out
kreis_to_exclude <- c("Wartburgkreis", "Saalfeld-Rudolstadt", "Saale-Orla-Kreis", "Nordhausen", "Greiz")
inddata_small<-inddata%>%
  filter(!Kreis %in% kreis_to_exclude)


#### Calculations #####
##

inddata<-inddata%>%
  mutate(wa_b=as.numeric(wa)/as.numeric(bet_no),
         log_wa_b=log(wa_b),
         log_gva=log(gva_ind_be))

###
ggplot(inddata, aes(x=year, y=wa_b))+
  geom_point()+
  facet_wrap(~Kreis)+
  scale_y_continuous(limits = c(0, 200))

###
ggplot(inddata, aes(x=log_gva, y=log_wa_b))+
  geom_point(aes(color=Kreis))+
  #facet_wrap(~Kreis, scales="free")+
  geom_smooth(method = "lm", se = FALSE) # Add correlation line without confidence intervals
  

unique(inddata_small$Kreis)
###Aufällige Kreis
#Wartburgkreis, Saalfeld-Rudolstadt, Saale-Orla-Kreis, Nordhausen, Greiz




## save data file ####
saveRDS(inddata, "inddata.rds")

######7
#### Stats ######


library(plm)

model <- plm(wa_b ~ gva_ind_be, data=inddata, index=c("Kreis"), model="within")
model_small <- plm(wa_b ~ gva_ind_be, data=inddata_small, index=c("Kreis"), model="within")
model_simple <- lm(wa_b ~ gva_ind_be, data=inddata)
model_log <- plm(log_wa_b ~ log_gva + mean_temp, data=inddata, index=c("Kreis"), model="within")


summary_model <- capture.output(summary(model))
summary_model_small <- capture.output(summary(model_small))
summary_model_simple <- capture.output(summary(model_simple))
summary_model_log <- capture.output(summary(model_log))


writeLines(summary_model, "base_model.txt")
writeLines(summary_model_small, "base_model_small.txt")
writeLines(summary_model_simple, "model_simple.txt")
writeLines(summary_model_log, "model_log.txt")



# writeLines(summary_model1, "stat_analysis/base_dem_model1.txt")
# writeLines(summary_model2, "stat_analysis/base_dem_model2.txt")
# writeLines(summary_model3, "stat_analysis/base_dem_model3.txt")

### Cleaning and filling missing values ###

#replace the - with a 0

library(tidyverse)

inddata <- inddata %>%
  mutate(across(where(is.character), ~str_replace(., "-", "0")))

#export as csv for reference
write.csv(inddata, "inddata.csv", row.names = FALSE)

#replacing "."  values using the mean imputation method 

library(dplyr)
library(readr)

head(inddata)
str(inddata)

# Replace '.' with NA in the 'wa' column
inddata$wa[inddata$wa == "."] <- NA

# Convert 'wa' to numeric type
inddata$wa <- as.numeric(inddata$wa)

### running the panel regression for the dataset

library(plm)
# Convert 'Kreis' and 'year' into factors
inddata$Kreis <- as.factor(inddata$Kreis)
inddata$year <- as.factor(inddata$year)

pdata <- pdata.frame(inddata, index = c("Kreis", "year"))

#fixed effect model 
fe_model <- plm(wa ~ log_gva,
                data = pdata, 
                model = "within")
summary(fe_model)

#random effect model
re_model <- plm(wa ~ log_gva, 
                data = pdata, model = "random")
summary(re_model)


# Hausman test to test the optimum panel reg. 
## if p < 0.05, the fixed effects model is preferred

hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

#since p val = 0.7111, the random reffect model is preffred.

# running panel VAR (PAnel variance)
#install.packages("panelvar")
library(panelvar)
library(tseries)
library(plm)
pdata <- pdata.frame(inddata, index = c("Kreis", "year"))

# Fit Panel VAR model with lag 1 (Ind Variables = bet_no, mean temp, gva)

pvar_model <- pvargmm(dependent_vars = c("wa", "mean_temp", "bet_no", "gva_ind_be", "gva_ind_bf"),
                      lags = 1,
                      transformation = "fod",  
                      data = pdata,
                      panel_identifier = c("Kreis", "year"),
                      steps = 14)
# Summary of the model
summary(pvar_model)

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
# calculating the TC using the "andbet" which is how much ind wastewater was recycled by company's own treatment plant

#replace missing values with na and convert column to numeric
inddata <- inddata %>%
  mutate(
    multi = ifelse(is.na(multi), 0, multi),
    circ = ifelse(is.na(circ), 0, circ)
  )
# Replace '.' with NA in the 'andbet' column
inddata$andbet[inddata$andbet == "."] <- NA

# Convert 'andbet' to numeric type
inddata$andbet <- as.numeric(inddata$andbet)

#### TC calculation ####
inddata <- inddata %>%
  group_by(Kreis) %>%  
  arrange(year) %>%   
  mutate(
    TC1 = (andbet - lag(andbet)) / lag(andbet) * 100  # Calculate TC directly
  ) %>%
  ungroup() 

print(head(inddata))

##### Calculating YoY change of "wa" - for verification of why the TC is negative
inddata <- inddata %>%
  group_by(Kreis) %>%  
  arrange(year) %>%   
  mutate(
    YoYWA = (wa - lag(wa)) / lag(wa) * 100  # Calculate TC directly
  ) %>%
  ungroup() 

## check for relationships
#remove infinite values
inddata$TC[inddata$TC == "Inf"] <- NA
inddata$YoYWA [inddata$YoYWA == "Inf"] <- NA
inddata$TC <- as.numeric(inddata$TC)
inddata$YoYWA <- as.numeric(inddata$YoYWA)

# Remove rows with NA values in TC or YoYWA
inddata_clean <- inddata %>%
   filter(!is.na(TC) & !is.na(YoYWA))

yoytrend <- lm(YoYWA ~ TC, data = inddata_clean)
 summary(yoytrend)
 
ggplot(inddata_clean, aes(x = TC, y = YoYWA)) +
   geom_point() +                        # Scatter plot of the data points
   geom_smooth(method = "lm", col = "red") +  # Add regression line in red
   labs(title = "Regression of YoYWA on TC", x = "Technological Change (TC)", y = "Year-over-Year Water Withdrawals (YoYWA)") +
   theme_minimal()  # Optional: a cleaner theme
 
#conclusion: andbet cannot be used as a TC as it gives negative results

#Approach 2 ####
#Calculating TC based on Water Intensity WI

### Water intensity (WI) calculation ####
inddata <- inddata %>%
 group_by(Kreis)  %>%
  arrange(year)  %>%
  mutate(
    WI = wa / gva_ind_bf,
    TC2 = -((WI - lag(WI)) / lag(WI) * 100) # used the (-) sign to convert the neg tc values to positive to indicate a positive TC with reduced watwer usage
  ) %>%
  ungroup()

inddata <- inddata %>%
  rename(TC_WI = TC2)

library(plm)
fixed_model_WI <- plm(wa ~ log_gva + TC_WI, data = inddata, model = "within", index = c("Kreis", "year"))
random_model_WI <- plm(wa ~ log_gva + mean_temp + TC_WI, data = inddata, model = "random", index = c("Kreis", "year"))  

summary(fixed_model_WI)
summary(random_model_WI)
  
#### CONSIDERING TC2#### CONSIDERING INVESTMENTS ON ENV PROTECTION ####
#setwd("/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/UFZ/Industrial_demand")
#getwd
#list.files
#envdata <- read.csv("/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/UFZ/Industrial_demand/env_inv.csv")
#head(envdata)
#save(envdata, file = "envdata.RData")

load("envdata.RData") #this contains the list of investments on environment by companies in Thuringia excl construction 
envdata <- envdata %>%
  arrange(Year) %>%
  mutate(TC = (envtot - lag(envtot)) / lag(envtot) * 100)

envdata <- envdata %>%
  rename(TC_Env = TC)

##### CALCULATION OF TC BASED ON CIRCULAR USE OF WATER ####
#DATASEET IS CREARTED IN ANOTHER R SCRIPT AND IMPORTED TO HERE AS AN RDS
#The two datasets "inddata" (previous) and "circular" 

circular$year2 <- circular$year
circular <- circular %>% select(-year)

write.csv(inddata, file = "inddata.csv", row.names = FALSE)
write.csv(circular, file = "circular.csv", row.names = FALSE)

inddata2 <- read.csv("inddata.csv")
# +++++++++++++++++++++++++++PANEL DATASET CONVERSION AND CLEANING+++++++++++++++++++++++++++++++++++

# Data cleaning
# Impute missing values in the 'wa' column proportionally to 'bet_no'
inddata <- inddata %>%
  group_by(Kreis) %>%
  mutate(
    wa = ifelse(is.na(wa), 
                mean(wa, na.rm = TRUE) * (bet_no / mean(bet_no, na.rm = TRUE)), 
                wa)
  ) %>%
  ungroup()

inddata <- inddata2
inddata$Kreis <- as.factor(inddata$Kreis)
inddata$year <- as.factor(inddata$year)

# Create a panel data frame
inddata <- pdata.frame(inddata, index = c("Kreis", "year"))
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Use multi and circ directly in the model

# Replace NA with 0 in multi and circ columns; as they are really 0 and not missing values

model <- lm(wa ~ log_gva + mean_temp + one + multi + circ, data = inddata)
summary(model)

# Calculate the efficiency ratios and their YoY changes
inddata <- inddata %>%
  arrange(year, Kreis) %>% 
  mutate(
    # Calculate ratios
    one_ratio = one / tot,        # Ratio of one-time use
    multi_ratio = multi / tot,    # Ratio of multiple reuse
    circ_ratio = circ / tot,      # Ratio of circular use
    
    # Year-over-Year (YoY) changes for each ratio
    TC_one = (one_ratio - lag(one_ratio)) / lag(one_ratio) * 100,
    TC_multi = (multi_ratio - lag(multi_ratio)) / lag(multi_ratio) * 100,
    TC_circ = (circ_ratio - lag(circ_ratio)) / lag(circ_ratio) * 100
  )

head(inddata)

model2 <- lm(wa ~ log_gva + mean_temp + one_ratio + multi_ratio + circ_ratio, data = inddata)
summary(model2)

#check the relationship of circular use in the model
model3 <- lm(wa ~ log_gva + mean_temp + TC_one + TC_multi + TC_circ, data = inddata)
summary(model3)

library(plm)
fixed_model_circ <- plm(wa ~ log_gva + circ_ratio, data = inddata, model = "within", index = c("Kreis", "year"))
random_model_circ <- plm(wa ~ log_gva + mean_temp + TC_circ, data = inddata, model = "random", index = c("Kreis", "year"))


summary(fixed_model_circ)

####### calculating the simple growth function for circular water use it in the main model

# Impute missing values in the 'wa' column proportionally to 'bet_no'
inddata <- inddata %>%
  group_by(Kreis) %>%
  mutate(
    wa = ifelse(is.na(wa), 
                mean(wa, na.rm = TRUE) * (bet_no / mean(bet_no, na.rm = TRUE)), 
                wa)
  ) %>%
  ungroup()
inddata <- inddata %>%
  mutate(
    multi = ifelse(is.na(multi), 0, multi),
    circ = ifelse(is.na(circ), 0, circ)
  )

inddata <- inddata %>%
  mutate(
    wa = as.numeric(wa),  
    year = as.numeric(year),           
    Kreis = as.factor(Kreis),
    circ = as.numeric(circ),
    gva_ind_be = as.numeric(gva_ind_be),
    gva_ind_bf = as.numeric(gva_ind_bf),
    log_gva = as.numeric(log_gva)
  )
str(inddata)

inddata <- pdata.frame(inddata, index = c("Kreis", "year"))
str(inddata)

inddata$year <- as.numeric(as.character(inddata$year))
inddata$Kreis <- as.character(inddata$Kreis)

#Plot time series of water use (wa)

library(ggplot2)
ggplot(inddata, aes(x = year, y = wa, color = Kreis)) +
  geom_line() +
  labs(title = "Time Series of Total Water Use", x = "Year", y = "Water Use") +
  theme_minimal()

# Plot time series of circular use (circ) 

ggplot(inddata, aes(x = year, y = circ, color = Kreis, group = Kreis)) +
  geom_line() +
  labs(title = "Time Series of Circular Water Use", x = "Year", y = "Circular Use (circ)") +
  theme_minimal()

# Since above plot is too noisy, aggregate circular to State level
state_level <- inddata %>%
  group_by(year) %>%
  summarize(circular_use_total = sum(circ, na.rm = TRUE))

ggplot(data = state_level, aes(x = year, y = circular_use_total, group = 1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) +
  #geom_smooth(method = "lm", color = "green", se = FALSE, linetype = "dashed") + # Trend line
  labs(title = "Aggregated Circular water use (2004-2019)",
       x = "Year",
       y = "Aggregated Circular use") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Fit growth models
# Linear model
linear_model <- lm(circular_use_total ~ as.numeric(as.character(year)), data = state_level)
summary(linear_model)

# Exponential model
exp_model <- lm(log(circular_use_total) ~ as.numeric(as.character(year)), data = state_level)
summary(exp_model)

#exponential transformation
growth_rate <- exp(coef(exp_model)[2]) - 1
print(growth_rate)

#model_test <- plm(wa ~ circ, data = inddata, model = "within", index = c("Kreis", "year"))
#summary(model_test)


#model_test_2 <- plm(wa_2 ~ circ, data = inddata, model = "within", index = c("Kreis", "year"))
#summary(model_test_2)

#Add the TC value to all Kreis for all years in the dataset
inddata$TC <- 507.6
inddata$TC <- as.numeric(inddata$TC)

#Calculating a Time varying TC (TCT)
# Define the base year and growth rate
base_year <- 2004  # Starting year of the dataset
growth_rate <- 507.6  # Linear growth rate per year

# Calculate time-based TC
inddata$TCT <- growth_rate * (as.numeric(as.character(inddata$year)) - base_year)


model_test <- plm(wa ~ log_gva + mean_temp + circ + TCT, data = inddata, model = "within", index = c("Kreis", "year"))
summary(model_test)

model_test_a <- lm(wa ~ log_gva + mean_temp + circ + TCT, data = inddata)
summary(model_test_a)

model_test2 <- plm(wa ~ log_gva + mean_temp + circ + TCT, data = inddata, model = "random", index = c("Kreis", "year"))
summary(model_test2)


#omit panel structure and run the model again
inddata2 <- read_csv("inddata.csv")
inddata2$TC <- 507.6
inddata2$TC <- as.numeric(inddata2$TC)
inddata2$TCT <- growth_rate * (as.numeric(as.character(inddata$year)) - base_year)

model_linear <- lm(wa ~ log_gva + mean_temp + circ + TC, data = inddata2)
summary(model_linear)


model_circ <- plm(wa ~ log_gva + mean_temp + circ, data= inddata, model = "random")
summary(model_circ)


#### VISUALIZATION of the IWW model (model_circ)
library(ggplot2)
library(broom)

# Extract coefficients with confidence intervals
coef_data <- tidy(model_circ, conf.int = TRUE)

# Plot the coefficients
ggplot(coef_data, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Coefficient Plot", x = "Predictor", y = "Estimate") +
  coord_flip() +
  theme_minimal()

inddata$predicted <- predict(model_circ, inddata)

ggplot(inddata2, aes(x = predicted, y = wa)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  labs(title = "Observed vs Predicted Values", x = "Predicted Values", y = "Observed Values") +
  theme_minimal()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Water use graph per no of use #### check "water use plots.R"


### COOLING MODEL ####

#import weather data
weather <- readRDS("kreis_weather_industry.RDS")

#import inddata again to avoid errors
inddata <- read_csv("inddata.csv")

#since weather data is from 2010 onwards.....
# split inddata into to subsets and use the inddata for 2010-2019 to climate analysis
# as imputing weather data would induce a lot of error

#subset dataset from 2010 onwards

inddata_2010_19 <- inddata %>%
  filter(year >= 2010)

#Modify the names to mach the same format

weather <- weather %>%
  dplyr::rename(Kreis = NUTS3_n) 

weather <- weather %>%
  mutate(Kreis = case_when(
    Kreis == "Eisenach" ~ "Stadt Eisenach",
    Kreis == "Erfurt" ~ "Stadt Erfurt",
    Kreis == "Gera" ~ "Stadt Gera",
    Kreis == "Jena" ~ "Stadt Jena",
    Kreis == "Suhl" ~ "Stadt Suhl",
    Kreis == "Weimar" ~ "Stadt Weimar",
    Kreis == "Ilm-Kreis" ~ "Ilm0Kreis",
    Kreis == "Saale-Orla-Kreis" ~ "Saale0Orla-Kreis",
    Kreis == "Saale-Holzland-Kreis" ~ "Saale0Holzland-Kreis",
    Kreis == "Saalfeld-Rudolstadt" ~ "Saalfeld0Rudolstadt",
    Kreis == "Schmalkalden-Meiningen" ~ "Schmalkalden0Meiningen",
    Kreis == "Unstrut-Hainich-Kreis" ~ "Unstrut0Hainich-Kreis",
    TRUE ~ Kreis  # Keep other values unchanged
  ))

#merge new weather data
inddata_weather <- left_join(inddata_2010_19, weather, by = c("year", "Kreis"))

#import cooling water
cooling <- read_csv("cooling.csv")

#subset and add cooling into the dataframe
cooling_2010_19 <- cooling %>%
  filter(year >= 2010)

inddata_weather <- left_join(inddata_weather, cooling_2010_19, by = c("year", "Kreis"))

#preprocessing to remove non numerics
inddata_weather$cooling[inddata_weather$cooling %in% c(".", "-", "", "NA")] <- NA
inddata_weather$cooling <- as.numeric(inddata_weather$cooling)
inddata_weather <- inddata_weather %>%
  filter(!is.na(cooling))

inddata_weather$Kreis <- as.factor(inddata_weather$Kreis)
inddata_weather$year <- as.factor(inddata_weather$year)

#convert to panel dataframe
inddata_weather_panel <- pdata.frame(inddata_weather, index = c("Kreis", "year"))

# Construct the cooling model
#Panel regression 
model_cooling1 <- plm(cooling ~ summer_TXK, 
                     data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_cooling1)

model_cooling2 <- plm(cooling ~ summer_TMK, 
                     data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_cooling2)

model_cooling3 <- plm(cooling ~ hot_days25, 
                     data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_cooling3)

model_cooling4 <- plm(cooling ~ hot_days30, 
                     data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_cooling4)

model_cooling5 <- plm(cooling ~ mean_temp, 
                     data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_cooling5)

#conclusion: we will check for each var separately and then we will decide which one is the most suitable for this model

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#convert the inddata into panel dataframe
inddata$wa <- as.numeric(inddata$wa)
inddata$wa_2 <- as.numeric(inddata$wa_2)
inddata$tot <- as.numeric(inddata$tot)
inddata$one <- as.numeric(inddata$one)
inddata$multi <- as.numeric(inddata$multi)
inddata$circ <- as.numeric(inddata$circ)

inddata$Kreis <- as.factor(inddata$Kreis)
inddata$year <- as.factor(inddata$year)

inddata_panel <- pdata.frame(inddata, index = c("Kreis", "year"))

### CIRCULAR WATER USE MODEL ####
model_circular <- plm(wa_2 ~ one + multi + circ,                                 #here wa_2 is "Verfügbare Wasser-menge"
                      data = inddata_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_circular)
#do not put water use in expl vars.We use the subset of data kickcout some and keep some, 
#what you kickout is the districts with multi and circular
#so we have districts with singular water use, 
# check the industrial producton on water withdrawal...why do the model? what do you need to expalin, 
#create an intepretationof the model. like why dowe needit to not.

### INDUSTRIAL WATER WITHDRAWAL MODEL ####

model_iww <- plm(wa ~ log_gva, data = inddata_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_iww)


model_betno <- plm((wa/bet_no) ~ log_gva, data = inddata_panel, model = "pooling", index = c("Kreis", "year"))
summary(model_betno)
#divide wa by bet_no and do the model.

model_gva <- plm(log_gva ~ bet_no, data = inddata_panel, model = "within", index = c("Kreis", "year"))
summary(model_gva)

#### WATER USE DATA SPLIT ####
#LOGIC: Classifying Kreis into low or high circular water use based on the top 25% singular-use threshold.
# Per year #
#inddata <- inddata %>%
#  group_by(year) %>%
#  mutate(singular_threshold = quantile(one/tot, 0.75, na.rm = TRUE), #top 25%
#         low_circular = ifelse((one / tot) >= singular_threshold, "LOW_C", "HIGH_C")) %>%
#  ungroup()

# Extract unique Kreis names for each category
#low_circular_kreis <- inddata %>%
#  filter(low_circular == "LOW_C") %>%
#  distinct(Kreis) %>%
#  pull(Kreis)

#high_circular_kreis <- inddata %>%
#  filter(low_circular == "HIGH_C") %>%
#  distinct(Kreis) %>%
#  pull(Kreis)

# Print the Kreis names for both categories
#cat("LOW_C:\n", paste(low_circular_kreis, collapse = ", "), "\n\n")
#cat("HIGH_C:\n", paste(high_circular_kreis, collapse = ", "), "\n")
inddata <- inddata %>%
  select(- circular_use.x) %>%
  select(- circular_use.xx)

### Aggregagte and check ###

# Aggregate total water usage per Kreis over all years
# Aggregate total water usage per Kreis

# Step 6: Check if the split worked correctly
cat("Number of observations in LOW_C subset:", nrow(low_circular_data), "\n")
cat("Number of observations in HIGH_C subset:", nrow(high_circular_data), "\n")

head(low_circular_data)
head(high_circular_data)

#convert to panel datasets
low_circular_panel <- pdata.frame(low_circular_data, index = c("Kreis", "year"))
high_circular_panel <- pdata.frame(high_circular_data, index = c("Kreis", "year"))

# Run regression models separately
low_circular_model <- plm(wa ~ log_gva , data = low_circular_panel, model = "pooling")
high_circular_model <- plm(wa ~ log_gva , data = high_circular_panel, model = "pooling")

# Compare results
summary(low_circular_model)
summary(high_circular_model)

#### SAVING ALL REGRESSION RESULTS ####
#install.packages("modelsummary")
#install.packages("pandoc")
library(pandoc)
library(modelsummary)

model_list <- list("IWW" = model_iww, "Cooling1" = model_cooling1, "Cooling2" = model_cooling2, 
                   "Cooling3" = model_cooling3, "Cooling4" = model_cooling4, "Cooling5" = model_cooling5,
                   "Low Circular" = low_circular_model, "High Circular" = high_circular_model, "Multi" = multi_model)

# Print table in console
modelsummary(model_list)

# Save to Word
modelsummary(model_list, output = "regression_results_multi.docx")

#### DISPLAY ALL MODELS ####
summary(model_iww)
summary(model_cooling1)
summary(model_cooling2)
summary(model_cooling3)
summary(model_cooling4)
summary(model_cooling5)
summary(low_circular_model)
summary(high_circular_model)

multi_model <- plm(cooling ~ log_gva + hot_days30, data = inddata_weather_panel, model = "pooling")
summary(multi_model)
modelsummary(multi_model)

#### BASE YEAR APPROACH ####
# Selected base year: 2004 ; start year of the dataset
# Calculate the mean_log_gva2005 
# standrdized log_gva

inddata <- inddata %>%
  group_by(Kreis) %>%
  mutate(
    log_gva_2004 = log_gva[year == 2004],
    log_gva_std  = log_gva - log_gva_2004
  ) %>%
  ungroup()

# creating a log_wa column as well
inddata <- inddata %>%
  mutate(log_wa = log(wa))

#save R dataframe
saveRDS(inddata, file = "inddata.rds")
write.csv(inddata, "inddata_final.csv", row.names = FALSE)

#convert to panel dataset 
#convert the inddata into panel dataframe
inddata$wa <- as.numeric(inddata$wa)
inddata$wa_2 <- as.numeric(inddata$wa_2)
inddata$tot <- as.numeric(inddata$tot)
inddata$one <- as.numeric(inddata$one)
inddata$multi <- as.numeric(inddata$multi)
inddata$circ <- as.numeric(inddata$circ)
inddata$log_gva_2004 <- as.numeric(inddata$log_gva_2004)
inddata$log_gva_std <- as.numeric(inddata$log_gva_std)
inddata$log_wa <- as.numeric(inddata$log_wa)
inddata$Kreis <- as.factor(inddata$Kreis)
inddata$year <- as.factor(inddata$year)

inddata_panel <- pdata.frame(inddata, index = c("Kreis", "year"))

# do the same for the low circualr and high circular dataframes and save them as
# RDS and also as panel dataframes
# LOW 

low_circular_data <- low_circular_data %>%
  group_by(Kreis) %>%
  mutate(
    log_gva_2004 = log_gva[year == 2004],
    log_gva_std  = log_gva - log_gva_2004
  ) %>%
  ungroup()

# creating a log_wa column as well
low_circular_data <- low_circular_data %>%
  mutate(log_wa = log(wa))

#save R dataframe
saveRDS(low_circular_data, file = "low_circular.rds")
write.csv(inddata, "low_circular.csv", row.names = FALSE)

#convert to panel dataset 
#convert the inddata into panel dataframe
low_circular_data$wa <- as.numeric(low_circular_data$wa)
low_circular_data$wa_2 <- as.numeric(low_circular_data$wa_2)
low_circular_data$tot <- as.numeric(low_circular_data$tot)
low_circular_data$one <- as.numeric(low_circular_data$one)
low_circular_data$multi <- as.numeric(low_circular_data$multi)
low_circular_data$circ <- as.numeric(low_circular_data$circ)
low_circular_data$log_gva_2004 <- as.numeric(low_circular_data$log_gva_2004)
low_circular_data$log_gva_std <- as.numeric(low_circular_data$log_gva_std)
low_circular_data$log_wa <- as.numeric(low_circular_data$log_wa)
low_circular_data$Kreis <- as.factor(low_circular_data$Kreis)
low_circular_data$year <- as.factor(low_circular_data$year)

low_circular_data_panel <- pdata.frame(low_circular_data, index = c("Kreis", "year"))

# HIGH

high_circular_data <- high_circular_data %>%
  group_by(Kreis) %>%
  mutate(
    log_gva_2004 = log_gva[year == 2004],
    log_gva_std  = log_gva - log_gva_2004
  ) %>%
  ungroup()

# creating a log_wa column as well
high_circular_data <- high_circular_data %>%
  mutate(log_wa = log(wa))

#save R dataframe
saveRDS(low_circular_data, file = "high_circular.rds")
write.csv(inddata, "high_circular.csv", row.names = FALSE)

#convert to panel dataset 
#convert the inddata into panel dataframe
high_circular_data$wa <- as.numeric(high_circular_data$wa)
high_circular_data$wa_2 <- as.numeric(high_circular_data$wa_2)
high_circular_data$tot <- as.numeric(high_circular_data$tot)
high_circular_data$one <- as.numeric(high_circular_data$one)
high_circular_data$multi <- as.numeric(high_circular_data$multi)
high_circular_data$circ <- as.numeric(high_circular_data$circ)
high_circular_data$log_gva_2004 <- as.numeric(high_circular_data$log_gva_2004)
high_circular_data$log_gva_std <- as.numeric(high_circular_data$log_gva_std)
high_circular_data$log_wa <- as.numeric(high_circular_data$log_wa)
high_circular_data$Kreis <- as.factor(high_circular_data$Kreis)
high_circular_data$year <- as.factor(high_circular_data$year)

high_circular_data_panel <- pdata.frame(high_circular_data, index = c("Kreis", "year"))

#### FINAL SET OF REGRESSIONS ####
# Regression models with log_wa vs log_gva_std

iww_log_OLS <- plm(log_wa ~ log_gva_std, data = inddata_panel, model = "pooling", index = c("Kreis", "year"))
iww_log_fixed <- plm(log_wa ~ log_gva_std, data = inddata_panel, model = "within", index = c("Kreis", "year"))

low_log_OLS <- plm(log_wa ~ log_gva_std, data = low_circular_data_panel, model = "pooling", index = c("Kreis", "year"))
low_log_fixed <- plm(log_wa ~ log_gva_std, data = low_circular_data_panel, model = "within", index = c("Kreis", "year"))

high_log_OLS <- plm(log_wa ~ log_gva_std, data = high_circular_data_panel, model = "pooling", index = c("Kreis", "year"))
high_log_fixed <- plm(log_wa ~ log_gva_std, data = high_circular_data_panel, model = "within", index = c("Kreis", "year"))

summary(iww_log_OLS)
summary(iww_log_fixed)
summary(low_log_OLS)
summary(low_log_fixed)
summary(high_log_OLS)
summary(high_log_fixed)

model_list2 <- list("IWW OLS" = iww_log_OLS, "IWW Fixed" = iww_log_fixed,
                    "Low Circ OLS" = low_log_OLS, "Low Circ Fixed" = low_log_fixed,
                    "High Circ OLS" = high_log_OLS, "High Circ Fixed" = high_log_fixed)
modelsummary(model_list2)

modelsummary(model_list2, output = "Log WA vs log GVA reg.docx")

#### COOLING MODELS WITHOUT LOG TRANSFORMATON ####

# Cooling models with log_gva 
model_cooling1_OLS <- plm(cooling ~ summer_TXK + log_gva, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling1_fixed <- plm(cooling ~ summer_TXK + log_gva, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling2_OLS <- plm(cooling ~ summer_TMK + log_gva, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling2_fixed <- plm(cooling ~ summer_TMK + log_gva, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling3_OLS <- plm(cooling ~ hot_days25 + log_gva, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling3_fixed <- plm(cooling ~ hot_days25 + log_gva, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling4_OLS <- plm(cooling ~ hot_days30 + log_gva, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling4_fixed <- plm(cooling ~ hot_days30 + log_gva, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling5_OLS <- plm(cooling ~ mean_temp + log_gva, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling5_fixed <- plm(cooling ~ mean_temp + log_gva, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

#model summaries

summary(model_cooling1_OLS)
summary(model_cooling1_fixed)
summary(model_cooling2_OLS)
summary(model_cooling2_fixed)
summary(model_cooling3_OLS)
summary(model_cooling3_fixed)
summary(model_cooling4_OLS)
summary(model_cooling4_fixed)
summary(model_cooling5_OLS)
summary(model_cooling5_fixed)

# model list
model_list_cooling <- list("Cooling TXK OLS" = model_cooling1_OLS,
                           "Cooling TXK Fixed" = model_cooling1_fixed,
                           "Cooling TMK OLS" = model_cooling2_OLS,
                           "Cooling TMK Fixed" = model_cooling2_fixed,
                           "Cooling HD25 OLS" = model_cooling3_OLS,
                           "Cooling HD25 Fixed" = model_cooling3_fixed,
                           "Cooling HD30 OLS" = model_cooling4_OLS,
                           "Cooling HD30 Fixed" = model_cooling4_fixed,
                           "Cooling Mean T OLS" = model_cooling5_OLS,
                           "Cooling Mean T Fixed" = model_cooling5_fixed)

modelsummary(model_list_cooling)
modelsummary(model_list_cooling, output = "All cooling models.docx")


#### DESCRIPTIVE STATS OF GVA ####

# Descriptive Statistics for the GVA column
GVA_stats <- inddata_panel %>%
  summarise(
    mean_GVA = mean(gva_ind_be, na.rm = TRUE),
    median_GVA = median(gva_ind_be, na.rm = TRUE),
    sd_GVA = sd(gva_ind_be, na.rm = TRUE),
    min_GVA = min(gva_ind_be, na.rm = TRUE),
    max_GVA = max(gva_ind_be, na.rm = TRUE)
  )

print(GVA_stats)


selected_year <- 2004

gva_year <- inddata %>% 
  filter(year == selected_year)

ggplot(gva_year, aes(x = reorder(Kreis, gva_ind_be), y = gva_ind_be)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = paste("Distribution of GVA Across Kreis in", unique(gva_year$year)),
       x = "Kreis",
       y = "Gross Value Added (GVA)") +
  theme_minimal()

# Aggregate GVA across all years by Kreis
aggregated_gva <- inddata %>%
  group_by(Kreis) %>%
  summarise(total_gva = sum(gva_ind_be, na.rm = TRUE)) %>%
  arrange(desc(total_gva))

# Print summary statistics
summary_gva <- aggregated_gva %>%
  summarise(
    mean_gva = mean(total_gva),
    median_gva = median(total_gva),
    sd_gva = sd(total_gva),
    min_gva = min(total_gva),
    max_gva = max(total_gva)
  )

print(summary_gva)

ggplot(aggregated_gva, aes(x = reorder(Kreis, total_gva), y = total_gva)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Aggregated GVA Distribution Across Kreis (All Years)",
       x = "Kreis",
       y = "Total GVA") +
  theme_minimal()

#### Base year calculation for the Cooling models ####
#2010 as the base year

inddata_weather <- inddata_weather %>%
  group_by(Kreis) %>%
  mutate(
    log_gva_2010 = log_gva[year == 2010],
    log_gva_std  = log_gva - log_gva_2010
  ) %>%
  ungroup()

inddata_weather <- inddata_weather %>%
  mutate(log_cooling = log(cooling))

inddata_weather<- inddata_weather %>%
  filter(is.finite(log_cooling))

#save dataset
saveRDS(inddata_weather, file = "inddata_weather.rds")
write.csv(inddata_weather,"inddata_weather.csv", row.names = FALSE)

#convert to panel dataset
inddata_weather$cooling <- as.numeric(inddata_weather$cooling)
inddata_weather$log_cooling <- as.numeric(inddata_weather$log_cooling)
inddata_weather$log_gva_std <- as.numeric(inddata_weather$log_gva_std)
inddata_weather$Kreis <- as.factor(inddata_weather$Kreis)
inddata_weather$year <- as.factor(inddata_weather$year)

inddata_weather_panel <- pdata.frame(inddata_weather, index = c("Kreis", "year"))

#perform the same analysis as above


# Cooling models with log_gva and log cooling
model_cooling1_OLS_log <- plm(log_cooling ~ summer_TXK + log_gva_std, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling1_fixed_log <- plm(log_cooling ~ summer_TXK + log_gva_std, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling2_OLS_log <- plm(log_cooling ~ summer_TMK + log_gva_std, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling2_fixed_log <- plm(log_cooling ~ summer_TMK + log_gva_std, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling3_OLS_log <- plm(log_cooling ~ hot_days25 + log_gva_std, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling3_fixed_log <- plm(log_cooling ~ hot_days25 + log_gva_std, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling4_OLS_log <- plm(log_cooling ~ hot_days30 + log_gva_std, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling4_fixed_log <- plm(log_cooling ~ hot_days30 + log_gva_std, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

model_cooling5_OLS_log <- plm(log_cooling ~ mean_temp + log_gva_std, 
                          data = inddata_weather_panel, model = "pooling", index = c("Kreis", "year"))

model_cooling5_fixed_log <- plm(log_cooling ~ mean_temp + log_gva_std, 
                            data = inddata_weather_panel, model = "within", index = c("Kreis", "year"))

#model summaries

summary(model_cooling1_OLS_log)
summary(model_cooling1_fixed_log)
summary(model_cooling2_OLS_log)
summary(model_cooling2_fixed_log)
summary(model_cooling3_OLS_log)
summary(model_cooling3_fixed_log)
summary(model_cooling4_OLS_log)
summary(model_cooling4_fixed_log)
summary(model_cooling5_OLS_log)
summary(model_cooling5_fixed_log)

# model list
model_list_cooling_log <- list("Log Cooling TXK OLS" = model_cooling1_OLS_log,
                               "Log Cooling TXK Fixed" = model_cooling1_fixed_log,
                               "Log Cooling TMK OLS" = model_cooling2_OLS_log,
                               "Log Cooling TMK Fixed" = model_cooling2_fixed_log,
                               "Log Cooling HD25 OLS" = model_cooling3_OLS_log,
                               "Log Cooling HD25 Fixed" = model_cooling3_fixed_log,
                               "Log Cooling HD30 OLS" = model_cooling4_OLS_log,
                               "Log Cooling HD30 Fixed" = model_cooling4_fixed_log,
                               "Log Cooling Mean T OLS" = model_cooling5_OLS_log,
                               "Log Cooling Mean T Fixed" = model_cooling5_fixed_log)

modelsummary(model_list_cooling_log)
modelsummary(model_list_cooling_log, output = "Log 2010 cooling models.docx")

# CONCLUSION OF LOG TRANSFORMATION AND BASE YEAR STD
#The log transformation on cooling water and also standardizing the log gva for the base year 2010
#did not yield any significant result and in return actually the reuslts which were significant in the previous instant
#were also turned out to be non significant

#Therefore: Log transformation of cooling water is not advisable in this dataset (inddata weather) for future analysis




