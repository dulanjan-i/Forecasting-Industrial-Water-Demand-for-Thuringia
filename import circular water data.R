library(readxl)
library(dplyr)
library(tidyverse)

vars <- c("Kreis", "wa_2", "tot", "one","multi", "circ")
#2004
water_temp <- read_xls("Data_Non_Public_Supply_TLS/16102_2004_01.xls", sheet="Tab8 ", skip=9)%>%
  rename(Kreis=1, wa_2=6, tot=7, one=8, multi=9, circ=10)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2004)
assign("water_2004", water_temp)

#2007-2013
for (yrs in c("2007", "2010", "2013")) {
  water_temp <- read_xls(paste0("Data_Non_Public_Supply_TLS/16102_", yrs,"_01.xls"), sheet="Tab8", skip=9)%>%
    rename(Kreis=1, wa_2=9, tot=10, one=11, multi=12, circ=13)%>%
    select(all_of(vars))%>%na.omit(Kreis)%>%
    mutate(year=as.numeric(yrs))
  assign(paste0("water_", yrs), water_temp)
  
}

#2016

water_temp <- read_xls("Data_Non_Public_Supply_TLS/2016.xls", sheet="Tab8", skip=9)%>%
  rename(Kreis=1, wa_2=9, tot=10, one=11, multi=12, circ=13)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2016)
assign("water_2016", water_temp)

#2019                        
water_temp <- read_xls("Data_Non_Public_Supply_TLS/2019.xls", sheet="Tab8_2019", skip=9)%>%
  rename(Kreis=1, wa_2=9, tot=10, one=11, multi=12, circ=13)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2019)
assign("water_2019", water_temp)

circular <- rbind(water_2004, water_2007,water_2010, water_2013, water_2016, water_2019 )

circular <- circular %>%
  mutate(across(c(wa_2, tot, one, multi, circ), ~ as.numeric(.)))
print(circular)


# List of values to filter out
values_to_remove <- c("Thüringen", "10000", "30000", "50000", "100000", "300000", "500000", "1 Mill.", "3 Mill.", "unter 10 000")

# Filter out rows containing any of the unwanted values in the dataset
circular_filtered <- circular %>%
  filter(!apply(., 1, function(row) any(row %in% values_to_remove)))

# View the filtered dataset
head(circular_filtered)

circular <- circular_filtered

