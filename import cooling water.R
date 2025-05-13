library(readxl)
library(dplyr)
library(tidyverse)

vars <- c("Kreis", "wa_3", "cooling")
#2004
water_temp1 <- read_xls("Data_Non_Public_Supply_TLS/16102_2004_01.xls", sheet="Tab10", skip=9)%>%
  rename(Kreis=2, wa_3=7, cooling=8)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2004)
assign("cooling_2004", water_temp1)

#2007-2013
for (yrs in c("2007", "2010", "2013")) {
  water_temp1 <- read_xls(paste0("Data_Non_Public_Supply_TLS/16102_", yrs,"_01.xls"), sheet="Tab10", skip=9)%>%
    rename(Kreis=1, wa_3=9, cooling=10)%>%
    select(all_of(vars))%>%na.omit(Kreis)%>%
    mutate(year=as.numeric(yrs))
  assign(paste0("cooling_", yrs), water_temp1)
  
}

#2016

water_temp1 <- read_xls("Data_Non_Public_Supply_TLS/2016.xls", sheet="Tab10-2016", skip=9)%>%
  rename(Kreis=1, wa_3=9, cooling=10)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2016)
assign("cooling_2016", water_temp1)

#2019                        
water_temp1 <- read_xls("Data_Non_Public_Supply_TLS/2019.xls", sheet="Tab10-2019", skip=9)%>%
  rename(Kreis=1, wa_3=9, cooling=10)%>%
  select(all_of(vars))%>%na.omit(Kreis)%>%
  mutate(year=2019)
assign("cooling_2019", water_temp1)

cooling <- rbind(cooling_2004, cooling_2007,cooling_2010, cooling_2013, cooling_2016, cooling_2019 )

cooling <- cooling %>%
  mutate(across(c(wa_2, tot, one, multi, circ), ~ as.numeric(.)))
print(cooling)


# List of values to filter out
values_to_remove <- c("Thüringen", "10000", "30000", "50000", "100000", "300000", "500000", "1 Mill.", "3 Mill.", "unter 10 000", data = cooling)

# Filter out rows containing any of the unwanted values in the dataset
cooling_filtered <- cooling %>%
  filter(!apply(., 1, function(row) any(row %in% values_to_remove)))

# View the filtered dataset
head(cooling_filtered)

cooling <- cooling_filtered

write_csv(cooling, "cooling.csv")
