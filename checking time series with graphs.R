library(ggplot2)
library(tidyr)
library(dplyr)

# Aggregate 'wa' by year to get annual totals
annual_totals <- inddata %>%
  group_by(year) %>%
  summarise(total_wa = sum(wa, na.rm = TRUE))

# Print aggregated data
print(annual_totals)

ggplot(data = annual_totals, aes(x = year, y = total_wa, group = 1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) +
  #geom_smooth(method = "lm", color = "green", se = FALSE, linetype = "dashed") + # Trend line
  labs(title = "Annual Total Industrial Water Withdrawals (2004-2019)",
       x = "Year",
       y = "Total Water Withdrawals (wa)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))








### CHECK TIME SERIES TRENDS ###

plot_ratios <- inddata %>%
  select(year, Kreis, one_ratio, multi_ratio, circ_ratio, wa) %>%
  group_by(year) %>%
  summarize(
    one_ratio = mean(one_ratio, na.rm = TRUE),
    multi_ratio = mean(multi_ratio, na.rm = TRUE),
    circ_ratio = mean(circ_ratio, na.rm = TRUE),
    wa = mean(wa, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(one_ratio, multi_ratio, circ_ratio, wa), 
               names_to = "variable", 
               values_to = "value")

# 2. Prepare data for plotting TC trends and "wa"
plot_TC <- inddata %>%
  select(year, Kreis, TC_one, TC_multi, TC_circ, wa) %>%
  group_by(year) %>%
  summarize(
    TC_one = mean(TC_one, na.rm = TRUE),
    TC_multi = mean(TC_multi, na.rm = TRUE),
    TC_circ = mean(TC_circ, na.rm = TRUE),
    wa = mean(wa, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(TC_one, TC_multi, TC_circ, wa), 
               names_to = "variable", 
               values_to = "value")

# 3. Plot ratios and "wa" over time
plot1 <- ggplot(plot_ratios, aes(x = year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Time Series Trend of Ratios and Water Withdrawals (wa)",
    x = "Year",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal()

# 4. Plot TC trends and "wa" over time
plot2 <- ggplot(plot_TC, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(
    title = "Time Series Trend of TC and Water Withdrawals (wa)",
    x = "Year",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal()

# Print plots
print(plot1)
print(plot2)


# Normalize the data
plot_ratios_normalized <- inddata %>%
  select(year, Kreis, one_ratio, multi_ratio, circ_ratio, wa) %>%
  group_by(year) %>%
  summarize(
    one_ratio = mean(one_ratio, na.rm = TRUE),
    multi_ratio = mean(multi_ratio, na.rm = TRUE),
    circ_ratio = mean(circ_ratio, na.rm = TRUE),
    wa = mean(wa, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(one_ratio, multi_ratio, circ_ratio, wa), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  mutate(value_normalized = (value - min(value, na.rm = TRUE)) / 
           (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)))

# Plot normalized data
plot_normalized <- ggplot(plot_ratios_normalized, aes(x = year, y = value_normalized, color = variable)) +
  geom_line(size = 1) +
  labs(
    title = "Time Series Trend (Normalized)",
    x = "Year",
    y = "Normalized Value",
    color = "Variable"
  ) +
  theme_minimal()

print(plot_normalized)

####### ALTERNATIVE REGRESSION APPROACHES ######

library(mgcv)
sapply(inddata[, c("log_gva", "mean_temp", "TC_circ")], function(x) length(unique(x)))

gam_model <- gam(wa ~ s(log_gva) + mean_temp + s(TC_circ), data = inddata)
summary(gam_model)
plot(gam_model, pages = 1, shade = TRUE, rug = TRUE, 
     seWithMean = TRUE, main = "Smooth Terms in GAM Model")
