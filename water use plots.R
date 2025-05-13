library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
inddata <- read.csv("inddata.csv")

# Ensure proper data types
inddata <- inddata %>%
  mutate(year = as.numeric(as.character(year)))

# ---- GRAPH 1: Aggregate to State Level ----

# Aggregate data to state level
state_data <- inddata %>%
  group_by(year) %>%
  summarise(across(c(one, multi, circ, tot), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = c(one, multi, circ), names_to = "Usage_Type", values_to = "Value")

# Plot state-level aggregated data
state_level_plot <- ggplot(state_data, aes(x = year, y = Value, fill = Usage_Type)) +
  geom_area(alpha = 0.6, position = "stack") +
  labs(
    title = "Aggregated Water Usage Over Time (State Level)",
    x = "Year",
    y = "Total Usage (in units)",
    fill = "Usage Type"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  
print(state_level_plot)

# ---- GRAPH 2: Faceted by Kreis ----

# Reshape data for faceted plot
faceted_data <- inddata %>%
  select(year, Kreis, one, multi, circ, tot) %>%
  pivot_longer(cols = c(one, multi, circ), names_to = "Usage_Type", values_to = "Value")

# Plot faceted data by Kreis
faceted_plot <- ggplot(faceted_data, aes(x = year, y = Value, fill = Usage_Type)) +
  geom_area(alpha = 0.6, position = "stack") +
  labs(
    title = "Water Usage Over Time by Region (Kreis)",
    x = "Year",
    y = "Total Usage (in units)",
    fill = "Usage Type"
  ) +
  facet_wrap(~ Kreis, scales = "free_y") +  # Facet by Kreis
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Optional color scheme
print(faceted_plot)





# ---- GRAPH: Aggregated Water Usage with Water Withdrawals ----

# Aggregate data to state level and calculate total water withdrawals
state_data_with_wa <- inddata %>%
  group_by(year) %>%
  summarise(across(c(one, multi, circ, tot, wa), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = c(one, multi, circ), names_to = "Usage_Type", values_to = "Value")

# Plot state-level aggregated data with water withdrawals line
state_level_plot_with_wa <- ggplot(state_data_with_wa, aes(x = year)) +
  # Stacked area plot for one, multi, circ
  geom_area(aes(y = Value, fill = Usage_Type), alpha = 0.6, position = "stack") +
  # Line for water withdrawals
  geom_line(aes(y = wa, color = "Water Withdrawals"), size = 1, linetype = "dashed") +
  # Legend and labels
  labs(
    title = "Aggregated Water Usage Over Time with Water Withdrawals (State Level)",
    x = "Year",
    y = "Total Usage (in units)",
    fill = "Usage Type",
    color = "Additional Line"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = c("Water Withdrawals" = "blue"))  # Color for the line

print(state_level_plot_with_wa)
