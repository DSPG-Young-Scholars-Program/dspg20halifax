
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(gghighlight)
library(tidyr)

va_pub_housing_summary <- data.table::fread(here("data", "original", "Housing", "county_pub_housing_2013_2019.csv")) %>% 
  filter(state == "VA", year > 2012) %>%
  replace("NA", NA) %>%
  as.data.frame()

hud_programs <- va_pub_housing_summary %>% 
  filter(program_label == "Summary of All HUD Programs")

va_pub_housing_summary %>%
  filter(!is.na(total_units), name == "Halifax County", pct_occupied > 0) %>%
  mutate(total_units = as.numeric(total_units)) %>%
  group_by(program_label, name, year) %>%
  mutate(tot_occupied = total_units * pct_occupied / 100, tot_unoccupied = (1 - pct_occupied / 100) * total_units) %>%
  filter(year > 2013) %>%
  pivot_longer(cols = c("tot_unoccupied", "tot_occupied"), names_to = "occupation_status") %>%
  mutate(occupation_status = factor(occupation_status, levels = c("tot_unoccupied", "tot_occupied"))) %>%
  ggplot() +
  geom_bar(aes(x = reorder(program_label, -value), y = value, fill = occupation_status), color = "#E7E6DC", stat = "identity", alpha = 0.9) +
  scale_fill_manual(values = c("#d8d3bc", "#6b6385")) +
  facet_wrap(~year) +
  theme_minimal()




