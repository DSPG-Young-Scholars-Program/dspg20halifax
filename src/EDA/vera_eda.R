
library(tidyr)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(plotly)

vera_data <- data.table::fread(here::here("data", "original", "Incarceration", "vera_incarceration_trends.csv")) %>% as.data.frame()

va_data <- vera_data %>% filter(state == "VA")
va_rural_data <- va_data %>% filter(urbanicity == "rural")

# ---- Jailing Rate ---- #

va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, male_jail_pop_rate, female_jail_pop_rate) %>%
  rename(Male = male_jail_pop_rate, Female = female_jail_pop_rate) %>%
  pivot_longer(names_to = "gender", cols = c("Male", "Female"))

plt <- ggplot(va_data_filt, aes(x = year, y = value, color = interaction(county_name, gender))) +
  geom_line() +
  scale_color_manual(values = c("#4E7E96", "#CA562C"), labels = c("Female", "Male")) +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
  facet_grid(~gender) +
  labs(color = "Gender", title = "Jailing Rate in Halifax County") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

plt

# ---- Prison Rate ---- #

scale_max <- max(c(va_data$male_jail_pop, va_data$female_jail_pop), na.rm = TRUE)

va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, male_jail_pop, female_jail_pop) %>%
  rename(Male = male_jail_pop, Female = female_jail_pop) %>%
  pivot_longer(names_to = "gender", cols = c("Male", "Female"))

plot_female <- va_data_filt %>% filter(gender == "Female") %>%
  ggplot(aes(x = year, y = value, group = county_name)) +
  geom_line(color = "#4E7E96") +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
  scale_y_continuous(limits = c(0, scale_max)) +
  labs(color = "Gender", title = "Female", y = "")  +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

plot_male <- va_data_filt %>% filter(gender == "Male") %>%
  ggplot(aes(x = year, y = value, group = county_name)) +
  geom_line(color = "#CA562C") +
  scale_y_continuous(limits = c(0, scale_max)) +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
  labs(color = "Gender", title = "Male") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

gridExtra::grid.arrange(plot_male, plot_female, ncol = 2, top = grid::textGrob("Jail Population in Halifax Co.", hjust = 0.5))

# ---- Prison Rate ---- #

scale_max <- max(c(va_data$male_prison_pop, va_data$female_prison_pop), na.rm = TRUE)

va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, male_prison_pop, female_prison_pop) %>%
  rename(Male = male_prison_pop, Female = female_prison_pop) %>%
  pivot_longer(names_to = "gender", cols = c("Male", "Female"))

plot_female <- va_data_filt %>% filter(gender == "Female") %>%
  ggplot(aes(x = year, y = value, group = county_name)) +
  geom_line(color = "#4E7E96") +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
  scale_y_continuous(limits = c(0, scale_max)) +
  labs(color = "Gender", title = "Female", y = "")  +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

plot_male <- va_data_filt %>% filter(gender == "Male") %>%
  ggplot(aes(x = year, y = value, group = county_name)) +
  geom_line(color = "#CA562C") +
  scale_y_continuous(limits = c(0, scale_max)) +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
  labs(color = "Gender", title = "Male") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

gridExtra::grid.arrange(plot_male, plot_female, ncol = 2, top = grid::textGrob("Prison Population in Halifax Co.", hjust = 0.5))

# ---- Prison Rate by Race ---- #


