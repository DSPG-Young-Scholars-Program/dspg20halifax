
library(tidyr)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(plotly)
library(stringr)
library(purrr)

vera_data <- data.table::fread(here::here("data", "original", "Incarceration", "vera_incarceration_trends.csv")) %>% as.data.frame()

va_data <- vera_data %>% filter(state == "VA")
va_rural_data <- va_data %>% filter(urbanicity == "rural")

## General look at distributions

## General right skew for all population/rate variables
## Missing values for VA for prison admissions and populations
## Same across levels though
va_data %>%
  select(contains("white")) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

va_data %>%
  select(contains("black")) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# ---- Jail Rate ---- #

scale_max <- max(c(va_data$male_prison_pop_rate, va_data$female_prison_pop_rate), na.rm = TRUE)

rates_by_gender_system <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, male_jail_pop_rate, female_jail_pop_rate, male_prison_pop_rate, female_prison_pop_rate) %>%
  #rename(Male = male_prison_pop_rate, Female = female_prison_pop_rate) %>%
  pivot_longer(names_to = "jail_gender_combo", cols = c("male_jail_pop_rate", "female_jail_pop_rate", "male_prison_pop_rate", "female_prison_pop_rate")) %>%
  mutate(system = ifelse(str_detect(jail_gender_combo, "jail"), "Jail", "Prison"),
         gender = ifelse(str_detect(jail_gender_combo, "female"), "Female", "Male"),
         gender = factor(gender, levels = c("Male", "Female")))

jail_plot <- rates_by_gender_system %>%
  filter(system == "Jail") %>%
  ggplot(aes(x = year, y = value, group = county_name, color = gender)) +
  geom_line() +
  scale_color_manual(values = c("#214C62", "#CA562C")) +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE, calculate_per_facet = TRUE) +
  scale_y_continuous(limits = c(0, scale_max)) +
  labs(color = "Gender", title = "", subtitle = "", y = "Incarcerated Individuals per 100,000\n", x = "\nYear")  +
  facet_wrap(gender~.) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, color = "gray10", size = 22), 
        plot.subtitle = element_text(hjust = 0.5, color = "gray30", face = "italic", size = 18),
        strip.text = element_text(size = 20, color = "gray30"),
        axis.text = element_text(size = 16, color = "gray30"),
        axis.title = element_text(size = 18, color = "gray10"),
        legend.position = "none")

prison_plot <- rates_by_gender_system %>%
  filter(system == "Prison") %>%
  ggplot(aes(x = year, y = value, group = county_name, color = gender)) +
  geom_line() +
  scale_color_manual(values = c("#214C62", "#CA562C")) +
  gghighlight(county_name == "Halifax County", use_direct_label = FALSE, calculate_per_facet = TRUE) +
  scale_y_continuous(limits = c(0, scale_max)) +
  labs(color = "Gender", title = "Incarceration Rate in Halifax County", subtitle = "Relative to other Virginia Counties", y = "Incarcerated Individuals per 100,000\n", x = "\nYear")  +
  facet_wrap(gender~.) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, color = "gray10", size = 22), 
        plot.subtitle = element_text(hjust = 0.5, color = "gray30", face = "italic", size = 18),
        strip.text = element_text(size = 20, color = "gray30"),
        axis.text = element_text(size = 16, color = "gray30"),
        axis.title = element_text(size = 18, color = "gray10"),
        legend.position = "none")

#ggsave(here::here("static", "findings", "incarceration_page_files", "jail_incarceration_rate.png"), plot = jail_plot)
#ggsave(here::here("static", "findings", "incarceration_page_files", "prison_incarceration_rate.png"), plot = prison_plot)

# ---- Jail + Prison Rate by Race ---- #

## Create list of column names to extract from data
## Note all races not currently included because of low population counts for aapi, latinx, native
#races <- c("aapi", "black", "latinx", "native", "white")
races <- c("black", "white")

race_cols <- c(paste(races, "jail_pop_rate", sep = "_"), paste(races, "prison_pop_rate", sep = "_"))

## Filter out counties/cities with seemingly duplicate data and convert to long format
va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, all_of(race_cols)) %>%
  pivot_longer(names_to = "race", cols = race_cols) %>%
  mutate(incarceration_type = case_when(str_detect(race, "jail") ~ "Jail",
                                        str_detect(race, "prison") ~ "Prison"),
         race = case_when(str_detect(race, "white") ~ "White",
                          str_detect(race, "black") ~ "Black"))
# str_detect(race, "aapi") ~ "Asian American/Pacific Islander",
# str_detect(race, "latinx") ~ "Latinx",
# str_detect(race, "native") ~ "Native American"))

plot_data <- va_data_filt %>% 
  group_by(year, race, incarceration_type) %>% 
  mutate(med = median(value, na.rm = TRUE))

## Subset to post-1990 (no data before) and Halifax
## Plot of jail and prison population rate over time in Halifax against VA median grouped by race
race_incarceration_plot <- plot_data %>% filter(year >= 1990, county_name == "Halifax County") %>% 
  ggplot() + 
  geom_line(aes(x = year, y = value, group = interaction(incarceration_type, race), color = incarceration_type), alpha = 1, size = 0.7) +
  geom_line(aes(x = year, y = med, group = interaction(incarceration_type, race), color = incarceration_type), linetype = "dashed", alpha = 0.5, size = 0.7) +
  scale_color_manual(values = c("#CA562C", "#214C62")) +
  labs(y = "Incarceration rate per 100,000", x = "Year", color = "System", title = "Incarceration Rates in Halifax Co.", subtitle = "Dashed lines represent Virginia median rate") +
  facet_grid(~race) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, color = "gray10", size = 22), 
        plot.subtitle = element_text(hjust = 0.5, color = "gray30", face = "italic", size = 18),
        axis.title = element_text(size = 18, color = "gray10"),
        axis.text = element_text(size = 16, color = "gray30"),
        strip.text = element_text(size = 20, color = "gray30"),
        panel.spacing = unit(4, "lines"),
        legend.key.size = unit(3, "line"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20))

#ggsave(here::here("static", "findings", "incarceration_page_files", "race_incarceration_plot.png"), plot = race_incarceration_plot)


# ---- Prison Admissions Rates ---- #

## Not sure how much there is in here - looks pretty noisy

#races <- c("aapi", "black", "latinx", "native", "white")
races <- c("black", "white")

race_cols <- paste(races, "prison_adm_rate", sep = "_")

va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, race_cols) %>%
  pivot_longer(names_to = "race", cols = race_cols) %>%
  mutate(race = case_when(str_detect(race, "white") ~ "White",
                          str_detect(race, "black") ~ "Black"))

plot_data <- va_data_filt %>% 
  group_by(year, race) %>% 
  mutate(med = median(value, na.rm = TRUE))

## Subset to post-1990 (no data before) and Halifax
## Plot of jail and prison population rate over time in Halifax against VA median grouped by race
plot_data %>% filter(year >= 1990, county_name == "Halifax County") %>% 
  ggplot() + 
  geom_line(aes(x = year, y = value, color = race), alpha = 1) +
  geom_line(aes(x = year, y = med, color = race), linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("#CA562C", "#214C62"))


## Not much jumping out when investigating ratio of black jail population to total jail population
## Most other variables are either confounded with this (both related to population, other factors) or completely unrelated
vera_data %>%
  filter(region == "South", year == 2017, black_jail_pop / total_jail_pop < 1,  total_jail_pretrial / total_jail_pop < 1) %>%
  ggplot() +
  geom_point(aes(x = black_jail_pop / total_jail_pop, y = log(total_jail_pop), color = urbanicity, size = total_pop), alpha = 0.8) +
  scale_size_continuous(range = c(1, 15)) +
  #coord_cartesian(ylim = c(0,1.5), xlim = c(0,1.5)) +
  #geom_smooth(aes(x = black_jail_pop / total_jail_pop, y = total_jail_pretrial / total_jail_pop, color = urbanicity, size = total_pop), alpha = 0.7, se=FALSE) +
  gghighlight(state == "VA", unhighlighted_params = list(alpha = 0.3))
