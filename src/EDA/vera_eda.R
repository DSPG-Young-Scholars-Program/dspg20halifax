
library(tidyr)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(plotly)
library(stringr)

vera_data <- data.table::fread(here::here("data", "original", "Incarceration", "vera_incarceration_trends.csv")) %>% as.data.frame()

va_data <- vera_data %>% filter(state == "VA")
va_rural_data <- va_data %>% filter(urbanicity == "rural")

# ---- Jail Rate ---- #

## Wrap plot in function just to make calling it easier
jail_prison_plot <- function() {
  
  scale_max <- max(c(va_data$male_jail_pop_rate, va_data$female_jail_pop_rate), na.rm = TRUE)
  
  va_data_jail <- va_data %>% 
    filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
    select(year, county_name, male_jail_pop_rate, female_jail_pop_rate) %>%
    rename(Male = male_jail_pop_rate, Female = female_jail_pop_rate) %>%
    pivot_longer(names_to = "gender", cols = c("Male", "Female"))
  
  jail_plot_female <- va_data_jail %>% filter(gender == "Female") %>%
    ggplot(aes(x = year, y = value, group = county_name)) +
    geom_line(color = "#214C62") +
    gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
    scale_y_continuous(limits = c(0, scale_max)) +
    labs(color = "Gender", title = "Female", y = "", x = "")  +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  jail_plot_male <- va_data_jail %>% filter(gender == "Male") %>%
    ggplot(aes(x = year, y = value, group = county_name)) +
    geom_line(color = "#CA562C") +
    scale_y_continuous(limits = c(0, scale_max)) +
    gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
    labs(color = "Gender", title = "Male", y = "", x = "") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  jail_plot <- gridExtra::grid.arrange(jail_plot_male, jail_plot_female, ncol = 2, top = "Jail and Prison Incarceration Rates in Halifax Co.\n", left = "Jail Population per 100,000")
  
  # ---- Prison Rate ---- #
  
  scale_max <- max(c(va_data$male_prison_pop_rate, va_data$female_prison_pop_rate), na.rm = TRUE)
  
  va_data_prison <- va_data %>% 
    filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
    select(year, county_name, male_prison_pop_rate, female_prison_pop_rate) %>%
    rename(Male = male_prison_pop_rate, Female = female_prison_pop_rate) %>%
    pivot_longer(names_to = "gender", cols = c("Male", "Female"))
  
  prison_plot_female <- va_data_prison %>% filter(gender == "Female") %>%
    ggplot(aes(x = year, y = value, group = county_name)) +
    geom_line(color = "#214C62") +
    gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
    scale_y_continuous(limits = c(0, scale_max)) +
    labs(y = "", x = "")  +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  prison_plot_male <- va_data_prison %>% filter(gender == "Male") %>%
    ggplot(aes(x = year, y = value, group = county_name)) +
    geom_line(color = "#CA562C") +
    scale_y_continuous(limits = c(0, scale_max)) +
    gghighlight(county_name == "Halifax County", use_direct_label = FALSE) +
    labs(y = "", x = "") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  prison_plot <- gridExtra::grid.arrange(prison_plot_male, prison_plot_female, ncol = 2, bottom = "Year", left = "Prison population per 100,000")
  
  return(gridExtra::grid.arrange(jail_plot, prison_plot))
  
}

jail_prison_plot()


# ---- Jail + Prison Rate by Race ---- #

## Create list of column names to extract from data
## Note all races not currently included because of low population counts for aapi, latinx, native
#races <- c("aapi", "black", "latinx", "native", "white")
races <- c("black", "white")

race_cols <- c(paste(races, "jail_pop_rate", sep = "_"), paste(races, "prison_pop_rate", sep = "_"))

## Filter out counties/cities with seemingly duplicate data and convert to long format
va_data_filt <- va_data %>% 
  filter(!county_name %in% c("Hopewell city", "Charles City County", "Colonial Heights city")) %>%
  select(year, county_name, race_cols) %>%
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
plot_data %>% filter(year >= 1990, county_name == "Halifax County") %>% 
  ggplot() + 
  geom_line(aes(x = year, y = value, group = interaction(incarceration_type, race), color = incarceration_type), alpha = 1, size = 0.7) +
  geom_line(aes(x = year, y = med, group = interaction(incarceration_type, race), color = incarceration_type), linetype = "dashed", alpha = 0.5, size = 0.7) +
  scale_color_manual(values = c("#CA562C", "#214C62")) +
  labs(y = "Incarceration rate per 100,000", x = "Year", color = "System", title = "Incarceration Rates in Halifax Co.", subtitle = "Dashed lines represent Virginia median rate") +
  facet_grid(~race) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "gray30", size = 14))


# ---- Prison Admissions Rates ---- #

## Not sure how much there will be in here.

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



