
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(gghighlight)
library(tidyr)
library(leaflet)
library(sf)

## Spatial data for VA if needed
# va_counties <- tigris::counties(state = "VA", year = 2018, cb = TRUE, resolution = "20m") %>% 
#   st_as_sf() %>% 
#   st_transform(crs = 4326)

## Read in all public housing data and clean county strings
pub_housing_summary <- vroom::vroom(here("data", "original", "Housing", "county_pub_housing_2013_2019.csv")) %>%
  replace("NA", NA) %>%
  mutate(across(everything(), function(col) ifelse(col == -1 | col == -4 | col == -5, NA, col))) %>% ## -1 = NA, -4 = Suppressed, -5 = % reporting too low
  mutate(name = str_extract(str_replace_all(str_to_lower(name), "[0-9]{3} ", ""), "^([^,])+"), ## County names have numbers for some years, include state name after a comma for some years. Clean these
         name = str_to_title(trimws(ifelse(str_detect(name, "city county"), str_replace_all(name, "county", ""), name)))) ## Clean duplicated counties that are labeled as both city and county

## Pct of households headed by a single parent (most are females in Halifax - can show)
va_data <- pub_housing_summary %>%
  filter(year == 2019, program_label %in% c("Housing Choice Vouchers", "Project Based Section 8")) %>%
  mutate(isVA = ifelse(state == "VA", TRUE, FALSE),
         isHalifax = ifelse(state == "VA" & name == "Halifax County", TRUE, FALSE),
         pct_lt24_head = ifelse(is.na(pct_lt24_head), 100 - pct_age25_50 - pct_age51_61 - pct_age62plus, pct_lt24_head)) %>%
  filter(isVA == TRUE)
  
us_data <- pub_housing_summary %>%
  filter(year == 2019, program_label %in% c("Housing Choice Vouchers", "Project Based Section 8")) %>%
  mutate(isVA = ifelse(state == "VA", TRUE, FALSE),
         isHalifax = ifelse(state == "VA" & name == "Halifax County", TRUE, FALSE),
         pct_lt24_head = ifelse(is.na(pct_lt24_head), 100 - pct_age25_50 - pct_age51_61 - pct_age62plus, pct_lt24_head)) %>%
  filter(isVA == FALSE)
  
plot <- ggplot() +
  geom_point(data = us_data, aes(x = pct_black_nonhsp, y = pct_1adult, color = interaction(isVA, isHalifax), size = pct_lt24_head, alpha = isVA), stroke = 1.25) +
  geom_point(data = va_data, aes(x = pct_black_nonhsp, y = pct_1adult, color = interaction(isVA, isHalifax), size = pct_lt24_head, alpha = isVA), stroke = 1.25) +
  coord_flip() +
  scale_size_continuous(range = c(0, 20), labels = c("0%", "10%", "20%", "30%", "40%")) +
  scale_color_manual(labels = c("U.S.", "Virginia", "Halifax County"), values = c("#e4e0cd", "#6b6385", "#FC4444"), na.translate=FALSE) +
  labs(y = "\nPercent Single Adults", x = "Percent Black (Non-Hispanic)\n", 
       title = "Characteristics of Subsidized Housing Population", 
       size = "Houshold heads less \nthan 24 years old") +
  guides(alpha = FALSE, color = guide_legend(title = "Locale", override.aes = list(size=10)), size = guide_legend(override.aes = list(shape = 21, alpha = 0.8))) +
  scale_alpha_manual(values = c(0.3, 0.8)) +
  facet_wrap(~program_label, ncol = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, color = "gray10", size = 22), 
        plot.subtitle = element_text(hjust = 0.5, color = "gray30", face = "italic", size = 18),
        axis.title = element_text(size = 18, color = "gray10"),
        axis.text = element_text(size = 16, color = "gray30"),
        strip.text = element_text(size = 20, color = "gray30"),
        panel.spacing = unit(4, "lines"),
        legend.key.size = unit(3, "line"),
        legend.text = element_text(size = 14, color = "gray30"),
        legend.title = element_text(size = 20, color = "gray10"))

plot












## Want to confirm wether there is pattern to missingness:
# pub_housing_summary %>%
#   filter(year == 2019, program_label == "Summary of All HUD Programs") %>%
#   summarize(sum(is.na(pct_lt24_head)))

## Really want to compare to pct_black of county - get from ACS
p1 <- pub_housing_summary %>%
  filter(year == 2019) %>%
  mutate(pct_female_head_child = ifelse(pct_female_head_child == -1, NA, pct_female_head_child)) %>%
  mutate(isVA = ifelse(states == "VA Virginia", TRUE, FALSE),
         isHalifax = ifelse(states == "VA Virginia" & name == "Halifax County", TRUE, FALSE)) %>%
  ggplot() +
  geom_point(aes(x = pct_black_nonhsp, y = pct_female_head, color = interaction(isVA, isHalifax), size = pct_lt24_head, alpha = isVA), stroke = 1.25) +
  coord_flip() +
  scale_size_continuous(range = c(0, 20)) +
  #facet_wrap(~year) +
  scale_color_manual(values = c("#e4e0cd", "#6b6385", "#FC4444")) +
  scale_alpha_manual(values = c(0.3, 0.8)) +
  theme_minimal() +
  theme(legend.position = "none")

## Really want to compare to pct_black of county - get from ACS
## Percent of households headed by female that have children
p2 <- pub_housing_summary %>%
  filter(year == 2019) %>%
  mutate(pct_female_head_child = ifelse(pct_female_head_child == -1, NA, pct_female_head_child)) %>%
  mutate(isVA = ifelse(states == "VA Virginia", TRUE, FALSE),
         isHalifax = ifelse(states == "VA Virginia" & name == "Halifax County", TRUE, FALSE)) %>%
  ggplot() +
  geom_point(aes(x = pct_black_nonhsp, y = pct_female_head_child, color = interaction(isVA, isHalifax), size = pct_lt24_head, alpha = isVA), stroke = 1.25) +
  coord_flip() +
  scale_size_continuous(range = c(0, 20)) +
  #facet_wrap(~year) +
  scale_color_manual(values = c("#e4e0cd", "#6b6385", "#FC4444")) +
  scale_alpha_manual(values = c(0.3, 0.8)) +
  theme_minimal()

# gridExtra::grid.arrange(p1, p2, ncol = 2)

# plotly::ggplotly(p2)

## NEXT: GET RELATIVE RATES OF DEMOGRAPHICS COMPARING TO COUNTY VALUES
## ALSO SEE IF THIS CAN BE LINKED TO SCHOOL DATA - A STORY MAY START DEVELOPING?