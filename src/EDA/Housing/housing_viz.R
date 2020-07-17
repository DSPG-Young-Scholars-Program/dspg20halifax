
library(here)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(tidycensus)
library(ggplot2)
library(gghighlight)
library(ggmap)

halifax_border <- counties(state = "VA") %>% st_as_sf() %>% st_transform(crs = 4326) %>% filter(GEOID == "51083") %>% select(GEOID, NAMELSAD)

## Read data

lihtc <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean")) %>% as.data.frame()

## Subset to areas needed
lihtc_halifax <- lihtc %>% filter(GEOID == "51083")
lihtc_va <- lihtc %>% filter(str_detect(GEOID, "^(51)"))

lihtc_halifax_sf <- lihtc_halifax %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Vera
vera_data <- data.table::fread(here("data", "original", "Incarceration", "vera_incarceration_trends.csv")) %>% as.data.frame()
halifax_incarc <- vera_data %>% filter(county_name == "Halifax County", state == "VA")

## Vouchers
voucher_data <- readr::read_csv(here("data", "original", "Housing", "voucher_data_long.csv"))
va_voucher_data <- voucher_data %>% filter(state == "Virginia")


pub_housing <- readr::read_csv(here("data", "original", "Housing", "public_housing_data.csv")) %>% select(-X33)
pub_housing_halifax <- st_read(here("data", "original", "Housing", "halifax_pub_housing.geojson"))

###########

va_voucher_data %>%
  ggplot() +
  geom_area(aes(x = year, y = 1 - pct_vouchers_in_use, fill = agency_name), alpha = 0.2, position = "dodge") +
  geom_line(aes(x = year, y = 1 - pct_vouchers_in_use, color = agency_name), alpha = 1) +
  gghighlight(agency_name == "VHDA", use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.1, colour = alpha("black", 0.1))) +
  scale_fill_manual(values = c("VHDA" = "#04776c")) +
  scale_color_manual(values = c("VHDA" = "#04776c")) +
  coord_cartesian(ylim = c(0, 0.5), expand = FALSE) +
  labs(title = "Percent Unused Vouchers by Housing Authority: VA", x = "Year", y = "Percent Unused") +
  theme_minimal() +
  theme(legend.position = "none")

va_voucher_data %>%
  filter(year > 2007) %>%
  ggplot() +
  geom_area(aes(x = year, y = total_assistance_payments_owners / authorized_vouchers, fill = agency_name), alpha = 0.2, position = "dodge") +
  geom_line(aes(x = year, y = total_assistance_payments_owners / authorized_vouchers, color = agency_name), alpha = 1) +
  gghighlight(agency_name == "VHDA", use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.1, colour = alpha("black", 0.1))) +
  scale_fill_manual(values = c("VHDA" = "#04776c")) +
  scale_color_manual(values = c("VHDA" = "#04776c")) +
  #coord_cartesian(ylim = c(0, 0.5), expand = FALSE) +
  labs(title = "Funding per Voucher by Housing Agency", x = "Year", y = "Payments to owners per voucher") +
  theme_minimal() +
  theme(legend.position = "none")
# 
# 
# ggplot(halifax_incarc) +
#   geom_line(aes(x = year, y = total_jail_dis / 12)) +
#   geom_point(aes(x = year, y = total_jail_dis / 12))
