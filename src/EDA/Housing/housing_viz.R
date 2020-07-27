
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
library(plotly)

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

## Voucher data
vouchers <- readxl::read_excel(here("data", "original", "Housing", "housing_voucher_data.xlsx"))

auth_vouchers <- vouchers %>%
  pivot_longer(cols = contains("authorized_vouchers"), names_to = "year", names_prefix = "authorized_vouchers", values_to = "authorized_vouchers") %>%
  select(state, code, agency_name, year, authorized_vouchers)

num_using_vouchers <- vouchers %>%
  pivot_longer(cols = contains("num_families_using_vouchers"), names_to = "year", names_prefix = "num_families_using_vouchers", values_to = "num_families_using_vouchers") %>%
  select(state, code, agency_name, year, num_families_using_vouchers)

pct_vouchers_in_use <- vouchers %>%
  pivot_longer(cols = contains("Percent of authorized vouchers in use"), names_to = "year", names_prefix = "Percent of authorized vouchers in use", values_to = "pct_vouchers_in_use") %>%
  select(state, code, agency_name, year, pct_vouchers_in_use)

total_payments <- vouchers %>%
  pivot_longer(cols = contains("total_assistance_payments_owners"), names_to = "year", names_prefix = "total_assistance_payments_owners", values_to = "total_assistance_payments_owners") %>%
  select(state, code, agency_name, year, "total_assistance_payments_owners")

vouchers_long <- full_join(full_join(full_join(auth_vouchers, num_using_vouchers), pct_vouchers_in_use), total_payments)


###########

va_voucher_data %>%
  ggplot() +
  geom_area(aes(x = year, y = 1 - pct_vouchers_in_use, fill = agency_name), alpha = 0.2, position = "dodge") +
  geom_line(aes(x = year, y = 1 - pct_vouchers_in_use, color = agency_name), alpha = 1) +
  gghighlight(agency_name == "VHDA", use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.1, colour = alpha("black", 0.1))) +
  scale_fill_manual(values = c("VHDA" =  "#FC4444")) +
  scale_color_manual(values = c("VHDA" =  "#FC4444")) +
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


#####

## Not much variation in these variables unfortunately...
vouchers_long %>% filter(state == "Virginia") %>%
  ggplot() +
  geom_line(aes(x = year, y = authorized_vouchers, group = agency_name)) +
  theme(legend.position = "none")

vouchers_long %>% filter(state == "Virginia") %>%
  ggplot() +
  geom_line(aes(x = as.numeric(year), y = num_families_using_vouchers, group = agency_name)) +
  theme(legend.position = "none")

## Percent vouchers in use - only available at housing authority level
vouchers_long %>% filter(state == "Virginia") %>%
  ggplot() +
  geom_line(aes(x = year, y = pct_vouchers_in_use, group = agency_name, color = agency_name)) +
  gghighlight(agency_name == "VHDA") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position = "none")

## This isn't particularly useful without information on population within each housing authority.
## Probably easier to just use the subsidized household data
vouchers_long %>% filter(state == "Virginia") %>%
  ggplot() +
  geom_line(aes(x = year, y = total_assistance_payments_owners, group = agency_name)) +
  theme(legend.position = "none")
