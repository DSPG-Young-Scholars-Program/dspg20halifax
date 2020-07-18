
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

halifax_offenses <- readr::read_csv(here("data", "original", "Crime", "halifax_offenses_combined.csv"))

# all_dates <- expand.grid(incident_date = seq.Date(as.Date("2004-01-01"), as.Date("2020-06-30"), by = "days"), ori = c("VA0410000", "VA1240000", "VA041SP00", "VA0410100"))
all_mo_yr <- expand.grid(incident_month = seq(1, 12), incident_year = seq(2009, 2019)) %>% bind_rows(data.frame(incident_month = seq(1,6), incident_year = 2020))

halifax_offenses <- halifax_offenses %>%
  mutate(incident_year = year(incident_date),
         incident_month = month(incident_date),
         incident_day = day(incident_date)) %>%
  filter(incident_year > 2008) ## Trivial amounts of data in previous years makes any data points seem questionable


mo_yr_counts <- halifax_offenses %>%
  group_by(incident_month, incident_year) %>%
  count() %>%
  right_join(all_mo_yr) %>%
  mutate(n = ifelse(is.na(n), 0, n))

ggplot(mo_yr_counts) +
  geom_line(aes(x = as.factor(incident_month), y = n, color = as.factor(incident_year), group = incident_year)) +
  geom_point(aes(x = as.factor(incident_month), y = n, color = as.factor(incident_year)), alpha = 0.5)

halifax_offenses %>%
  group_by(incident_date) %>%
  count() %>%
  ggplot() +
  geom_line(aes(x = incident_date, y = n))

ggplot(halifax_offenses) +
  geom_(aes(x = incident_date))
