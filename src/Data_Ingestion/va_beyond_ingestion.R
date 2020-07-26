library(dplyr)

ori_mappings <- readr::read_csv(here::here("data", "original", "Crime", "full_va_crime", "ori_county_mappings.csv"))

files_personal <- list.files(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_personal"))
files_property <- list.files(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_property"))
files_society <- list.files(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_society"))

va_personal_offenses <- do.call(bind_rows, lapply(files_personal, function(x) vroom::vroom(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_personal", x), skip = 6)))
va_property_offenses <- do.call(bind_rows, lapply(files_property, function(x) vroom::vroom(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_property", x), skip = 6)))
va_society_offenses <- do.call(bind_rows, lapply(files_society, function(x) vroom::vroom(here::here("data", "original", "Crime", "full_va_crime", "va_crimes_society", x), skip = 6)))

head(va_personal_offenses)
head(va_property_offenses)
head(va_society_offenses)

va_personal_offenses <- va_personal_offenses %>%
  mutate(offense_type = "Personal") %>%
  inner_join(ori_mappings)

va_property_offenses <- va_property_offenses %>%
  mutate(offense_type = "Property") %>%
  inner_join(ori_mappings)

va_society_offenses <- va_society_offenses %>%
  mutate(offense_type = "Society") %>%
  inner_join(ori_mappings)

# full_data <- bind_rows(bind_rows(va_personal_offenses, va_property_offenses), va_society_offenses)
# 
# head(full_data)

#vroom::vroom_write(va_property_offenses, here::here("data", "original", "Crime", "full_va_crime", "va_crimes_property_all_years.csv"), delim = ",")

