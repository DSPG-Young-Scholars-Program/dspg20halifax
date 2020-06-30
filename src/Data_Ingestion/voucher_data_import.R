
library(here)
library(dplyr)
library(tidyr)
library(stringr)

# ---- Voucher Recipient Data ---- #

voucher_avail <- readxl::read_xlsx(here("data", "original", "Housing", "housing_voucher_availability.xlsx")) %>%
  head(-2) %>%
  rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>% 
  rename_with(~gsub(",", "", .x, fixed = TRUE)) %>%
  rename_with(~gsub("%", "pct", .x, fixed = TRUE)) %>%
  rename_with(~gsub("#", "num", .x, fixed = TRUE))

# ---- Housing Voucher Data ---- #

voucher_data <- readxl::read_xlsx(here("data", "original", "Housing", "housing_voucher_data.xlsx"))

voucher_data <- voucher_data %>% pivot_longer(cols = -c(state, code, agency_name), names_to = "var", values_to = "values") %>%
  separate(var, into = c("var", "year"), sep = "(?<=[a-zA-Z])(?=[0-9])", remove = FALSE) %>%
  pivot_wider(names_from = var, values_from = values) %>%
  rename("pct_vouchers_in_use" = `Percent of authorized vouchers in use`)

# ---- Public housing ---- #

pub_housing <- readr::read_csv(here("data", "original", "Housing", "public_housing_data.csv")) %>% select(-X33)

## Need to clean this up as things have codes of 0, 1, and 2 which are meaningless without dictionary.

# ---- Low Income Housing Tax Credit Data ---- #

lihtc_data <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "LIHTCPUB.CSV"))


