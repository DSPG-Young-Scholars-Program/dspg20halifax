
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

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

lihtc_data <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "LIHTCPUB.CSV"), na.strings = "")

## Variables to be recoded as 1/2 for Yes/No
binary_recode_cols <- c("scattered_site_cd", "resyndication_cd", "low_ceil", "non_prof", "basis",
                        "bond", "mff_ra", "fmha_514", "fmha_515", "fmha_538","home", 
                        "tcap", "cdbg", "htf", "fha", "hopevi", "tcep", "rad", "qozf",
                        "trgt_pop", "trgt_fam", "trgt_eld", "trgt_dis", "trgt_hml", "trgt_other", "nonprog")

## Cleaning up numeically coded columns so we don't have to reference the dictionary for everything
lihtc_data <- lihtc_data %>% 
  map_at(binary_recode_cols, ~case_when(. == 1 ~ "Yes",
                                        . == 2 ~ "No")) %>% 
  map_at("inc_ceil", ~case_when(. == 1 ~ "50% AMGI",
                                . == 2 ~ "60% AMGI")) %>%
  map_at("rentasst", ~case_when(. == 1 ~ "Federal",
                                . == 2 ~ "State",
                                . == 3 ~ "Both",
                                . == 4 ~ "Neither")) %>%
  map_at("type", ~case_when(. == 1 ~ "New construction",
                            . == 2 ~ "Acquisition and rehab",
                            . == 3 ~ "Both",
                            . == 4 ~ "Existing")) %>%
  map_at("credit", ~case_when(. == 1 ~ "30% present value",
                              . == 2 ~ "70% present value",
                              . == 3 ~ "Both",
                              . == 4 ~ "TCEP only")) %>%
  map_at("metro", ~case_when(. == 1 ~ "Metro/noncentral city",
                             . == 2 ~ "Metro/central city",
                             . == 3 ~ "Non-metro")) %>%
  map_at("dda", ~case_when(. == 0 ~ "Not in DDA",
                           . == 1 ~ "In metro DDA",
                           . == 2 ~ "In non-metro DDA",
                           . == 3 ~ "In metro go zone DDA",
                           . == 4 ~ "In non-metro go zone DDA")) %>%
  map_at("qct", ~case_when(. == 1 ~ "in qualified tract",
                           . == 2 ~ "not in qualified tract")) %>%
  map_at("nlm_reason", ~case_when(. == 1 ~ "completed extended-use period",
                                  . == 2 ~ "safe under qualified contract",
                                  . == 3 ~ "other")) %>%
  map_at("record_stat", ~case_when(. == "N" ~ "New",
                                   . == "U" ~ "Updated",
                                   . == "X" ~ "Existing")) %>%
  as.data.frame() %>%
  separate(fips2010, into = c("GEOID", "tract_fips"), 5)
  

## Halifax county projects
lihtc_data %>% filter(str_detect(fips2010, "^(51083)"))

## VA projects
lihtc_data %>% filter(str_detect(fips2010, "^(51)"))

# readr::write_csv(lihtc_data, here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean"))

