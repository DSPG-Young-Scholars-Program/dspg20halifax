library(tidycensus)

census_api_key("02d0507d6c692cfb3fdfd21e7400d5d53c7028f3", install = TRUE)

Sys.Date()

get_acs(geography = "tract", variables = "DP03_0001PE", state = "VA", county = "Halifax")
acs5_variables <- load_variables(year = 2018, dataset = "acs5", cache = TRUE)
acs5_variables

get_acs(geography = "county", 
        state = "WA", 
        table = "DP05",
        cache = TRUE)

library(tigris)
VA_counties_map <- plot(counties(state = "VA"))
Halifax <-(tracts(state = "VA", county = "Halifax"))

library(ggplot2)
library(sf)
library(tidyverse)
library(dplyr)
Petersburg <- tracts(state = "VA", county = "Petersburg")

no_HS_degree <- get_acs(geography = "tract", 
variable = "B06009_002", 
survey = "acs5", 
geometry = TRUE,
state = "VA",
county = "Petersburg")
va_no_HS <- plot(no_HS_degree["estimate"])
petersburg_ed_map <- mapview(no_HS_degree, zcol = "estimate", legend = TRUE)
petersburg_ed_map@map


