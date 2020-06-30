
library(acs)
library(tidycensus)
library(dplyr)
library(sf)

## Function to create a single sf object with data for an ACS table across multiple years
get_acs_multi_years <- function(table, # ACS table ID
                                var_names, # Should be in the format obtained from acs package load_variables function
                                years = seq(2010, 2018), 
                                state = "VA", 
                                geography = "county", 
                                survey = "acs5") # Switch to acs1 to avoid overlapping survey windows
  {
  
  i <- 1
  data_by_year <- list()
  
  ## Iterate through years, pull data, store in list
  for (year in years) {
    
    ## Pull ACS data for current year
    tmp <- get_acs(geography = geography, year = year, table = table, state = state, survey = survey) %>%
      left_join(var_names, by = c("variable" = "name")) %>%
      mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label))))) %>%
      select(-variable) %>%
      pivot_wider(names_from = label,
                  values_from = c(estimate, moe),
                  names_glue = "{label}_{.value}") %>%
      mutate(year = year)
    
    ## Store in list
    data_by_year[[i]] <- tmp
    
    i <- i + 1
    
  }
  
  ## Combine into single dataframe
  tmp_full <- do.call(rbind, data_by_year)
  
  ## Join on spatial data
  acs_df <- left_join(va_counties, tmp_full, by = c("GEOID"))
  
  return(acs_df)
  
}
