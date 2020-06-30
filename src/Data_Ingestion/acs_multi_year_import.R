
library(tidycensus)
library(dplyr)
library(sf)
library(tigris)

## Function to create a single sf object with data for an ACS table across multiple years
get_acs_multi_years <- function(table, # ACS table ID
                                var_names, # Should be in the format obtained from tidycensus package load_variables function
                                years = seq(2010, 2018), 
                                state = "VA", 
                                geography = "county", 
                                survey = "acs1", # Switch to acs1 to avoid overlapping survey windows
                                spatial = TRUE) # Do you want spatial info?
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
  tmp_full <- do.call(bind_rows, data_by_year)
  
  if (spatial == TRUE) {
    ## Spatial data for VA counties
    counties <- counties(state = state, class = "sf", cb = TRUE, resolution = "20m") %>% 
      st_transform(crs = 4326)
    
    ## Join on spatial data
    acs_df <- left_join(counties, tmp_full, by = c("GEOID"))
    
  } else {
    
    acs_df <- tmp_full
    
  }
  
  return(acs_df)
  
}
