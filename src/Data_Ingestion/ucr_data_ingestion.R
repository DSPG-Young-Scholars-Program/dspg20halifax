
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

#
#
# Script to pull data from FBI's NIBRS database using their API --------------------------------------------------------------------------
#
#

## You will need an API key to pull data. The script assumes you have stored your key in the R environment.
## API Keys can be obtained here: https://api.data.gov/signup/
## More documentation about data source here: https://crime-data-explorer.fr.cloud.gov/api



# ---- Helpful Functions ---- #

## Basic function to pull data from the API given a specific path
get_nibrs <- function(api_path) {
  
  req <- GET(paste0(api_path, "?API_KEY=", Sys.getenv("UCR_API_KEY")))
  
  status <- http_status(req)
  
  if (status$category == "Success") {
    
    req_text <- fromJSON(content(req, as = "text"))
    
    if ("data" %in% names(req_text)) {
      return(req_text$data)
    } else {
      return(req_text$results)
    }
    
  } else {
    stop(status$message)
  }
  
}

## Function that uses the get_nibrs function to pull demographic data at the agency level
get_nibrs_agency <- function(offenses, ## What crimes to pull
                             person, ## "offender" or "victim" data
                             ori_codes, ## Vector of codes for specific agencies
                             demo_var ## Single demographic variable of interest
                             ) {
  
  i <- 1
  data_list <- list()
  
  for (offense in offenses) {
    for (ori in ori_codes) {
      
      ## Assemble path for this combination of variables
      path <- paste("https://api.usa.gov/crime/fbi/sapi/api/nibrs", offense, person, "agencies", ori, demo_var, sep = "/")
      
      ## Pull data
      data <- get_nibrs(path)
      
      ## Make sure there is data for this combination
      if (length(data) != 0) {
        data$offense <- offense
        data$demo_var <- demo_var
        data$agency <- ori
        
        data_list[[i]] <- data
        
        i <- i + 1
      }
      
    }
  }
  
  ## Combine data into single source
  combined <- do.call(bind_rows, data_list)
  combined <- combined %>% select(agency, data_year, offense, demo_var, everything())
  
  return(combined)
  
}

## Function that uses the get_nibrs function to pull demographic data at the state level
get_nibrs_state <- function(offenses, ## What crimes to pull
                            person, ## "offender" or "victim" data
                            state, ## single state abbreviation
                            demo_var ## Demographic variable of interest
                            ) {
  
  i <- 1
  data_list <- list()
  
  for (offense in offenses) {
    
    ## Assemble path for this combination of variables
    path <- paste("https://api.usa.gov/crime/fbi/sapi/api/nibrs", offense, person, "states", state, demo_var, sep = "/")
    
    ## Pull data
    data <- get_nibrs(path)
    
    ## Make sure there is data for this combination
    if (length(data) != 0) {
      data$offense <- offense
      data$demo_var <- demo_var
      data$state <- state
      
      data_list[[i]] <- data
      
      i <- i + 1
    }
    
  }
  
  ## Combine data into single source
  combined <- do.call(bind_rows, data_list)
  combined <- combined %>% select(state, data_year, offense, demo_var, everything())
  
  return(combined)
  
}



# ---- Halifax offenses ---- #

## Offenses to view (all that are available for certain datasets)
offenses <- c("aggravated-assault", "burglary", "larceny", "motor-vehicle-theft", "homicide", "rape", "robbery", "arson", "violent-crime")

## Codes for each police entity in Halifax
halifax_sheriff_ori <- "VA0410000"
halifax_police_ori <- "VA0410100"
south_boston_ori <- "VA1240000"
sp_halifax_ori <- "VA041SP00"
sp_south_boston_ori <- "VA124SP00"

ori_codes <- c(halifax_sheriff_ori, halifax_police_ori, south_boston_ori, sp_halifax_ori, sp_south_boston_ori)

## Pull data for each demographic variable of interest
halifax_age_data <- get_nibrs_agency(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "age")
halifax_race_data <- get_nibrs_agency(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "race")
halifax_sex_data <- get_nibrs_agency(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "sex")

## Still not totally clear on the differences between these two but for comparability I get the counts from the "offender" API path as well..
halifax_offense_counts <- get_nibrs_agency(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "count")
#halifax_offense_counts <- get_nibrs_agency(offenses = offenses, person = "offense", geog_level = "agencies", ori_codes = ori_codes, demo_var = "count")



# ---- State level offenses ---- #

## Counts by type of crime and demographics at state level
va_sex_data <- get_nibrs_state(offenses = offenses, person = "offender", state = "VA", demo_var = "sex")
va_age_data <- get_nibrs_state(offenses = offenses, person = "offender", state = "VA", demo_var = "age")
va_race_data <- get_nibrs_state(offenses = offenses, person = "offender", state = "VA", demo_var = "race")



# ---- Write data for easier access ---- #

# readr::write_csv(va_race_data, here::here("data", "original", "Crime", "va_race_data.csv"))

