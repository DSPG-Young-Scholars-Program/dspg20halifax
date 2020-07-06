
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

## Offenses to view (all available for certain datasets)
offenses <- c("aggravated-assault", "burglary", "larceny", "motor-vehicle-theft", "homicide", "rape", "robbery", "arson", "violent-crime")

# ---- Halifax offenses ---- #

## Codes for each police entity in Halifax
halifax_sheriff_ori <- "VA0410000"
halifax_police_ori <- "VA0410100"
south_boston_ori <- "VA1240000"
sp_halifax_ori <- "VA041SP00"
sp_south_boston_ori <- "VA124SP00"

ori_codes <- c(halifax_sheriff_ori, halifax_police_ori, south_boston_ori, sp_halifax_ori, sp_south_boston_ori)

## Generic function to pull data from the API
get_nibrs <- function(api_path) {
  
  req <- GET(paste0(api_path, "?API_KEY=", Sys.getenv("UCR_API_KEY")))
  req_text <- fromJSON(content(req, as = "text"))
  
  if ("data" %in% names(req_text)) {
    return(req_text$data)
  } else {
    return(req_text$results)
  }
  
}

## Function that uses the get_nibrs function to pull demographic data specifically
get_nibrs_demo <- function(offenses, ## What crimes to pull
                           person, ## "offender" or "victim" data
                           ori_codes, 
                           demo_var) {
  
  i <- 1
  data_list <- list()
  
  for (offense in offenses) {
    for (ori in ori_codes) {
      
      ## Assemble path for this combination of variables
      path <- paste("https://api.usa.gov/crime/fbi/sapi/api/nibrs", offense, person, "agencies", state, ori, demo_var, sep = "/")
      
      print(path)
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

halifax_age_data <- get_nibrs_demo(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "age")
halifax_race_data <- get_nibrs_demo(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "race")
halifax_sex_data <- get_nibrs_demo(offenses = offenses, person = "offender", ori_codes = ori_codes, demo_var = "sex")

## Still not totally clear on the differences between these two but for simplicity I get the counts from the "offender" API path as well..
halifax_offense_counts <- get_nibrs_demo(offenses = offenses, person = "offender", geog_level = "agencies", ori_codes = ori_codes, demo_var = "count")
#halifax_offense_counts <- get_nibrs_demo(offenses = offenses, person = "offense", geog_level = "agencies", ori_codes = ori_codes, demo_var = "count")

# ---- State level offenses ---- #

## Combine data from all offense types into single dataset
i <- 1
data_list <- list()

for (offense in offenses) {
  
  ucr_get <- GET(paste0("https://api.usa.gov/crime/fbi/sapi/api/nibrs/", offense, "/offender/states/VA/count?API_KEY=", Sys.getenv("UCR_API_KEY")))
  ucr_list <- fromJSON(content(ucr_get, as = "text"))
  ucr_data <- ucr_list$data
  
  ucr_data$offense <- offense
  
  data_list[[i]] <- ucr_data
  
  i <- i + 1
}

va_offense_data <- do.call(bind_rows, data_list)


