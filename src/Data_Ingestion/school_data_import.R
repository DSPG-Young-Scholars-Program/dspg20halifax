
library(educationdata)
library(dplyr)
library(stringr)
library(sf)
library(tigris)

halifax_school_ids <- c("510177000710", "510177002745", "510177001907", "510177001848", "510177000714", "510177000716", 
                        "510177000717", "510177002769", "510177002759", "510177002836", "510177000720")

## Convenience function to pull for multiple schools at once
get_multiple_schools <- function(school_ids, source, topic, by) {
  
  nces_data <- list()
  i <- 1
  
  ## Iterate through halifax schools and get absenteeism data
  for (school in school_ids) {
    
    nces_data[[i]] <- get_education_data(level = "schools",
                                         source = source,
                                         topic = topic,
                                         filters = list(ncessch = school), ## Iterate through halifax schools
                                         by = by)
    
    i <- i + 1
    
  }
  
  combined_data <- do.call(bind_rows, nces_data) %>%
    mutate(across(c("ncessch", "fips", "leaid", "race", "sex"), as.character)) %>%
    mutate(race = recode(race, "1" = "White", "2" = "Black", "3" = "Hispanic", "4" = "Asian", "5" = "American Indian or Alaska Native", 
                         "6" = "Native Hawaiian or other Pacific Islander", "7" = "Two or more races",  "8" = "Nonresident alien",  
                         "9" = "Unknown",  "20" = "Other", "99" = "Total"),
           sex = recode(sex, "1" = "Male", "2" = "Female", "9" = "Unknown", "99" = "Total"))
  
  return(combined_data)
}

#
#
# Directory Data ------------------------------------------------------------------------------------------------------------------------
#
# 

## Initialize loop variables
nces_data <- list()
i <- 1

## Get general info about all schools (no idea why we can't filter by school here - API is dumb) 
for (year in seq(2011, 2015)) {
  for (school in halifax_school_ids) {
    nces_data[[i]] <- get_education_data(level = "schools", source = "ccd", topic = "directory", filters = list(year = year, ncessch = school))
    i <- i + 1
  }
}

halifax_schools <- do.call(bind_rows, nces_data) %>%
  mutate(across(c("ncessch", "fips", "leaid"), as.character))

#
#
# Enrollment Data ---------------------------------------------------------------------------------------------------------
#
#

## May not be needed if included in directory

# ## Initialize loop variables
# nces_data <- list()
# i <- 1
# 
# ## Pull Halifax enrollment data for years 2011-2015
# for (school in halifax_school_ids) {
#   for (year in seq(2011, 2015))
#     nces_data[[i]] <- get_education_data(level = "schools", source = "ccd", topic = "enrollment", filters = list(ncessch = school, year = year), by = list("race", "sex"))
#     i <- i + 1
# }
# 
# ## Combine into single source
# enrollment_data <- do.call(bind_rows, nces_data) %>% 
#   mutate(across(c("ncessch", "fips", "leaid", "race", "sex"), as.character)) %>%
#   mutate(race = recode(race, "1" = "White", "2" = "Black", "3" = "Hispanic", "4" = "Asian", "5" = "American Indian or Alaska Native", 
#                        "6" = "Native Hawaiian or other Pacific Islander", "7" = "Two or more races",  "8" = "Nonresident alien",  
#                        "9" = "Unknown",  "20" = "Other", "99" = "Total"),
#          sex = recode(sex, "1" = "Male", "2" = "Female", "9" = "Unknown", "99" = "Total"))

#
#
# Absenteeism Data ------------------------------------------------------------------------------------------------------------------------
#
#

## Pull absenteeism data for Halifax schools
absentee_data <- get_multiple_schools(school_ids = halifax_school_ids, 
                                      source = "crdc", 
                                      topic = "chronic-absenteeism", 
                                      by = list("sex", "race"))


halifax_absentee_data <- left_join(halifax_schools, absentee_data)

#
#
# Discipline Data ------------------------------------------------------------------------------------------------------------------------
#
#

discipline_data <- get_multiple_schools(school_ids = halifax_school_ids,
                                        source = "crdc",
                                        topic = "discipline",
                                        by = list("disability", "race", "sex"))

halifax_discpline_data <- left_join(halifax_schools, discipline_data)

#
#
# Merge On Directory ------------------------------------------------------------------------------------------------------------------------
#
#

## Merge and write
halifax_school_data <- full_join(halifax_absentee_data, halifax_discpline_data)

# readr::write_csv(halifax_school_data, here::here("data", "original", "halifax_school_data.csv"))





