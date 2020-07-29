
# load and clean arrest data on all VA counties

library(here)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidycensus)

all_va_personal <- read_csv(here("data", "original", "Crime", "full_va_crime", "va_crimes_personal_all_years.csv"))

# replace all spaces in column names with underscore
colnames(all_va_personal) <- gsub(".", 
                                  "_",
                                  make.names(colnames(all_va_personal)),
                                  fixed = TRUE)

# make all column names lowercase
colnames(all_va_personal) <- tolower(colnames(all_va_personal))

all_va_personal$incident_year = year(all_va_personal$incident_date)
all_va_personal$incident_month = month(all_va_personal$incident_date)
all_va_personal$incident_day = day(all_va_personal$incident_date)


all_va_property <- read_csv(here("data", "original", "Crime", "full_va_crime", "va_crimes_property_all_years.csv"))

# replace all spaces in column names with underscore
colnames(all_va_property) <- gsub(".", 
                                  "_",
                                  make.names(colnames(all_va_property)),
                                  fixed = TRUE)

# make all column names lowercase
colnames(all_va_property) <- tolower(colnames(all_va_property))

all_va_property$incident_year = year(all_va_property$incident_date)
all_va_property$incident_month = month(all_va_property$incident_date)
all_va_property$incident_day = day(all_va_property$incident_date)

all_va_society <- read_csv(here("data", "original", "Crime", "full_va_crime", "va_crimes_society_all_years.csv"))

# replace all spaces in column names with underscore
colnames(all_va_society) <- gsub(".", 
                                 "_",
                                 make.names(colnames(all_va_society)),
                                 fixed = TRUE)

# make all column names lowercase
colnames(all_va_society) <- tolower(colnames(all_va_society))

all_va_society$incident_year = year(all_va_society$incident_date)
all_va_society$incident_month = month(all_va_society$incident_date)
all_va_society$incident_day = day(all_va_society$incident_date)

all_va_arrest <- rbind(all_va_personal, all_va_property, all_va_society)

colnames(all_va_arrest)[32:33] <- c("offense_category", "county_cap")
all_va_arrest <- all_va_arrest[,-33]

# remove arrests with county set to Division or Statewide Departments
arrest_division <- which(grepl("Division", all_va_arrest$county, fixed = TRUE))
arrest_state <- which(grepl("Statewide Departments", all_va_arrest$county, fixed = TRUE))
arrest_remove <- c(arrest_division, arrest_state)

all_va_arrest <- all_va_arrest[-arrest_remove,]

# change all counties with City in name to actual county
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Alexandria City"), 
                                "Fairfax County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Lynchburg City"), 
                                "Amherst County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Colonial Heights City"), 
                                "Dinwiddie County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Petersburg City"), 
                                "Dinwiddie County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Waynesboro City"), 
                                "Augusta County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Galax City"), 
                                "Carroll County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Manassas City"), 
                                "Prince William County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Danville City"), 
                                "Pittsylvania County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Harrisonburg City"), 
                                "Rockingham County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Winchester City"), 
                                "Frederick County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Fredericksburg City"), 
                                "Spotsylvania County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Portsmouth City"), 
                                "Norfolk County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Chesapeake City"), 
                                "Norfolk County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Clifton Forge City"), 
                                "Alleghany County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Martinsville City"), 
                                "Henry County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Salem City"), 
                                "Roanoke County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Williamsburg City"), 
                                "James City County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Radford City"), 
                                "Montgomery County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Lexington City"), 
                                "Rockbridge County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Staunton City"), 
                                "Augusta County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Norton City"), 
                                "Wise County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Manassas Park City"), 
                                "Prince William County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Bristol City"), 
                                "Washington County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Poquoson City"), 
                                "York County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Covington City"), 
                                "Alleghany County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Buena Vista City"), 
                                "Rockbridge County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Emporia City"), 
                                "Greensville County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Norfolk City"), 
                                "Norfolk County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Hopewell City"), 
                                "Prince George County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Hampton City"), 
                                "Hampton County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Virginia Beach City"), 
                                "Virginia Beach County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Newport News City"), 
                                "Newport News County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "King & Queen County"), 
                                "King and Queen County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Falls Church"), 
                                "Fairfax County")
all_va_arrest$county <- replace(all_va_arrest$county, 
                                str_detect(all_va_arrest$county, "Greenesville County"), 
                                "Greensville County")

all_va_arrest$county_cap <- toupper(sapply(str_split(all_va_arrest$county, " County"), "[", 1))

write.csv(all_va_arrest,
          "../../data/original/Crime/full_va_crime/clean_all_arrest_all_years.csv")

# get population counts by race for each county
# from census

# race variables
racevars <- c(White = "P005003",
              Black = "P005004")

va_pop_by_race <- get_decennial(geography = "county", 
                                variables = racevars,
                                state = "VA",
                                summary_var = "P001001") %>%
  select(NAME, race = variable, pop = value, 
         total_pop = summary_value) %>%
  as.data.frame()

va_pop_by_race <- reshape(va_pop_by_race, 
                          idvar = "NAME",
                          timevar = "race",
                          v.names = "pop",
                          direction = "wide",
                          sep = "_")

va_pop_by_race$county <- sapply(str_split(va_pop_by_race$NAME, ", Virginia"), "[", 1)

va_pop_by_race[va_pop_by_race$county == "Fairfax County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Fairfax County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Alexandria city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Amherst County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Amherst County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Lynchburg city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Dinwiddie County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Dinwiddie County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Colonial Heights city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Dinwiddie County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Dinwiddie County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Petersburg city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Augusta County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Augusta County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Waynesboro city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Carroll County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Carroll County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Galax city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Prince William County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Prince William County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Manassas city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Pittsylvania County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Pittsylvania County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Danville city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Rockingham County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Rockingham County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Harrisonburg city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Frederick County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Frederick County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Winchester city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Spotsylvania County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Spotsylvania County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Fredericksburg city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Norfolk County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Norfolk County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Portsmouth city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Norfolk County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Norfolk County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Chesapeake city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Henry County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Henry County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Martinsville city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Roanoke County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Roanoke County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Salem city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "James City County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "James City County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Williamsburg city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Montgomery County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Montgomery County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Radford city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Rockbridge County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Rockbridge County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Lexington city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Augusta County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Augusta County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Staunton city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Wise County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Wise County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Norton city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Prince William County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Prince William County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Manassas Park city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Washington County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Washington County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Bristol city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "York County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "York County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Poquoson city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Alleghany County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Alleghany County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Covington city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Rockbridge County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Rockbridge County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Buena Vista city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Greensville County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Greensville County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Emporia city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Prince George County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Prince George County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Hopewell city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Greensville County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Greensville County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Emporia city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Fairfax County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Fairfax County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Falls Church city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Albemarle County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Albemarle County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Charlottesville city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Bedford County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Bedford County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Bedford city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Southampton County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Southampton County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Franklin city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Henrico County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Henrico County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Richmond city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Roanoke County", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Roanoke County", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Roanoke city", c(2:4)] 

va_pop_by_race[va_pop_by_race$county == "Norfolk city", c(2:4)] <- va_pop_by_race[va_pop_by_race$county == "Norfolk city", c(2:4)] +
  va_pop_by_race[va_pop_by_race$county == "Suffolk city", c(2:4)] 

va_pop_by_race$county <- replace(va_pop_by_race$county, 
                                 str_detect(va_pop_by_race$county, "Norfolk city"), 
                                 "Norfolk County")

va_pop_by_race$county <- replace(va_pop_by_race$county, 
                                 str_detect(va_pop_by_race$county, "Hampton city"), 
                                 "Hampton County")

va_pop_by_race$county <- replace(va_pop_by_race$county, 
                                 str_detect(va_pop_by_race$county, "Virginia Beach city"),
                                 "Virginia Beach County")

va_pop_by_race$county <- replace(va_pop_by_race$county, 
                                 str_detect(va_pop_by_race$county, "Newport News city"), 
                                 "Newport News County")

# remove redundant cities
city_keep <- c(120, 121, 132, 112)
city_remove <- c(91, 97:134)
city_remove <- city_remove[!(city_remove %in% city_keep)]
va_pop_by_race <- va_pop_by_race[-city_remove,]

va_pop_by_race$county_cap <- toupper(sapply(str_split(va_pop_by_race$county, " County"), "[", 1))

write.csv(va_pop_by_race,
          "../../data/original/Crime/full_va_crime/clean_county_pop_by_race.csv")
