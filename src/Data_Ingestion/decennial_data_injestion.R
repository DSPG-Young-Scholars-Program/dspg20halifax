library(tidycensus)
library(acs)
library(tigris)
library(sf)
library(dplyr)

#pulling Decennial Data into R

# Decinnial dataset regarding race in Halifax county on the Tract level
race <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_population = "P001001", white_race = "P003002", black_race = "P003003", native_race = "P003004",
                            asian_race = "P003005", two_races = "P003008", hispanic_ethnicity = "P004003"))

# Decinnial dataset regarding the total population and whether they live in mortgage owned, free and clear owned, or rented housing
household_status <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
                               variables = c(total_pop_in_housing = "H011001", pop_in_mortgage_owned_housing = "H011002", pop_in_full_owned_housing = "H011003", pop_in_renters_housing = "H011004"))

#further breakdown of the above household dataset, broken down by race
homeowner_by_race <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_owner = "H014002", white_owner= "H014003", black_owner = "H014004", 
                            total_renter = "H014010", white_renter = "H014011", black_renter = "H014012"))

# Decinnial dataset regarding average household size in Halifax county on the Tract level
avg_household <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(avg_household_size = "H012001", owners_avg_houshold_size = "H012002", renters_avg_household_size = "H012003"))

# Decinnial dataset regarding household size in Halifax county on the Tract level
household_size <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(one_household_members = "H013002", two_household_members = "H013003", three_household_members = "H013004",
                            four_household_members = "H013005", five_household_members = "H013006", six_household_members = "H013007",
                            seven_or_more_household_members = "H013008"))

# Decinnial dataset regarding gender in Halifax county on the Tract level
gender <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(male_pop = "P012002", female_pop = "P012026"))
# Decinnial dataset regarding median age in Halifax county on the Tract level
median_age <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_median_age = "P013001", white_median_age = "P013A001", black_median_age = "P013B001"))
# Decinnial dataset about age in halifax county 
#adult = 18 years of age and older; juvenile = under 18 years of age
adult_or_juvenile <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide", 
              variables = c(pop_under18 = "P016002", adult_pop = "P016003"))
# Decinnial dataset regarding the family situations (marriage and/or housing situations) in Halifax county 
household_type <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_households = "P018001", husband_wife_household = "P018003", 
                            male_no_wife_household = "P018005", female_no_husband_household = "P018006",
                            lives_alone_houshold = "P018008"))
# Decinnial dataset regarding the age and gender of the institionalized population in Halifax county 
#Data also separated by the type of corrections the person is in
institutionalized_pop <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_institutionalized_pop = "P042002", total_adult_corrections_pop = "P042003", total_juvenile_corrections_pop = "P042004",
                            male_pop_under18_adult_corrections = "P043005", male_pop_juvenile_corrections  = "P043006", 
                            male_pop_18to64_adult_corrections = "P043015", male_pop_above65_adult_corrections = "P043025",
                            female_pop_under18_adult_corrections = "P043036", female_pop_juvenile_corrections  = "P043037", 
                            female_pop_18to64_adult_corrections = "P043046", female_pop_above65_adult_corrections = "P043056"))

#total population living in group quarters, separtaed by each specific group housing type
group_quarters <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "wide",
              variables = c(total_group_quarters_pop = "PCT020001", Federal_detention_pop = "PCT020004", federal_prisons_pop = "PCT020005", state_prisons_pop = "PCT020006",
                            local_jails_pop = "PCT020007", community_corrections_pop = "PCT020008", juvenile_group_homes_pop = "PCT020011",
                            juvenile_rehab_pop = "PCT020012", juvenile_corrections_pop = "PCT020013", mental_hospitals_pop = "PCT020016",
                            homeless_shelters_pop = "PCT020027", adult_rehab_pop = "PCT020029", adult_group_homes_pop = "PCT020028"))

#merging all of the above datasets into one large Halifax Dataset
Xhalifax_decennial_data <- Reduce(function(...) merge(..., all=TRUE), list(race, avg_household, household_status, homeowner_by_race, household_size, gender, median_age, adult_or_juvenile, household_type, institutionalized_pop, group_quarters))

#adding spatial data to the Halifax dataset
Halifax_tracts <- tracts("VA", county = "Halifax", cb = TRUE)
Xhalifax__tracts <- st_as_sf(Halifax_tracts)

#merging decennial dataset into the spatial dataset 
halifax_data <- right_join(Xhalifax_decennial_data, Xhalifax__tracts, by = "GEOID")

#setting Halifax data to sf
halifax_decennial_data <- st_as_sf(halifax_data)

#adding Percentage Columns
halifax_decennial_data <- halifax_decennial_data %>%
  mutate(pct_white_race = white_race/total_population) %>% 
  mutate(pct_non_white = 1 - pct_white_race) %>%
  mutate(pct_black_race = black_race/total_population) %>%
  mutate(pct_asian_race = asian_race/total_population) %>%
  mutate(pct_native_race = native_race/total_population) %>%
  mutate(pct_hispanic_ethnicity = hispanic_ethnicity/total_population) %>%
  mutate(pct_mortgage_owned_housing = pop_in_mortgage_owned_housing/total_pop_in_housing) %>%
  mutate(pct_full_owned_housing = pop_in_full_owned_housing/total_pop_in_housing) %>%
  mutate(pct_renters_housing = pop_in_renters_housing/total_population) %>%
  mutate(pct_white_owner = white_owner/total_owner) %>%
  mutate(pct_black_owner = black_owner/total_owner) %>%
  mutate(pct_white_renter = white_renter/total_renter) %>%
  mutate(pct_black_renter = black_renter/total_renter) %>%
  mutate(pct_male_pop = male_pop/total_population) %>%
  mutate(pct_female_pop = female_pop/total_population) %>%
  mutate(pct_under18 = pop_under18/total_population) %>%
  mutate(pct_adult = adult_pop/total_population) %>%
  mutate(pct_husband_wife_household = husband_wife_household/total_households) %>%
  mutate(pct_male_no_wife_household = male_no_wife_household/total_households) %>%
  mutate(pct_female_no_husband_household = female_no_husband_household/total_households) %>%
  mutate(pct_single_parent = pct_male_no_wife_household + pct_female_no_husband_household) %>%
  mutate(pct_lives_alone_household = lives_alone_houshold/total_households) 

#Save Halifax Decennial Dataset to a GeoJson file
st_write(halifax_decennial_data, here::here("src", "Data_Ingestion", "halifax_decennial_data.geojson"), driver = "GeoJSON")

