library(tidycensus)
library(acs)

race <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(white = "P003002", black = "P003003", native = "P003004",
                            asian = "P003005", two_races = "P003008", hispanic = "P004003"))
homeowner <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(total = "H012001", owner = "H012002", renter = "H012003"))
homeowner_by_race <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(white_owner= "H014003", black_owner = "H014004", white_renter = "H014011",
                            black_renter = "H014012"))
household_size <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(one = "H013002", two = "H013003", three = "H013004",
                            four = "H013005", five = "H013006", six = "H013007",
                            seven_or_more = "H013008"))
gender <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(male = "P012002", female = "P012026"))
median_age <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(total = "P013001", white_total = "P013A001", black_total = "P013B001"))
adult_or_juvenile <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(minor = "P016002", adult = "P016003"))
household_type <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(husband_wife = "P018003", male_no_wife = "P018005", female_no_husband = "P018006",
                            lives_alone = "P018008"))
institutionalized_pop <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(total_institutionalized = "P042002", total_adult_corrections = "P042003", total_juvenile_corrections = "P042004",
                            male_under18_adult_corrections = "P043005", male_juvenile_corrections  = "P043006", 
                            male_18to64_adult_corrections = "P043015", male_above65_adult_corrections = "P043025",
                            female_under18_adult_corrections = "P043036", female_juvenile_corrections  = "P043037", 
                            female_18to64_adult_corrections = "P043046", female_above65_adult_corrections = "P043056"))
group_quarters <- get_decennial(geography = "county", state = "VA", county = "Halifax",
              variables = c(Federal_detention = "PCT020004", federal_prisons = "PCT020005", state_prisons = "PCT020006",
                            local_jails = "PCT020007", community_corrections = "PCT020008", juvenile_group_homes = "PCT020011",
                            juvenile_rehab = "PCT020012", juvenile_corrections = "PCT020013", mental_hospitals = "PCT020016",
                            homeless_shelters = "PCT020027", adult_rehab = "PCT020029", adult_group_homes = "PCT020028"))
