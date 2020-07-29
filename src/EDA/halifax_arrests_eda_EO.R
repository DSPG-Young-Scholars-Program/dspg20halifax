
library(here)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

halifax_offenses <- readr::read_csv(here("data", "original", "Crime", "halifax_offenses_combined.csv"))

halifax_offenses <- halifax_offenses %>%
  mutate(incident_year = year(incident_date),
         incident_month = month(incident_date),
         incident_day = day(incident_date)) %>%
  filter(between(incident_year, 2010, 2019)) 

year_by_year_arrest <- halifax_offenses %>%
  group_by(incident_year) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n)) + 
  geom_bar(stat='identity', fill = "#0072B2") + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

top_ten_offense <- halifax_offenses %>%
  group_by(offense_type) %>%
  tally %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x = offense_type, y = n)) +
  geom_bar(stat='identity', fill = "#0072B2") +
  scale_x_discrete(labels = c("Aggravated\nAssault", "Other\nLarceny", "Breaking/\nEntering",
                              "Property\nDamage", "Drugs/\nNarcotics", "Shoplifting",
                              "Simple\nAssault", "Theft from\nBuilding", "Theft from\nVehicle",
                              "Weapon\nViolation")
                   ) + 
  xlab("Incident type") + ylab("Number of incidients") + 
  theme_minimal()

top_ten_offense <- sort(table(halifax_offenses$offense_type), 
                        decreasing = TRUE)[1:10]

top_ten_offense_by_year <- halifax_offenses %>%
  filter(offense_type %in% names(top_ten_offense)) %>%
  group_by(incident_year, offense_type) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offense_type)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

# maybe split up offenses so not all 10 on one plot

# get proportions of black/white, not just raw counts
offender_race_by_year <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American",
                              "White")) %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

race_by_offense_type <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American", "White") & 
           offense_type == "Drug/Narcotic Violations") %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

race_by_offense_type <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American", "White") & 
           offense_type == "Simple Assault") %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

race_by_offense_type <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American", "White") & 
           grepl("Weapon", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

# offenses that probably correlate more with 
# actually doing the crime
# police respond to Vandalism/Burglary, they
# dont really go looking for it
race_by_offense_type <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American", "White") & 
           grepl("Vandalism", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()
  
race_by_offense_type <- halifax_offenses %>%
  filter(offender_race %in% c("Black or African American", "White") & 
           grepl("Burglary", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, offender_race) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offender_race)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

# summary of age
age <- halifax_offenses %>%
  filter(offender_age > 0) %>%
  ggplot(aes(x = offender_age, fill = "#0072B2")) + 
  geom_density() +
  theme(legend.position = "none") +
  xlab("Offender age") + ylab("Density")

# summary of age by year
age_by_year <- halifax_offenses %>%
  filter(offender_age > 0) %>%
  ggplot(aes(x = offender_age, fill = "#0072B2")) + 
  geom_density() +
  facet_wrap(.~incident_year) + 
  theme(legend.position = "none") +
  xlab("Offender age") + ylab("Density")

# age by race
age_by_race <- halifax_offenses %>%
  filter(offender_age > 0 & 
           offender_race %in% c("Black or African American", "White")) %>%
  ggplot(aes(x = offender_age, fill = "#0072B2")) + 
  geom_density() +
  facet_wrap(.~offender_race) + 
  theme(legend.position = "none") +
  xlab("Offender age") + ylab("Density")

#  hour of day
halifax_offenses_hour <- halifax_offenses %>%
  filter(hour_of_day != "Unknown") %>%
  mutate(incident_hour = case_when(hour_of_day == "12:00am-12:59am" ~ 24,
                                   hour_of_day == "1:00am-1:59am" ~ 1,
                                   hour_of_day == "2:00am-2:59am" ~ 2,
                                   hour_of_day == "3:00am-3:59am" ~ 3,
                                   hour_of_day == "4:00am-4:59am" ~ 4,
                                   hour_of_day == "5:00am-5:59am" ~ 5,
                                   hour_of_day == "6:00am-6:59am" ~ 6,
                                   hour_of_day == "7:00am-7:59am" ~ 7,
                                   hour_of_day == "8:00am-8:59am" ~ 8,
                                   hour_of_day == "9:00am-9:59am" ~ 9,
                                   hour_of_day == "10:00am-10:59am" ~ 10,
                                   hour_of_day == "11:00am-11:59am" ~ 11,
                                   hour_of_day == "12:00n-12:59pm" ~ 12,
                                   hour_of_day == "1:00pm-1:59pm" ~ 13,
                                   hour_of_day == "2:00pm-2:59pm" ~ 14,
                                   hour_of_day == "3:00pm-3:59pm" ~ 15,
                                   hour_of_day == "4:00pm-4:59pm" ~ 16,
                                   hour_of_day == "5:00pm-5:59pm" ~ 17,
                                   hour_of_day == "6:00pm-6:59pm" ~ 18,
                                   hour_of_day == "7:00pm-7:59pm" ~ 19,
                                   hour_of_day == "8:00pm-8:59pm" ~ 20,
                                   hour_of_day == "9:00pm-9:59pm" ~ 21,
                                   hour_of_day == "10:00pm-10:59pm" ~ 22,
                                   hour_of_day == "11:00pm-11:59pm" ~ 23)
         )

hour <- halifax_offenses_hour %>%
  ggplot(aes(x = as.numeric(incident_hour), fill = "#0072B2")) + 
  geom_density() +
  scale_x_continuous(breaks = seq(1, 24, 2),
                     labels = paste0(seq(1, 24, 2), ":00")) + 
  theme(legend.position = "none") +
  xlab("Hour of incident") + ylab("Density")


# look at incident clearance
halifax_not_cleared <- halifax_offenses %>%
  filter(incident_clearance == "Not Cleared") 

prop.table(table(halifax_not_cleared$offender_race))

halifax_cleared <- halifax_offenses %>%
  filter(incident_clearance == "Cleared by Arrest")

prop.table(table(halifax_cleared$offender_race))

sort(table(halifax_not_cleared$offense_type),decreasing=TRUE)

# missing races
# gender, ethnicity
# age all 0
# all unknown offender
# all not cleared
halifax_missing_race <- halifax_offenses %>%
  filter(offender_race == "Missing")

(top_ten_offense/sum(table(halifax_offenses$offense_type)))[order(names(top_ten_offense))]

halifax_missing_top_ten_offense <- halifax_missing_race %>%
  filter(offense_type %in% names(top_ten_offense))

perc_off_missing <- data.frame(table(halifax_missing_top_ten_offense$offense_type)/
                                 sum(table(halifax_missing_race$offense_type)))
colnames(perc_off_missing) <- c("offense_type", "perc_off_missing")

halifax_missing_top_ten_offense %>%
  group_by(offense_type) %>%
  slice(1) %>%
  left_join(perc_off_missing, by = "offense_type") %>%
  ggplot(aes(x = offense_type, y = perc_off_missing)) +
  geom_bar(stat='identity', fill = "#0072B2") +
  scale_x_discrete(labels = c("Aggravated\nAssault", "Other\nLarceny", "Breaking/\nEntering",
                              "Property\nDamage", "Drugs/\nNarcotics", "Shoplifting",
                              "Simple\nAssault", "Theft from\nBuilding", "Theft from\nVehicle",
                              "Weapon\nViolation")
  ) + 
  xlab("Incident type") + ylab("% of incidients") + 
  ggtitle("% of each incident type among the missing arrests") + 
  theme_minimal()

halifax_not_missing_race <- halifax_offenses %>%
  filter(offender_race != "Missing")

halifax_not_missing_top_ten_offense <- halifax_not_missing_race %>%
  filter(offense_type %in% names(top_ten_offense))

perc_off_not_missing <- data.frame(table(halifax_not_missing_top_ten_offense$offense_type)/
                                 sum(table(halifax_not_missing_race$offense_type)))
colnames(perc_off_not_missing) <- c("offense_type", "perc_off_missing")

halifax_not_missing_top_ten_offense %>%
  group_by(offense_type) %>%
  slice(1) %>%
  left_join(perc_off_not_missing, by = "offense_type") %>%
  ggplot(aes(x = offense_type, y = perc_off_missing)) +
  geom_bar(stat='identity', fill = "#0072B2") +
  scale_x_discrete(labels = c("Aggravated\nAssault", "Other\nLarceny", "Breaking/\nEntering",
                              "Property\nDamage", "Drugs/\nNarcotics", "Shoplifting",
                              "Simple\nAssault", "Theft from\nBuilding", "Theft from\nVehicle",
                              "Weapon\nViolation")
  ) + 
  xlab("Incident type") + ylab("% of incidents") + 
  ggtitle("% of each incident type among the non-missing arrests") + 
  theme_minimal()

top_ten_offense_missing_race <- sort(table(halifax_missing_race$offense_type), 
                                     decreasing = TRUE)[1:10]

top_ten_offense_by_year <- halifax_offenses %>%
  filter(offense_type %in% names(top_ten_offense)) %>%
  group_by(incident_year, offense_type) %>%
  tally %>%
  ggplot(aes(x = incident_year, y = n, color = offense_type)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = c(seq(2009, 2019)),
                     labels = c(seq(2009, 2019))) +
  xlab("Year") + ylab("Number of incidents") +
  theme_minimal()

# proportion of missing race incidents in each year
table(halifax_missing_race$incident_year)/sum(table(halifax_missing_race$incident_year))

# proportion of all incidents in each year
table(halifax_offenses$incident_year)/sum(table(halifax_offenses$incident_year))

table(halifax_missing_race$incident_month)/sum(table(halifax_missing_race$incident_month))

table(halifax_offenses$incident_month)/sum(table(halifax_offenses$incident_month))


## separate out offenses by agency
halifax_sheriff <- halifax_offenses %>%
  filter(agency_name == "Halifax CO Sheriff's Office")

head(sort(table(halifax_sheriff$offense_type),decreasing=T),10)

halifax_police <- halifax_offenses %>%
  filter(agency_name == "Halifax Police Department")

head(sort(table(halifax_police$offense_type),decreasing=T),10)

s_boston_police <- halifax_offenses %>%
  filter(agency_name == "South Boston Police Department")

head(sort(table(s_boston_police$offense_type),decreasing=T),10)

state_police <- halifax_offenses %>%
  filter(agency_name == "State Police - Halifax CO")

head(sort(table(state_police$offense_type),decreasing=T),10)


halifax_offenses %>%
  filter(agency_name == "Halifax CO Sheriff's Office") %>%
  group_by(offender_race) %>%
  tally

halifax_offenses %>%
  filter(agency_name == "Halifax Police Department") %>%
  group_by(offender_race) %>%
  tally

halifax_offenses %>%
  filter(agency_name == "South Boston Police Department") %>%
  group_by(offender_race) %>%
  tally

halifax_offenses %>%
  filter(agency_name == "State Police - Halifax CO") %>%
  group_by(offender_race) %>%
  tally
