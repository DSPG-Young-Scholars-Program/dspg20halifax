homeowner_by_race <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "tidy",
                                   variables = c(white_owner= "H014003", black_owner = "H014004", white_renter = "H014011",
                                                 black_renter = "H014012"))
ggplot()+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  labs(x = "Census Tract", 
       y = "Type of Homeownership",
       title = "Type of Homeownership by race",
       subtitle = "Halifax County, VA",
       caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")