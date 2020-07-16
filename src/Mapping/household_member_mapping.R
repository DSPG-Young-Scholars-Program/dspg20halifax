ggplot()+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = one_household_members, col = "blue"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = two_household_members, col = "red"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = three_household_members, col = "orange"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = four_household_members, col = "grey"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = five_household_members, col = "#CAFF70"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = six_household_members, col = "#BF3EFF"))+
   geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = seven_or_more_household_members, col = "#FFFF00"))+
    labs(x = "Number of Household Members", 
       y = "Census Tract",
       title = "Number of household members by Census Tract",
      subtitle = "Halifax County, VA",
        caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")