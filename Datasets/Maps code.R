ggplot(Teen_Births_VA, aes(x = Birth Rate, y = County))+
geom_point()

#creating a graph about no High school degree percentages
va_no_HS_degree <- get_acs(geography = "county", variable = "DP02_0060P", 
                 year = 2018, geometry = TRUE, 
                   state = "VA")
ggplot(va_no_HS_degree, aes(fill= estimate))+
  geom_sf()+
  theme_minimal()+
  labs(title = "Virginians with no HS degree")

#creating a map about mental health counselors in VA
mental_health <- get_acs(geography = "county", variable = "B24114_136", 
                           year = 2018, geometry = TRUE, 
                           state = "VA")
ggplot(mental_health, aes(fill= estimate))+
  geom_sf()+
  theme_minimal()+
  labs(title = "Mental Health counselors in Virginia")

#teen birth rates across country
ggplot(National_teen_births, aes(x = RATE, y = STATE))+
  geom_col()+
  labs(title = "Teen Birth rates across the country")