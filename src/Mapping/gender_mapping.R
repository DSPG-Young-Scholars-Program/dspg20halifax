library(devtools)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

gender <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "tidy",
                        variables = c(male_pop = "P012002", female_pop = "P012026"))
ggplot()+
  geom_col(data = gender , aes(x = variable, y = value))+
  scale_fill_brewer("Set2")+
  labs(x = "Gender",
    y = "population",
       title = "Gender Breakdown in Halifax County, VA",
       caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")
