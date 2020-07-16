library(devtools)
library(tidyr)
library(ggplot2)
library(dplyr)

new_data <- gather(halifax_decennial_data,"races","race_estimates", 3:8, na.rm = TRUE)

new_data$races <- factor(new_data$races, levels = c("white_race", "black_race", "hispanic_ethnicity", "asian_race", "native_race", "two_races"))
ggplot()+
  geom_col(data = new_data, aes(x = races, y = race_estimates))+
  labs(y = "population",
    title = "Race Breakdown in Halifax County, VA",
    caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")
