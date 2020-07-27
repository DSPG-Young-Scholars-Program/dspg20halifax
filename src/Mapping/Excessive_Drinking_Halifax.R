library(ggplot2)

#load the data source from the CSV
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Halifax (1).csv"))
datasource

#grab the '% Excessive Drinking' column for Halifax
list <- c(14, 14, 14, 13, 13, 10 ,10, 11, 8, 8)
#grab the '% Excessive Drinking' column for VA in general
va <- c(17, 17, 17, 17, 17, 16, 16, 16, 16, 16)
ggplot(data = datasource, aes(x = X1)) +
  #add geom_lines for each location
   geom_line(aes(y = list, colour = "#CC6666")) +
  geom_line(aes(y = va, colour = "#9999CC")) +
  labs(x = 'Year', y = '% Excessive Drinking') + scale_x_continuous(breaks = seq(2011, 2020, 2), lim = c(2011, 2020)) +
  #add relevant scaling for y axis
  scale_y_continuous(breaks = seq(5, 20, 5), lim = c(5, 20))  +
  #relabel the scale appropriately
  scale_color_manual(labels = c("VA", "Halifax, VA"), values = c("#CC6666",  "#9999CC")) +
  guides(color=guide_legend("Location"))
  # geom_smooth(aes(y = list), method = "lm", formula = y ~ poly(x, 7), se = FALSE) +
  # geom_smooth(aes(y = va), method = "lm", formula = y ~ poly(x, 8), se = FALSE)

