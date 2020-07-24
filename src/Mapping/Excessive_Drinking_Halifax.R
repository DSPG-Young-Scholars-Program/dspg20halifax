library(ggplot2)
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Halifax (1).csv"))
datasource

# low <- c(13, 14, 14, 12, 12, 6, 6, 6, 6, 5)
# high <- c(14, 15, 15, 13, 13, 18, 18, 19, 12, 11)
list <- c(14, 14, 14, 13, 13, 10 ,10, 11, 8, 8)
va <- c(17, 17, 17, 17, 17, 16, 16, 16, 16, 16)
ggplot(data = datasource, aes(x = X1)) +
   geom_line(aes(y = list, colour = "#CC6666")) + geom_line(aes(y = va, colour = "#9999CC")) +
  labs(x = 'Year', y = '% Excessive Drinking') + scale_x_continuous(breaks = seq(2011, 2020, 2), lim = c(2011, 2020)) +
  scale_y_continuous(breaks = seq(5, 20, 5), lim = c(5, 20))  +
  scale_color_manual(labels = c("Halifax, VA", "VA"), values = c("#CC6666",  "#9999CC")) +
  guides(color=guide_legend("Location"))
  # geom_smooth(method = "auto", se = TRUE, fullrange = FALSE, level = 0.95, stat_smooth = list)
