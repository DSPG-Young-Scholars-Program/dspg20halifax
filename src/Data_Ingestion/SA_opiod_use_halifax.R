library(ggplot2)

datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "CDC Opioid Prescription Data - Halifax (1).csv"))

head(datasource)
ggplot(data = datasource, aes(x= Year)) +
  geom_line(aes(y = Halifax, colour = "red")) + geom_line(aes(y = "US Total", colour = "green"))+
  labs(y = 'Opioid Prescription Rates per 100 People') +
  scale_color_discrete(name = "Location", labels = c("Halifax, VA", "US")) +
  geom_point(aes(y = Halifax)) + geom_point(aes(y = "US Total"))

