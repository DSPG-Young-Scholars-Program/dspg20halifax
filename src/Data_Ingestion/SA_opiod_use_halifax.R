library(ggplot2)

#read CSV
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "CDC Opioid Prescription Data - Halifax (1).csv"))
total <- datasource$`US Total`

#plot the data
ggplot(data = datasource, aes(x= Year)) +
  #plot of Halifax and VA state over time
  geom_line(aes(y = Halifax, colour = "red")) + geom_line(aes(y = total, colour = "green")) +
  labs(y = 'Opioid Prescription Rates per 100 People') +
  #relabel the scale appropriately
  scale_color_discrete(name = "Location", labels = c("Halifax, VA", "US")) +
  geom_point(aes(y = Halifax)) + geom_point(aes(y = total))

