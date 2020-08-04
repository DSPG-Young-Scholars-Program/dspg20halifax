library(ggplot2)

#read CSV
datasource <- readr::read_csv(here::here("data",
                                         "original", "Substance_Abuse",
                                         "CDC Opioid Prescription Data - Halifax.csv"))
datasource
total <- datasource$`US Total`
va <- datasource$VA

#plot the data
ggplot(data = datasource, aes(x= Year)) +
  #plot of Halifax and VA state over time
  geom_line(aes(y = Halifax, colour = "red")) + geom_line(aes(y = total, colour = "green")) +
  geom_line(aes(y = va, colour = "blue"))
  labs(y = 'Opioid Prescription Rates per 100 People') +
  #relabel the scale appropriately
  scale_color_discrete(name = "Location", labels = c("Halifax, VA", "US", "VA")) +
  geom_point(aes(y = Halifax)) + geom_point(aes(y = total))

