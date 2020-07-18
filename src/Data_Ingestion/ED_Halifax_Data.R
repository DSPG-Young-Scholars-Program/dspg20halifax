install.packages("hrbrthemes")
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

#Ed.Data.VDH...Copy.of.Halifax$Drug <-
#  factor(Ed.Data.VDH...Copy.of.Halifax$Drug , levels=c("Opiod", "Heroin") )

data <- ggplot(Ed.Data.VDH...Copy.of.Halifax, aes(x =Year, y =Count, fill=Drug, text = Drug))+
  geom_area() + theme_ipsum() + labs(x = "Year", y = "ED Overdose Visits") +
  scale_fill_viridis(discrete = TRUE) + ggtitle("Emergency Department Overdose Visits in Halifax, VA")

data_real <- ggplotly(data, tooltip = "text")
data_real
