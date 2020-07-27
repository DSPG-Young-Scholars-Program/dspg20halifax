install.packages("hrbrthemes")
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(readr)

#read in data source
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Ed Data VDH - Copy of Halifax.csv"))
datasource
# ggplot for the data
ggplot(datasource, aes(x =Year, y =Count, fill=Drug))+
  geom_area() + theme_classic() + labs(x = "Year", y = "ED Overdose Visits") +
  ggtitle("Emergency Department Overdose Visits in Halifax, VA")
