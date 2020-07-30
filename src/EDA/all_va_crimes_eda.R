
library(here)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(tidycensus)
library(sf)
library(cowplot)

options(scipen = 10000)

# load data
all_va_crime <- read_csv(here("data", "original", "Crime", "full_va_crime", "clean_all_arrest_all_years.csv"))
va_pop_by_race <- read_csv(here("data", "original", "Crime", "full_va_crime", "clean_county_pop_by_race.csv"))

# get va county map data
states <- map_data("state")

#select only the Washington information from the states dataset
va_state <- subset(states, region == "virginia")

#name the built-in county dataset in mapdata to 'counties'
counties <- map_data("county")

#select only the washington information from the states dataset
va_county <- subset(counties, region == "virginia")
va_county$county_cap <- toupper(va_county$subregion)


ditch_the_axis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  legend.position='none'
  #legend.justification='left',
  #legend.direction='horizontal'
)

va_base <- ggplot(data = va_state, 
                  mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.1) + 
  geom_polygon(color = "black", fill = "gray")

## plot total number of crimes in each county ##
county_total <- all_va_crime %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(crime_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

county_total_plot <- va_base +
  geom_polygon(data = county_total, aes(fill = crime_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) + 
  geom_text(data = county_total %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

county_total_year <- all_va_crime %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(crime_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_total_year <- county_total_year %>% filter(county_cap == "HALIFAX")

county_total_year_plot <- ggplot(data = county_total_year %>% filter(county_cap != "HALIFAX"), 
                                 aes(x = incident_year, y = crime_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_total_year, 
            aes(x = incident_year, y = crime_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()


county_black <- all_va_crime %>%
  filter(offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

county_black_plot <- va_base + 
  geom_polygon(data = county_black, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = county_black %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

county_black_year <- all_va_crime %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(crime_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_year <- county_black_year %>% filter(county_cap == "HALIFAX")

county_black_year_plot <- ggplot(data = county_black_year %>% filter(county_cap != "HALIFAX"), 
                                 aes(x = incident_year, y = crime_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_year, 
            aes(x = incident_year, y = crime_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

county_white <- all_va_crime %>%
  filter(offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

county_white_plot <- va_base + 
  geom_polygon(data = county_white, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = county_white %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

county_white_year <- all_va_crime %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(crime_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_year <- county_white_year %>% filter(county_cap == "HALIFAX")

county_white_year_plot <- ggplot(data = county_white_year %>% filter(county_cap != "HALIFAX"), 
                                 aes(x = incident_year, y = crime_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_year, 
            aes(x = incident_year, y = crime_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

total_grid <- plot_grid(county_total_plot, county_total_year_plot,
                        county_black_plot, county_black_year_plot,
                        county_white_plot, county_white_year_plot,
                        nrow = 3,
                        labels = c("All", "",
                                   "Black", "",
                                   "White", ""))

total_title <- ggdraw() + 
  draw_label("Number of total crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(total_title, total_grid, ncol = 1, rel_heights = c(0.1, 1))


## get the top ten most common offenses ##
top_ten_offense <- all_va_crime %>%
  group_by(offense_type) %>%
  tally %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  as.data.frame()

## for each of the top ten offenses
## plot total per capita and 
## by Black and White per capita

# simple assault
all_assault <- all_va_crime %>%
  filter(offense_type == "Simple Assault") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(assault_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

all_assault_plot <- va_base +  
  geom_polygon(data = all_assault, aes(fill = assault_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = all_assault %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

all_assault_year <- all_va_crime %>%
  filter(offense_type == "Simple Assault") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(assault_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_assault_year <- all_assault_year %>% filter(county_cap == "HALIFAX")

all_assault_year_plot <- ggplot(data = all_assault_year %>% filter(county_cap != "HALIFAX"), 
                                aes(x = incident_year, y = assault_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_assault_year, 
            aes(x = incident_year, y = assault_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.025) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()


black_assault <- all_va_crime %>%
  filter(offense_type == "Simple Assault" & 
           offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

black_assault_plot <- va_base + 
  geom_polygon(data = black_assault, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = black_assault %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis


black_assault_year <- all_va_crime %>%
  filter(offense_type == "Simple Assault" & 
           offender_race == "Black or African American") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(assault_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_assault_year <- black_assault_year %>% filter(county_cap == "HALIFAX")

black_assault_year_plot <- ggplot(data = black_assault_year %>% filter(county_cap != "HALIFAX"), 
                                  aes(x = incident_year, y = assault_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_assault_year, 
            aes(x = incident_year, y = assault_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.025) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

white_assault <- all_va_crime %>%
  filter(offense_type == "Simple Assault" & 
           offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

white_assault_plot <- va_base +  
  geom_polygon(data = white_assault, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = white_assault %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

white_assault_year <- all_va_crime %>%
  filter(offense_type == "Simple Assault" & 
           offender_race == "White") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(assault_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_assault_year <- white_assault_year %>% filter(county_cap == "HALIFAX")

white_assault_year_plot <- ggplot(data = white_assault_year %>% filter(county_cap != "HALIFAX"), 
                                  aes(x = incident_year, y = assault_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_assault_year, 
            aes(x = incident_year, y = assault_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()


assault_grid <- plot_grid(all_assault_plot, all_assault_year_plot,
                          black_assault_plot, black_assault_year_plot,
                          white_assault_plot, white_assault_year_plot,
                          nrow = 3,
                          labels = c("All", "",
                                     "Black", "",
                                     "White", ""))
assault_title <- ggdraw() + 
  draw_label("Number of assault crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(assault_title, assault_grid, ncol = 1, rel_heights = c(0.1, 1))


# drug violations
all_drug <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(drug_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

all_drug_plot <- va_base +  
  geom_polygon(data = all_drug, aes(fill = drug_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = all_drug %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis


black_drug <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations" & 
           offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

black_drug_plot <- va_base +  
  geom_polygon(data = black_drug, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = black_drug %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

white_drug <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations" & 
           offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

white_drug_plot <- va_base +  
  geom_polygon(data = white_drug, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = white_drug %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

all_drug_year <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(drug_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_drug_year <- all_drug_year %>% filter(county_cap == "HALIFAX")

all_drug_year_plot <- ggplot(data = all_drug_year %>% filter(county_cap != "HALIFAX"), 
                             aes(x = incident_year, y = drug_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_drug_year, 
            aes(x = incident_year, y = drug_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

black_drug_year <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations" & 
           offender_race == "Black or African American") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(drug_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_drug_year <- black_drug_year %>% filter(county_cap == "HALIFAX")

black_drug_year_plot <- ggplot(data = black_drug_year %>% filter(county_cap != "HALIFAX"), 
                               aes(x = incident_year, y = drug_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_drug_year, 
            aes(x = incident_year, y = drug_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.1) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

white_drug_year <- all_va_crime %>%
  filter(offense_type == "Drug/Narcotic Violations" & 
           offender_race == "White") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(drug_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_drug_year <- white_drug_year %>% filter(county_cap == "HALIFAX")

white_drug_year_plot <- ggplot(data = white_drug_year %>% filter(county_cap != "HALIFAX"), 
                               aes(x = incident_year, y = drug_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_drug_year, 
            aes(x = incident_year, y = drug_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

drug_grid <- plot_grid(all_drug_plot, all_drug_year_plot,
                       black_drug_plot, black_drug_year_plot,
                       white_drug_plot, white_drug_year_plot,
                       nrow = 3,
                       labels = c("All", "",
                                  "Black", "",
                                  "White", ""))
drug_title <- ggdraw() + 
  draw_label("Number of drug crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(drug_title, drug_grid, ncol = 1, rel_heights = c(0.1, 1))


# vandalism
all_vandalism <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE)) %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(vandalism_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

all_vandalism_plot <- va_base +  
  geom_polygon(data = all_vandalism, aes(fill = vandalism_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = all_vandalism %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

black_vandalism <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

black_vandalism_plot <- va_base +  
  geom_polygon(data = black_vandalism, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = black_vandalism %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

white_vandalism <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

white_vandalism_plot <- va_base +  
  geom_polygon(data = white_vandalism, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = white_vandalism %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis


all_vandalism_year <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(vandalism_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_vandalism_year <- all_vandalism_year %>% filter(county_cap == "HALIFAX")

all_vandalism_year_plot <- ggplot(data = all_vandalism_year %>% filter(county_cap != "HALIFAX"), 
                                  aes(x = incident_year, y = vandalism_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_vandalism_year, 
            aes(x = incident_year, y = vandalism_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

black_vandalism_year <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(vandalism_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_vandalism_year <- black_vandalism_year %>% filter(county_cap == "HALIFAX")

black_vandalism_year_plot <- ggplot(data = black_vandalism_year %>% filter(county_cap != "HALIFAX"), 
                                    aes(x = incident_year, y = vandalism_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_vandalism_year, 
            aes(x = incident_year, y = vandalism_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.01) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

white_vandalism_year <- all_va_crime %>%
  filter(grepl("Vandalism", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(vandalism_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_vandalism_year <- white_vandalism_year %>% filter(county_cap == "HALIFAX")

white_vandalism_year_plot <- ggplot(data = white_vandalism_year %>% filter(county_cap != "HALIFAX"), 
                                    aes(x = incident_year, y = vandalism_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_vandalism_year, 
            aes(x = incident_year, y = vandalism_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

vandalism_grid <- plot_grid(all_vandalism_plot, all_vandalism_year_plot,
                            black_vandalism_plot, black_vandalism_year_plot,
                            white_vandalism_plot, white_vandalism_year_plot,
                            nrow = 3,
                            labels = c("All", "",
                                       "Black", "",
                                       "White", ""))

vandalism_title <- ggdraw() + 
  draw_label("Number of vandalism crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(vandalism_title, vandalism_grid, ncol = 1, rel_heights = c(0.1, 1))


# larceny
all_larceny <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE)) %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(larceny_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

all_larceny_plot <- va_base +  
  geom_polygon(data = all_larceny, aes(fill = larceny_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = all_larceny %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

black_larceny <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

black_larceny_plot <- va_base +  
  geom_polygon(data = black_larceny, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = black_larceny %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

white_larceny <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

white_larceny_plot <- va_base +  
  geom_polygon(data = white_larceny, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = white_larceny %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

all_larceny_year <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(larceny_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_larceny_year <- all_larceny_year %>% filter(county_cap == "HALIFAX")

all_larceny_year_plot <- ggplot(data = all_larceny_year %>% filter(county_cap != "HALIFAX"), 
                                aes(x = incident_year, y = larceny_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_larceny_year, 
            aes(x = incident_year, y = larceny_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

black_larceny_year <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(larceny_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_larceny_year <- black_larceny_year %>% filter(county_cap == "HALIFAX")

black_larceny_year_plot <- ggplot(data = black_larceny_year %>% filter(county_cap != "HALIFAX"), 
                                  aes(x = incident_year, y = larceny_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_larceny_year, 
            aes(x = incident_year, y = larceny_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.01) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

white_larceny_year <- all_va_crime %>%
  filter(grepl("Larceny", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(larceny_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_larceny_year <- white_larceny_year %>% filter(county_cap == "HALIFAX")

white_larceny_year_plot <- ggplot(data = white_larceny_year %>% filter(county_cap != "HALIFAX"), 
                                  aes(x = incident_year, y = larceny_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_larceny_year, 
            aes(x = incident_year, y = larceny_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

larceny_grid <- plot_grid(all_larceny_plot, all_larceny_year_plot,
                          black_larceny_plot, black_larceny_year_plot,
                          white_larceny_plot, white_larceny_year_plot,
                          nrow = 3,
                          labels = c("All", "",
                                     "Black", "",
                                     "White", ""))

larceny_title <- ggdraw() + 
  draw_label("Number of larceny crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(larceny_title, larceny_grid, ncol = 1, rel_heights = c(0.1, 1))



# weapon
all_weapon <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE)) %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(weapon_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

all_weapon_plot <- va_base +  
  geom_polygon(data = all_weapon, aes(fill = weapon_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = all_weapon %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

black_weapon <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(black_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

black_weapon_plot <- va_base +  
  geom_polygon(data = black_weapon, aes(fill = black_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = black_weapon %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

white_weapon <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(white_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

white_weapon_plot <- va_base +  
  geom_polygon(data = white_weapon, aes(fill = white_per_cap), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_text(data = white_weapon %>% filter(county_cap == "HALIFAX") %>% slice(1), 
            aes(x = long - 0.15, y = lat + 0.2, label = "H")) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_bw() +
  ditch_the_axis

all_weapon_year <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE)) %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(weapon_per_cap = n/total_pop) %>%
  inner_join(va_county, by = "county_cap")

halifax_weapon_year <- all_weapon_year %>% filter(county_cap == "HALIFAX")

all_weapon_year_plot <- ggplot(data = all_weapon_year %>% filter(county_cap != "HALIFAX"), 
                               aes(x = incident_year, y = weapon_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_weapon_year, 
            aes(x = incident_year, y = weapon_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

black_weapon_year <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE) & 
           offender_race == "Black or African American") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(weapon_per_cap = n/pop_Black) %>%
  inner_join(va_county, by = "county_cap")

halifax_black_weapon_year <- black_weapon_year %>% filter(county_cap == "HALIFAX")

black_weapon_year_plot <- ggplot(data = black_weapon_year %>% filter(county_cap != "HALIFAX"), 
                                 aes(x = incident_year, y = weapon_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_black_weapon_year, 
            aes(x = incident_year, y = weapon_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  ylim(0, 0.03) + 
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

white_weapon_year <- all_va_crime %>%
  filter(grepl("Weapon", offense_type, fixed = TRUE) & 
           offender_race == "White") %>%
  group_by(incident_year, county_cap) %>%
  tally %>%
  as.data.frame() %>%
  inner_join(va_pop_by_race, by = "county_cap") %>%
  mutate(weapon_per_cap = n/pop_White) %>%
  inner_join(va_county, by = "county_cap")

halifax_white_weapon_year <- white_weapon_year %>% filter(county_cap == "HALIFAX")

white_weapon_year_plot <- ggplot(data = white_weapon_year %>% filter(county_cap != "HALIFAX"), 
                                 aes(x = incident_year, y = weapon_per_cap, group = county_cap)) + 
  geom_line(alpha = 0.1) +
  geom_line(data = halifax_white_weapon_year, 
            aes(x = incident_year, y = weapon_per_cap),
            color = "red", lwd = 1.3) + 
  scale_x_continuous(breaks = c(seq(2010, 2019)),
                     labels = c(seq(2010, 2019))) +
  xlab("Year") + ylab("Crimes per capita") +
  theme_bw()

weapon_grid <- plot_grid(all_weapon_plot, all_weapon_year_plot,
                         black_weapon_plot, black_weapon_year_plot,
                         white_weapon_plot, white_weapon_year_plot,
                         nrow = 3,
                         labels = c("All", "",
                                    "Black", "",
                                    "White", ""))

weapon_title <- ggdraw() + 
  draw_label("Number of weapon crimes per capita in each county from 2010 to 2019", 
             fontface='bold')
plot_grid(weapon_title, weapon_grid, ncol = 1, rel_heights = c(0.1, 1))

