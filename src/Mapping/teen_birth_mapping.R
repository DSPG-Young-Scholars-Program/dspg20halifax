new_data <- Teen_Births_Southside %>% mutate(is_halifax = case_when(County == "Halifax" ~ "Yes",
                                                                    County != "Halifax" ~ "No"))

ggplot(new_data, aes(x = Birth_Rate, y = reorder(County, Birth_Rate), fill = is_halifax)) +
  geom_col(position = "dodge") +
  labs(title = "Teen Births in Virginia", subtitle = "Southside Region", fill = "Halifax", x = "Birth rate (per 1000 females aged 15-19)", y = "County",
       caption = "Data source: County Health Rankings, 2012 - 2018 measure") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8))
