d <- gather(halifax_decennial_data,"median_age","age_estimates", 29:31, na.rm = TRUE)
ggplot()+
  geom_col(data = d, position = "dodge", aes(x = median_age, y = age_estimates, col = "white"))+
  labs(title = "Age Breakdown in Halifax County",
       y = "Age",
       x = "Census Tract")