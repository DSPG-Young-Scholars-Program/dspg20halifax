

#school_data <- readr::read_csv(here::here("data", "original", "Schools", "halifax_school_data.csv"))
school_data <- readr::read_csv(here::here("data", "original", "Schools", "halifax_school_data_updated.csv"))

## Sparse data - lots of zeroes for suspensions. Not surprising.
## May be related to level of school - pull out high school?
school_data %>%
  select(contains("susp")) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

## Columns to aggregate across disability levels (not interested in this variable for now)
sum_cols <- c("students_chronically_absent", "students_susp_out_sch_single", "students_susp_out_sch_multiple", 
              "expulsions_no_ed_serv", "expulsions_with_ed_serv", "expulsions_zero_tolerance", 
              "students_corporal_punish", "students_arrested", "students_referred_law_enforce")

## Aggregate data but keep in long format
aggregated_data <- school_data %>%
  select(-disability, -lep) %>%
  group_by(year, school_name, race, sex, enrollment, reduced_price_lunch, free_lunch, free_or_reduced_price_lunch, teachers_fte) %>%
  summarize(across(all_of(sum_cols), function(x) sum(x, na.rm = TRUE)))

## First looks
aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>% ## Only years when this data is reported
  ggplot() +
  geom_line(aes(x = year, y = students_susp_out_sch_single, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>% ## Only years when this data is reported
  ggplot() +
  geom_line(aes(x = year, y = students_susp_out_sch_multiple, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

## Low counts for other races make things problematic. Scale to enrollment
aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race %in% c("White", "Black"), sex != "Total", year %in% c(2011, 2013, 2015)) %>% ## Only years when this data is reported
  ggplot() +
  geom_line(aes(x = year, y = students_susp_out_sch_single / enrollment, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race %in% c("White", "Black"), sex != "Total", year %in% c(2011, 2013, 2015)) %>% ## Only years when this data is reported
  ggplot() +
  geom_line(aes(x = year, y = students_susp_out_sch_multiple / enrollment, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

## So few expulsions - probably not enough to explore.
aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>%
  ggplot() +
  geom_line(aes(x = year, y = expulsions_no_ed_serv, group = interaction(race, sex), color = race)) +
  geom_point(aes(x = year, y = expulsions_no_ed_serv, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>%
  ggplot() +
  geom_line(aes(x = year, y = expulsions_with_ed_serv, group = interaction(race, sex), color = race)) +
  geom_point(aes(x = year, y = expulsions_with_ed_serv, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>%
  ggplot() +
  geom_line(aes(x = year, y = expulsions_zero_tolerance, group = interaction(race, sex), color = race)) +
  geom_point(aes(x = year, y = expulsions_zero_tolerance, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

## Contact with law enforcement
## Looks like 2013 may be a weird year - hard to believe there were 0 cases
aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race != "Total", sex != "Total", year %in% c(2011, 2013, 2015)) %>%
  ggplot() +
  geom_line(aes(x = year, y = students_arrested, group = interaction(race, sex), color = race)) +
  geom_point(aes(x = year, y = students_arrested, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)

aggregated_data %>%
  filter(str_detect(school_name, "HIGH"), race %in% c("White", "Black"), sex != "Total", year %in% c(2011, 2013, 2015)) %>%
  ggplot() +
  geom_line(aes(x = year, y = students_referred_law_enforce / enrollment, group = interaction(race, sex), color = race)) +
  geom_point(aes(x = year, y = students_referred_law_enforce / enrollment, group = interaction(race, sex), color = race)) +
  facet_grid(~sex)





