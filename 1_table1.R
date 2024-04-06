

# Table 1: Descriptive statistics
# January 2024
# Borg DN

# WD
here()

# Load data
d <- read_xlsx('Judo variability analysis - Index adjusted 2.xlsx',
               sheet = 1) %>%
  clean_names() %>%
  mutate(sex = as.factor(sex),
         weight = as.numeric(weight),
         age = as.integer(age),
         y = as.factor(type_of_results),
         medalist = as.factor(medalist),
         medalist_y = recode_factor(medalist,
                                    'No' = '0',
                                    'Yes' = '1'),
         sport_class = as.factor(old_visual_class),
         ground_techiniques_per_match = ground_techiniques/total_matches) %>%
  drop_na(weight) # This drops the four athletes (n = 1 female and n = 3 males) who did not start the tournment (i.e., 'DNS')


# Count by sex
table(d$sex)

# Mean and SD
d %>%
  group_by(sex) %>%
  summarise(
    across(
      .cols = c(age,
                weight),
      .fns = list(mean = ~mean(., na.rm = T),
                  sd = ~sd(., na.rm = T)))) %>%
  View()

# Matches
d %>%
  group_by(sex) %>%
  summarise(
    across(
      .cols = c(total_matches,
                throws_per_match,
                ground_techiniques_per_match,
                penalties_per_match),
      .fns = list(median = ~median(., na.rm = T),
                  q1 = ~quantile(., prob = 0.25, na.rm = T),
                  q3 = ~quantile(., prob = 0.75, na.rm = T)))) %>%
  t() %>%
  as.data.frame()

# Sport class
d %>%
  group_by(sex, sport_class) %>%
  summarise(n=n())

# Weight class
d %>%
  group_by(sex, weight_categories) %>%
  summarise(n=n())


#### End
