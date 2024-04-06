

# Fitted values: Men
## Load saved male model

# Look at parameter estimates
posterior <- as.matrix(fit)

color_scheme_set('darkgray')
mcmc_areas(posterior,
           pars = c('b_age_s',
                    'b_weight_s',
                    'b_sport_classB2',
                    'b_sport_classB3',
                    'b_throws_per_match_s',
                    'b_ground_techniques_per_match_s'))+
  geom_vline(xintercept = 0)

# Load raw data
dsub <- read_xlsx('Judo variability analysis - Index adjusted 2.xlsx',
               sheet = 1) %>%
  clean_names() %>%
  mutate(sex = as.factor(sex),
         weight = as.numeric(weight),
         age = as.integer(age),
         y = as.factor(type_of_results),
         world_ranking = as.integer(world_ranking),
         medalist = as.factor(medalist),
         medalist_y = recode_factor(medalist,
                                    'No' = '0',
                                    'Yes' = '1'),
         sport_class = as.factor(old_visual_class)) %>%
  filter(sex == 'Male') %>%
  drop_na(weight) # Drop NA for weight because these are actually 'DNS' - did not start

# Constant for transformation
constant <- 5

# Get fitted values
# Age
plot_age <- function(p){conditional_effects(fit, effects = 'age_s', prob = {{p}})[1] %>%
    as.data.frame() %>%
    rename(
      x = age_s.age_s,
      y = age_s.estimate__,
      lower = age_s.lower__,
      upper = age_s.upper__)
}

plot_age_90 <- plot_age(p = 0.9)
plot_age_66 <- plot_age(p = 0.66)

mu = mean(dsub$age)
sigma = sd(dsub$age)

min(dsub$age)
max(dsub$age)
table(dsub$medalist, dsub$age)

plot_age_90 %>%
  mutate(x = (x*sigma)+mu) %>%
  filter(x >19) %>%
  ggplot(aes(x = x,
             y = y,
             ymin = lower,
             ymax = upper))+
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.275,
              fill = '#3b528b')+
  geom_ribbon(data = plot_age_66 %>%
                mutate(x = (x*sigma)+mu) %>%
                filter(x >19),
              aes(ymin = lower,
                  ymax = upper),
              alpha = 0.6,
              fill = '#3b528b')+
  scale_y_continuous(limits = c(0,1))+
  coord_cartesian(xlim = c(18,61))+
  scale_x_continuous(n.breaks = 7)+
  geom_line()+
  theme_classic()+
  labs(x = 'Age (years)',
       y = 'Probability of a medal')+
  guides(fill = 'none') -> p1; p1



# Throws per match
plot_throw <- function(p){conditional_effects(fit, effects = 'throws_per_match_s', prob = {{p}})[1] %>%
    as.data.frame() %>%
    rename(
      x = throws_per_match_s.throws_per_match_s,
      y = throws_per_match_s.estimate__,
      lower = throws_per_match_s.lower__,
      upper = throws_per_match_s.upper__)
  }

plot_g_90 <- plot_throw(p = 0.9)
plot_g_66 <- plot_throw(p = 0.66)

plot_g_90 %>%
  mutate(x1 = x/constant,
         x2 = round(x1, digits = 2)) %>%
  filter(x2 %in% c(0,0.24,0.51,0.75,.99,1.25)) %>%
  ggplot(aes(x = x2,
             y = y))+
  geom_pointinterval(aes(ymin = lower,
                         ymax = upper),
                     size = 0.5,
                     colour = '#3b528b')+
  geom_pointinterval(data = plot_g_66 %>%
                       mutate(x1 = x/constant,
                              x2 = round(x1, digits = 2)) %>%
                       filter(x2 %in% c(0,0.24,0.51,0.75,.99,1.25)),
                     aes(ymin = lower,
                         ymax = upper),
                     size = 6,
                     colour = '#3b528b')+
  theme_classic()+
  scale_x_continuous(n.breaks = 6)+
  coord_cartesian(xlim = c(0,1.25))+
  labs(x = 'Throws per match',
       y = 'Probability of a medal')+
  guides(colour = 'none') -> p2; p2



# Ground techniques
plot_ground <- function(p){conditional_effects(fit, effects = 'ground_techniques_per_match_s', prob = {{p}})[1] %>%
    as.data.frame() %>%
    rename(
      x = ground_techniques_per_match_s.ground_techniques_per_match_s,
      y = ground_techniques_per_match_s.estimate__,
      lower = ground_techniques_per_match_s.lower__,
      upper = ground_techniques_per_match_s.upper__)
}

plot_g_90 <- plot_ground(p = 0.9)
plot_g_66 <- plot_ground(p = 0.66)

plot_g_90 %>%
  mutate(x1 = x/constant,
         x2 = round(x1, digits = 2)) %>%
  filter(x2 %in% c(0,0.25,0.49,0.75,1)) %>%
  ggplot(aes(x = x2,
             y = y))+
  geom_pointinterval(aes(ymin = lower,
                         ymax = upper),
                     size = 0.5,
                     colour = '#3b528b')+
  geom_pointinterval(data = plot_g_66 %>%
                       mutate(x1 = x/constant,
                              x2 = round(x1, digits = 2)) %>%
                       filter(x2 %in% c(0,0.25,0.49,0.75,1)),
                     aes(ymin = lower,
                         ymax = upper),
                     size = 6,
                     colour = '#3b528b')+
  theme_classic()+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(limits = c(0,1))+
  coord_cartesian(xlim = c(0,1))+
  labs(x = 'Groundwork techniques per match',
       y = 'Probability of a medal') -> p3; p3


# Penalties
plot_pen <- function(p){conditional_effects(fit, effects = 'penalties_per_match_s', prob = {{p}})[1] %>%
    as.data.frame() %>%
    rename(
      x = penalties_per_match_s.penalties_per_match_s,
      y = penalties_per_match_s.estimate__,
      lower = penalties_per_match_s.lower__,
      upper = penalties_per_match_s.upper__)
}

plot_g_90 <- plot_pen(p = 0.9)
plot_g_66 <- plot_pen(p = 0.66)

plot_g_90 %>%
  mutate(x1 = x/constant,
         x2 = round(x1, digits = 2)) %>%
  filter(x2 %in% c(0,1.00,2.00,3.00)) %>%
  ggplot(aes(x = x2,
             y = y))+
  geom_pointinterval(aes(ymin = lower,
                         ymax = upper),
                     size = 0.5,
                     colour = '#3b528b')+
  geom_pointinterval(data = plot_g_66 %>%
                       mutate(x1 = x/constant,
                              x2 = round(x1, digits = 2)) %>%
                       filter(x2 %in% c(0,1.00,2.00,3.00)),
                     aes(ymin = lower,
                         ymax = upper),
                     size = 6,
                     colour = '#3b528b')+
  theme_classic()+
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(limits = c(0,1))+
  coord_cartesian(xlim = c(0,3))+
  labs(x = 'Penalties per match',
       y = 'Probability of a medal') -> p4; p4

# Panel
plot_grid(p1,
          p2,
          p3,
          p4,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          labels = c('A','B','C','D'),
          ncol = 1)

# Save
ggsave(file = "figure_men.png",
       width = 3.5,
       height = 10,
       dpi = 600)



#### End
