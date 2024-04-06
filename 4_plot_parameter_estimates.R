

# Plot of parameter estimates for each model
# January 2024
# Borg DN

# Male model parameter estimates
## Load saved male model

posterior <- as.matrix(fit)

dplot <-
  posterior %>%
  as.data.frame() %>%
  mutate(Age = b_age_s,
         Weight = b_weight_s,
         `Sport Class—B2` = b_sport_classB2,
         `Sport Class—B3` = b_sport_classB3,
         `Throws per match` = b_throws_per_match_s,
         `Groundwork techniques per match` = b_ground_techniques_per_match_s,
         `Penalties per match` = b_penalties_per_match_s) %>%
  select(Age,
         Weight,
         `Sport Class—B2`,
         `Sport Class—B3`,
         `Throws per match`,
         `Groundwork techniques per match`,
         `Penalties per match`) %>%
  pivot_longer(names_to = 'parameter',
               values_to = 'effect',
               cols = everything()) %>%
  mutate(parameter = factor(parameter, levels = c('Age',
                                                  'Weight',
                                                  'Sport Class—B2',
                                                  'Sport Class—B3',
                                                  'Throws per match',
                                                  'Groundwork techniques per match',
                                                  'Penalties per match')))

dplot %>%
  ggplot()+
  stat_halfeye(aes(x = effect,
                   y = parameter),
               .width = c(0.66,0.95),
               fill = '#3b528b',
               alpha = 0.65)+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(n.breaks = 7)+
  geom_vline(xintercept = 0, colour = 'black', size = 0.5, linetype = 5)+
  coord_cartesian(xlim = c(-2.5,3.5))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  facet_grid(~'Parameter estimates: Male model')+
  labs(x = 'Effect (logit)',
       y = 'Parameter') -> p1; p1


# Female model parameter estimates
## Load saved female model

posterior <- as.matrix(fit)

dplot <-
  posterior %>%
  as.data.frame() %>%
  mutate(Age = b_age_s,
         `Sport Class—B2` = b_sport_classB2,
         `Sport Class—B3` = b_sport_classB3,
         `Throws per match` = b_throws_per_match_s,
         `Groundwork techniques per match` = b_ground_techniques_per_match_s,
         `Penalties per match` = b_penalties_per_match_s) %>%
  select(Age,
         `Sport Class—B2`,
         `Sport Class—B3`,
         `Throws per match`,
         `Groundwork techniques per match`,
         `Penalties per match`) %>%
  pivot_longer(names_to = 'parameter',
               values_to = 'effect',
               cols = everything()) %>%
  mutate(parameter = factor(parameter, levels = c('Age',
                                                  'Sport Class—B2',
                                                  'Sport Class—B3',
                                                  'Throws per match',
                                                  'Groundwork techniques per match',
                                                  'Penalties per match')))

dplot %>%
  ggplot()+
  stat_halfeye(aes(x = effect,
                   y = parameter),
               .width = c(0.66,0.95),
               fill = '#ed6925',
               alpha = 0.75)+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(n.breaks = 7)+
  geom_vline(xintercept = 0, colour = 'black', size = 0.5, linetype = 5)+
  coord_cartesian(xlim = c(-2.5,3.5))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  facet_grid(~'Parameter estimates: Female model')+
  labs(x = 'Effect (logit)',
       y = 'Parameter') -> p2; p2


# Panel
plot_grid(p1,
          p2,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          labels = c('A','B'),
          ncol = 1)

# Save
ggsave(file = "figure_parameter_estimates.png",
       width = 6,
       height = 7,
       dpi = 600)


#### End

  
