

# Judo performance
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
         world_ranking = as.integer(world_ranking),
         medalist = as.factor(medalist),
         medalist_y = recode_factor(medalist,
                                    'No' = '0',
                                    'Yes' = '1'),
         sport_class = as.factor(old_visual_class)) %>%
  filter(sex == 'Male') %>%
  drop_na(weight) # Drop NA for weight because these are actually 'DNS' - did not start

# Missing
vis_miss(d)

# Look
names(d)
head(d,10)

# Plot
d %>%
  ggplot(aes(x = y,
             y = age))+
  geom_boxplot(outlier.size = -1)+
  geom_jitter(width = 0.1)

d %>%
  ggplot(aes(x = medalist_y,
             y = technical_variability))+
  geom_boxplot(outlier.size = -1)+
  geom_jitter(width = 0.1)

age_eff <- d %>% filter(sex == 'Male')
table(age_eff$medalist,age_eff$age)

age_eff %>%
  group_by(age,medalist) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = medalist,
              values_from = n) %>%
  mutate_at(c(2:3), ~replace(., is.na(.), 0)) %>%
  mutate(pct_medal = Yes/(No+Yes)*100) %>%
  ggplot(aes(x = age,
             y = pct_medal))+
  geom_point()+
  geom_smooth(method = 'lm')

# Missing data
vis_miss(d)

# Standardize
dsub <-
  d %>%
  mutate(age_s = as.numeric(scale(age, center = T, scale = T)),
         weight_s = as.numeric(scale(weight, center = T, scale = T)),
         ground_techiniques = as.integer(ground_techiniques),
         total_matches = as.integer(total_matches),
         ground_techniques_per_match = ground_techiniques/total_matches,
         throws_per_match_s = throws_per_match) %>%
  dplyr::select(sex,
                age_s,
                sport_class,
                throws_per_match,
                ground_techniques_per_match,
                penalties_per_match,
                weight_s,
                medalist_y)

vis_miss(dsub)

# Check predictors
cor_matrix <- cor(dsub %>%
                    dplyr::select(age_s,
                                  weight_s,
                                  throws_per_match,
                                  ground_techniques_per_match,
                                  penalties_per_match),
                  method = "spearman") %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ round(., digits = 2)))

ggcorrplot(cor_matrix,
           hc.order = T,
           type = "lower",
           lab = T,
           lab_col = 'white')+
  scale_fill_continuous()

# Save
# ggsave(file = "x_correlations_men.png",
#        width = 6,
#        height = 5,
#        dpi = 600)

# Bootstrap CI for strongest correlations
# cor_boot <- function(x,y){corci({{x}},{{y}}, method = 'spearman', nboot = 2000)}
# round(cor_boot(dsub$throws_per_match, dsub$ground_techiniques_per_match)$conf.int,2)

# Relabel
constant = 5
dsub$ground_techniques_per_match_s = dsub$ground_techniques_per_match*constant
dsub$throws_per_match_s = dsub$throws_per_match*constant
dsub$penalties_per_match_s = dsub$penalties_per_match*constant

# Candidate model
fit <- brm(medalist_y ~ age_s + weight_s + sport_class + throws_per_match_s + ground_techniques_per_match_s + penalties_per_match_s,
          family = bernoulli(link = "logit"),
          data = dsub,
          prior = set_prior('horseshoe(df = 1, scale_global = 1)', class = "b"),
          cores = 8,
          iter = 40000,
          chains = 4,
          control = list(adapt_delta = 0.9),
          seed = 123)

# Parameter estimates
posterior <- as.matrix(fit)
color_scheme_set('darkgray')
mcmc_areas(posterior,
           pars = c('b_weight_s',
                    'b_age_s',
                    'b_sport_classB2',
                    'b_sport_classB3',
                    'b_throws_per_match_s',
                    'b_ground_techniques_per_match_s',
                    'b_penalties_per_match_s'),
           prob_outer = .95,
           prob = 2/3,
           point_est = 'median')+
  geom_vline(xintercept = 0, colour = 'darkorange3')+
  theme_bw()

# Post prob of effects
gather_draws(fit, b_sport_classB2) %>% mean_qi(.value>0)
gather_draws(fit, b_sport_classB3) %>% mean_qi(.value>0)

# Keep all variables in the model

# Save best model
save(fit, file = "fit_men.RData")

# Summary including posterior prob of effects
summary(fit)
gather_draws(fit, b_age_s) %>% mean_qi(.value<0)
gather_draws(fit, b_weight_s) %>% mean_qi(.value>0)
gather_draws(fit, b_sport_classB2) %>% mean_qi(.value>0)
gather_draws(fit, b_sport_classB3) %>% mean_qi(.value>0)
gather_draws(fit, b_throws_per_match_s) %>% mean_qi(.value>0)
gather_draws(fit, b_ground_techniques_per_match_s) %>% mean_qi(.value>0)
gather_draws(fit, b_penalties_per_match_s) %>% mean_qi(.value>0)


# Cross validate
dmod <- dsub %>% mutate(row_id = 1:nrow(.))

results <- data.frame()

for (k in unique(dmod$row_id)) {
  
  # Create training and validation datasets
  train_dat <- dplyr::filter(dmod, row_id != k)
  val_dat <- dplyr::filter(dmod, row_id == k)
  
  # Fit the logistic regression model using glm
  fit <- brm(medalist_y ~ age_s + weight_s + sport_class + throws_per_match_s + ground_techniques_per_match_s + penalties_per_match_s,
             family = bernoulli(link = "logit"),
             data = train_dat,
             prior = set_prior('horseshoe(df = 1, scale_global = 1)', class = "b"),
             cores = 8,
             iter = 4000,
             chains = 4,
             control = list(adapt_delta = 0.9),
             seed = 123)
  
  # Predict on validation set
  pred_prob <- predict(fit, newdata = val_dat, type = "response")
  
  # Store the results
  val_dat$predicted_prob = pred_prob
  results <- rbind(results, val_dat)
}


# From model
pred <- results$predicted_prob[,1]
y_var <- dsub$medalist_y

df <- cbind(y_var,pred) %>%
  as.data.frame()

#write.csv(df, file = 'cv_final_model_men.csv')
df <- read.csv('cv_final_model_men.csv')

# Relabel
df$y_var[df$y_var == 0] <- 2
df$y_var[df$y_var == 1] <- 0
df$y_var[df$y_var == 2] <- 1

# Plot ROC
roc(df$y_var, df$pred) %>%
  ggroc(colour = '#3b528b',
        alpha = 0.8)+
  geom_segment(aes(x = 1,
                   xend = 0,
                   y = 0,
                   yend = 1),
               color = 'gray50',
               linetype = 'longdash',
               size = 0.35)+
  labs(x = 'Specificity',
       y = 'Sensitivity')+
  theme_bw()+
  theme(axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid = element_blank(), # '#440154FF'
        legend.position = c(0.7, 0.2)) -> p1; p1

# Save
ggsave(file = "roc_men.png",
       width = 5,
       height = 4,
       dpi = 600)

# Calibration plot
calPerf = val.prob.ci.2(df$pred,  df$y_var)




#### End


