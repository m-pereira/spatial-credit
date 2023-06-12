# quantile regression
# https://www.youtube.com/watch?v=Gtz8ca_4hVg&ab_channel=yuzaRDataScience
## automate selection
# https://www.youtube.com/watch?v=Im293ClFen4&ab_channel=yuzaRDataScience

library(tidyverse)
library(tidymodels)

my_df_complete  <-  readRDS(here::here("data","final.RDS"))
my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>%
  select(-cod_ibge, -uf,-municipio)

# my_df <- 
# my_df %>% 
#   select(-mesorregiao,-microrregiao)
### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.7)
my_df_split
## in case you want use bootstrap bootstrap
k_fold <- vfold_cv(training(my_df_split), v = 5)
## recipe-------------------
lm_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.9) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.2) %>%
  step_dummy(all_nominal_predictors(),one_hot = FALSE)

lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()
lm_recipe

training_data <- 
  lm_recipe %>% prep() %>% 
  bake(training(my_df_split))
testing_data <- 
  lm_recipe %>% prep() %>% 
  bake(testing(my_df_split))

# model median (2nd quantile) regression
lr <- lm(credito ~ ., data = training_data)
library(quantreg)
mr25 <- rq(credito ~ ., data = training_data, 
         tau =  0.25)

mr50 <- rq(credito ~ ., data = training_data, 
           tau =  0.25)

mr75 <- rq(credito ~ ., data = training_data, 
           tau =  0.25)
mr <- rq(credito ~ ., data = training_data, 
         tau = seq(0.25, 0.75, by = 0.25))

# compare models
AIC(lr, mr25,mr50,mr75) # => the lower AIC the better
summary(mr)

library(sjPlot) # I made a video on this 📦
theme_set(theme_bw())
plot_models(lr, mr25, show.values = TRUE, 
            m.labels = c("median model", "Median model"), 
            legend.title = "Model type")

plot_models(mr25, mr75, show.values = TRUE, 
            m.labels = c("Median", "Median model"), 
            legend.title = "Model type")

library(olsrr)
ols_plot_resid_lev(lr)

library(performance) # I made a video on this 📦
check_heteroscedasticity(lr)
check_outliers(lr)
check_normality(lr) 
check_homogeneity(lr)

mr
summary(mr) %>% 
  plot(parm = "jobclass2. Information")

## more info 
# https://yuzar-blog.netlify.app/posts/2022-12-01-quantileregression/
# https://www.youtube.com/watch?v=M_7MOkAm9WU&ab_channel=yuzaRDataScience
library(robustbase)
my_lm_rob <- 
  lmrob(credito ~. ,data = training_data)

my_lm_rob %>% summary()
plot_model(my_lm_rob, type = "pred")
tab_model(my_lm_rob)


check_heteroscedasticity(my_lm_rob)
check_outliers(my_lm_rob)
check_normality(my_lm_rob) 
check_homogeneity(my_lm_rob)



## automate selection
# https://www.youtube.com/watch?v=Im293ClFen4&ab_channel=yuzaRDataScience

