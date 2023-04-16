library(tidyverse)
library(tidymodels)
library(easystats)
### carregar os dados
my_df_complete  <-  readRDS(here::here("data","final.RDS"))
my_df <- 
  my_df_complete %>% filter(credito > 0)

my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>% 
  select(-cod_ibge, -uf,-municipio)

my_df <- 
  my_df %>% 
  select(-mesorregiao,-microrregiao)
my_df
### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.75)
## in case you want use bootstrap bootstrap
k_fold <- vfold_cv(training(my_df_split), v = 5)
## recipe-------------------
my_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.8) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.018) %>%
  step_dummy(all_nominal_predictors(),one_hot = FALSE) 


my_prep_df <- 
my_recipe %>% prep() %>% 
  bake(my_df_split)
my_lm <-  
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(credito ~., data = my_prep_df)
my_lm$fit %>% summary()
my_lm %>% tidy()


check_model(my_lm)
