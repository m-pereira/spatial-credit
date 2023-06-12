# https://yuzar-blog.netlify.app/posts/2022-05-31-glmulti/
library(car)        # extracts model results
library(tictoc)     # checks running time
library(sjPlot)     # visualizes model results
library(glmulti)    # finds the BEST model
#library(flextable)  # beautifies tables
library(performance)# checks and compares quality of models
library(tidyverse)
library(tidymodels)

my_df_complete  <-  readRDS(here::here("data","final.RDS"))
my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>%
  select(-cod_ibge, -uf,-municipio)
my_df <- 
 my_df %>% 
   select(-mesorregiao,-microrregiao)
### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.8)
my_df_split
## recipe-------------------
lm_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.8) %>% 
  # step_novel(all_nominal_predictors()) %>% 
  # step_unknown(all_nominal_predictors()) %>% 
  # step_other(all_nominal_predictors(), threshold = 0.025) %>%
  # step_dummy(all_nominal_predictors(),one_hot = FALSE)
  step_rm(all_nominal_predictors())
lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% glimpse()
lm_recipe

training_data <- 
  lm_recipe %>% prep() %>% 
  bake(training(my_df_split)) %>% 
  select(-galinaceos_total,
         -suino_total,
         -moto,
         -trator,
         -valor_area_temp)
testing_data <- 
  lm_recipe %>% prep() %>% 
  bake(testing(my_df_split))

tic()
g_model <-
glmulti(credito   ~ .,
        data   = training_data, 
        crit   = aicc,       # AICC corrected AIC for small samples
        level  = 2,          # 2 with interactions, 1 without  
        method = "g",        # "d", or "h", or "g"
        # testar g (genetic algorithm)
        family = gaussian, 
        fitfunction = glm,   # Type of model (LM, GLM, GLMER etc.)
        confsetsize = 100#,
        #plotty=FALSE
        )   # Keep 100 best models

toc()
g_model
optimal_model_glmulti_genetic    <- g_model@objects[[1]]
compare_performance(optimal_model_glmulti_genetic)
optimal_model_glmulti_genetic
plot(g_model, type = "s")

best_model <- g_model@objects[[2]]


car::Anova(best_model)

plot_model(best_model, type = "int") %>% 
  plot_grid()
