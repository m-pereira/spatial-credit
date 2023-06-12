# booststrap regression
# https://www.youtube.com/watch?v=sjCxIHVGkdE&ab_channel=yuzaRDataScience
# https://markjrieke.github.io/workboots/
# https://patrick-rockenschaub.com/post/2022-01-26-tidymodels-optimism-bootstrap/

# https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/05-resampling-methods.html


library(tidyverse)
library(tidymodels)
library(workboots)

### carregar os dados
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
boots_folds <- bootstraps(training(my_df_split), times = 1e3, apparent = TRUE)
boots_folds
## recipe-------------------
boots_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.9) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.5) %>%
  step_dummy(all_nominal_predictors(),one_hot = TRUE)

boots_recipe %>% prep() %>% 
  bake(my_df_split) %>% glimpse()

boots_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()

#boots_recipe %>% prep() %>% bake(my_df_split) %>% View()

##spec-------------------
boots_spec <-
  linear_reg(
     penalty = tune(),
     mixture = tune()
    ) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

##workflow----------------------
boots_workflow <-
  workflow() %>%
  add_recipe(boots_recipe) %>%
  add_model(boots_spec)
boots_workflow


## tune grid------------------
set.seed(7785)

boots_grid <- grid_latin_hypercube(
  penalty(range = c(-10, 0), trans = log10_trans()), 
  mixture(range = c(0, 1), trans = NULL),
  size = 10)

boots_grid
set.seed(7785)
doParallel::registerDoParallel()

boots_tune <-
  tune_grid(boots_workflow,
            boots_folds,
            grid = boots_grid,
            metrics = 
              metric_set(
                rsq,
                rmse, # traditional
                huber_loss, # menos sensivel a outliers ao rmse
                ccc, # Concordance correlation coefficient sensível a outleirs
                iic,  # mede a correlação ideal
                rpiq,rpd # medem consistency/correlation não acurácia
              )
  )
# show_notes(.Last.tune.result)
boots_tune
### show best ------------------------
show_best(boots_tune,metric = "rmse")
show_best(boots_tune,metric = "rsq")
show_best(boots_tune,metric = "ccc")
show_best(boots_tune,metric = "huber_loss")
show_best(boots_tune,metric = "icc")
show_best(boots_tune,metric = "ccc")
show_best(boots_tune,metric = "rpiq")
show_best(boots_tune,metric = "rpd")

autoplot(boots_tune)
## finalize workflow---------------
# boots_workflow %>% 
#   fit_resamples(
#     resamples = boots_folds,
#     control = control_resamples(save_pred = TRUE)
#   )

final_boots <- boots_workflow %>%
  finalize_workflow(select_best(boots_tune,metric = "rmse"))
final_boots

## last fit----------------------
my_df_fit <- last_fit(final_boots, my_df_split)
my_df_fit_v <- fit(final_boots, testing(my_df_split))


my_df_fit
#saveRDS(my_df_fit,here::here("artifacts","wkflw.RDS"))

collect_metrics(my_df_fit)
predict(
  my_df_fit$.workflow[[1]],
  slice_tail(testing(my_df_split),n=20))
predict(
  my_df_fit$.workflow[[1]],
  slice_head(testing(my_df_split),n=20))


## understand the model--------------------
my_flw <- my_df_fit %>% extract_workflow() 
final_fitted <- my_df_fit$.workflow[[1]]
predict(final_fitted, my_df[10:12, ])

library(rpart.plot)
rpart.plot(my_flw$fit)

library(vip)
my_df_fit %>% 
  extract_fit_parsnip() %>% 
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

library(DALEXtra)
final_fitted <- my_df_fit$.workflow[[1]]
predict(final_fitted, my_df[10:12, ])

boots_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(testing(my_df_split),
                       -credito),
  y = dplyr::select(training(my_df_split),
                    credito),
  verbose = FALSE
)

library(modelStudio)
new_observation <- testing(my_df_split) %>% slice_head()
modelStudio(boots_explainer, new_observation)
library(modelDown)
## save the model------------------
# library(vetiver)
# v <- my_df_fit %>%
#   extract_workflow() %>%
#   vetiver_model(model_name = "boots-v1")
# v
# library(pins)
# board <- board_temp(versioned = TRUE)
# board %>% vetiver_pin_write(v)
# vetiver_write_plumber(board, "credit-risk", 
#                       rsconnect = FALSE)
# vetiver_write_docker(v)
