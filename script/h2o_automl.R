# h2o_automl
#https://www.youtube.com/watch?v=Qd6Re5_I-Ps&ab_channel=R-LadiesGlobal
# https://agua.tidymodels.org/
library(tidyverse)
library(tidymodels)
library(agua)
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
k_fold <- vfold_cv(training(my_df_split), v = 5)
## recipe-------------------
lm_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.9) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.03) %>%
  step_dummy(all_nominal_predictors(),one_hot = TRUE)

lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% glimpse()

lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()

#lm_recipe %>% prep() %>% bake(my_df_split) %>% View()

##spec-------------------
h2o_start()
lm_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_mode("regression") %>%
  set_engine("h2o")

##workflow----------------------
lm_workflow <-
  workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec)
lm_workflow
## tune grid------------------
set.seed(7785)

lm_grid <- grid_latin_hypercube(
  mixture(range = c(0, 1), trans = NULL),
  penalty(range = c(-10, 0), trans = log10_trans()), 
  size = 30)

lm_grid
set.seed(7785)
doParallel::registerDoParallel()

lm_tune <-
  tune_grid(lm_workflow,
            k_fold,
            grid = lm_grid,
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
lm_tune
### show best ------------------------
show_best(lm_tune,metric = "rmse")
show_best(lm_tune,metric = "rsq")
show_best(lm_tune,metric = "ccc")
show_best(lm_tune,metric = "huber_loss")
show_best(lm_tune,metric = "icc")
show_best(lm_tune,metric = "ccc")
show_best(lm_tune,metric = "rpiq")
show_best(lm_tune,metric = "rpd")

autoplot(lm_tune)
## finalize workflow---------------
final_lm <- lm_workflow %>%
  finalize_workflow(select_best(lm_tune,metric = "huber_loss"))
final_lm

## last fit----------------------
my_df_fit <- last_fit(final_lm, my_df_split)
my_df_fit_v <- fit(final_lm, testing(my_df_split))


my_df_fit
saveRDS(my_df_fit,here::here("artifacts","wkflw.RDS"))

collect_metrics(my_df_fit)
predict(
  my_df_fit$.workflow[[1]],
  slice_tail(testing(my_df_split),n=20))
predict(
  my_df_fit$.workflow[[1]],
  slice_head(testing(my_df_split),n=20))

pred_df_complete <- 
  predict(
    my_df_fit$.workflow[[1]],
    my_df_complete
  )
pred_df_complete <- 
  cbind(my_df_complete$cod_ibge,
        my_df_complete$uf,
        pred_df_complete,
        my_df_complete$credito) 
colnames(pred_df_complete) <-  c("cod_ibge","uf","previsao","credito")
# pred_df_complete %>% 
#   saveRDS(here::here("artifacts","pred_df_complete.RDS"))
# 
# pred_df_cred_0 <- 
#   predict(
#     my_df_fit$.workflow[[1]],
#     my_df_complete %>% 
#       filter(credito == 0) 
#   )
# pred_df_cred_0 %>% 
#   saveRDS(here::here("artifacts","pred_df_cred_0.RDS"))
# pred_df_cred_0 %>% sum()

## understand the model--------------------
my_flw <- my_df_fit %>% extract_workflow() 
final_fitted <- my_df_fit$.workflow[[1]]
predict(final_fitted, my_df[10:12, ])

library(rpart.plot)
rpart.plot(my_flw$fit)
rpart.plot(my_df_fit$.workflow)

library(vip)
my_df_fit %>% 
  extract_fit_parsnip() %>% 
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

library(DALEXtra)
final_fitted <- my_df_fit$.workflow[[1]]
predict(final_fitted, my_df[10:12, ])

lm_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(testing(my_df_split),
                       -credito),
  y = dplyr::select(training(my_df_split),
                    credito),
  verbose = FALSE
)

library(modelStudio)
new_observation <- testing(my_df_split) %>% slice_head()
modelStudio(lm_explainer, new_observation)
library(modelDown)
## save the model------------------
# library(vetiver)
# v <- my_df_fit %>%
#   extract_workflow() %>%
#   vetiver_model(model_name = "lm-v1")
# v
# library(pins)
# board <- board_temp(versioned = TRUE)
# board %>% vetiver_pin_write(v)
# vetiver_write_plumber(board, "credit-risk", 
#                       rsconnect = FALSE)
# vetiver_write_docker(v)
