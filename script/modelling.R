library(tidyverse)
library(tidymodels)
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


### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.75)
## in case you want use bootstrap bootstrap
k_fold <- vfold_cv(training(my_df_split), v = 5)
## recipe-------------------
rf_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.8) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.018) %>%
  step_dummy(all_nominal_predictors(),one_hot = FALSE) 

rf_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()
##spec-------------------
rf_spec <-
  rand_forest(mtry = tune(),
              trees = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")
rf_spec %>% extract_parameter_set_dials()

##workflow----------------------
rf_workflow <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)
rf_workflow

## tune grid------------------
rf_grid <- grid_latin_hypercube(
  mtry(c(14,26)),
  trees(c(80,300)),
  min_n(c(12,30)),
  size = 30)
rf_grid
set.seed(7785)
doParallel::registerDoParallel()

rf_tune <-
  tune_grid(rf_workflow,
            k_fold,
            grid = rf_grid,
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
rf_tune
### show best ------------------------
show_best(rf_tune,metric = "rmse")
show_best(rf_tune,metric = "rsq")
show_best(rf_tune,metric = "ccc")
show_best(rf_tune,metric = "huber_loss")
show_best(rf_tune,metric = "icc")
show_best(rf_tune,metric = "ccc")
show_best(rf_tune,metric = "rpiq")
show_best(rf_tune,metric = "rpd")


autoplot(rf_tune)
## finalize workflow---------------
final_rf <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune,metric = "rmse"))
final_rf

## last fit----------------------
my_df_fit <- last_fit(final_rf, my_df_split)
my_df_fit_v <- fit(final_rf, testing(my_df_split))


my_df_fit
#saveRDS(my_df_fit,here::here("artifacts","wkflw-rf.RDS"))

collect_metrics(my_df_fit)
predict(
  my_df_fit$.workflow[[1]],
  slice_tail(testing(my_df_split),n=20))
predict(
  my_df_fit$.workflow[[1]],
  slice_head(testing(my_df_split),n=20))


## understand the model--------------------

library(vip)
my_df_fit %>% 
  extract_fit_parsnip() %>% 
  vip(20)
# imp_spec <- rf_spec %>%
#   finalize_model(select_best(rf_tune)) %>%
#   set_engine("ranger", importance = "permutation")
# 
# vip_obj <- 
# workflow() %>%
#   add_recipe(rf_recipe) %>%
#   add_model(imp_spec) %>%
#   fit(training(my_df_split)) %>%
#   extract_fit_parsnip() 
# vip_obj %>% 
#   vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

library(DALEXtra)
final_fitted <- my_df_fit %>% extract_workflow() 
predict(final_fitted, my_df[10:12, ])

rf_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(testing(my_df_split),
                       -credito),
  y = dplyr::select(training(my_df_split),
                    credito),
  verbose = FALSE
)
rf_explainer
pdp_time <- model_profile(
  rf_explainer,
  variables = "loan_amnt",
  N = NULL,
  groups = "term"
)


as_tibble(pdp_time$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Time to complete track",
    y = "Predicted probability of shortcut",
    color = NULL,
    title = "Partial dependence plot for Mario Kart world records",
    subtitle = "Predictions from a decision rf model"
  )

# 
# 
# ## save the model------------------
# library(vetiver)
# v <- my_df_fit %>%
#   extract_workflow() %>%
#   vetiver_model(model_name = "rf-v1")
# v
# library(pins)
# board <- board_temp(versioned = TRUE)
# board %>% vetiver_pin_write(v)
# vetiver_write_plumber(board, "credit-risk-rf", rsconnect = FALSE)
# vetiver_write_docker(v)
# 