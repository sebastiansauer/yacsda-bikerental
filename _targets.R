library(targets)
library(tidymodels)
library(tidyverse)
library(tidy.outliers)  # from Github
#library(visdat)
#library(recipeselectors)  # not working
#library(tictoc)
#library(ggstatsplot)
#library(DataExplorer)
#library(easystats)
#library(vip)  # variable importance
library(crew)  # use multiple cores


# source funs:
source("R/funs-div.R")
source("R/recipes.R")


tar_option_set(
  controller = crew_controller_local(workers = 1)
)




# Define the pipeline:
list(
  

# setup -------------------------------------------------------------------

  tar_target(path, read_yaml("path.yml"), packages = "yaml"),
  
  # import the train and test data:
  tar_target(d_train_raw, read_csv(path$d_train)),
  tar_target(d_test_raw, read_csv(path$d_test)),
  tar_target(d_train, add_id(d_train_raw)),
  tar_target(d_test, add_id(d_test_raw)),
  

# wf1: rf ---------------------------------------------------------------------
  tar_target(rec1, def_recipe1(d_train)),
  tar_target(wf1_name, "Random Forest with no tuning, typical preproc"),
  tar_target(d_train_baked, bake(prep(rec1), new_data = NULL)),
  tar_target(mod_rf, rand_forest(mode = "regression",
                                 mtry = 3,
                                 trees = 2000) %>%  
               set_engine("ranger",
                          importance = "permutation")),
  tar_target(wf1, workflow() %>% 
               add_recipe(rec1) %>% 
               add_model(mod_rf)),
  tar_target(wf1_fitted, fit(wf1, d_train)),
  tar_target(wf1_importance, wf1_fitted %>% 
               extract_fit_parsnip() %>% vip(), 
             packages = "vip"),
  tar_target(wf1_pred, d_test %>% 
               mutate(pred_wf1 = predict(wf1_fitted, new_data = d_test)[[".pred"]])),
  tar_target(wf1_preds_corrected, 
             correct_for_no_func(wf1_pred, pred_wf1)),
  tar_target(wf1_performance,  metrics(wf1_preds_corrected,
                                       truth = count,
                                       estimate = pred_wf1)),
  
  

# wf2 xgb logged outcome ---------------------------------------------------------------------
  tar_target(d_train_logged, 
             d_train %>% 
               mutate(count_log = log(count)) %>% 
               select(-count) %>%  # don't forget to remove "count"
               mutate(count_log = ifelse(count_log == -Inf, 0, count_log))),
  # we need to log the outcome var in the test data as well:
  tar_target(d_test_logged,
             d_test %>% 
               mutate(count_log = log(count)) %>% 
               select(-count) %>% 
               mutate(count_log = ifelse(count_log == -Inf, 0, count_log))),
  tar_target(wf2_name, "XGB with trees and learn rate tuned, typical preproc, y logged"),
  tar_target(rec2, def_recipe2(d_train_logged)),
  tar_target(d_train_logged_baked, bake(prep(rec2), new_data = NULL)),
  tar_target(mod_boost, 
             boost_tree(mode = "regression",
                        mtry = 3,
                        trees = tune(),
                        learn_rate = tune()) %>%  
               set_engine(engine = "xgboost")),
  tar_target(rsmpl, 
             vfold_cv(d_train_logged, v = 5, strata = count_log)),
  tar_target(wf2, workflow() %>% 
               add_recipe(rec2) %>% 
               add_model(mod_boost)),
  tar_target(wf2_fitted, tune_grid(wf2, resamples = rsmpl)),
  tar_target(wf2_best, wf2_fitted %>% select_best()),
  tar_target(wf2_fitted_best, wf2 %>% 
               finalize_workflow(wf2_best) %>% 
               fit(d_train_logged)),
  tar_target(wf2_importance, 
             wf2_fitted_best %>% 
               extract_fit_parsnip() %>% 
               vip(), packages = c("vip", "tidymodels")),
  tar_target(wf2_pred, 
             d_test_logged %>% 
               mutate(pred_wf2 = predict(wf2_fitted_best, 
                                         new_data = d_test_logged)[[".pred"]])),
  tar_target(wf2_preds_corrected, correct_for_no_func(wf2_pred, pred_wf2) %>% 
               mutate(pred_wf2_delog = exp(pred_wf2),
                      count_delog = exp(count_log))),  # delogging needed
  tar_target(wf2_performance,  metrics(wf2_preds_corrected,
                                       truth = count_delog,
                                       estimate = pred_wf2_delog)),
  
  
  

# nullmodel ---------------------------------------------------------------


  tar_target(null_pred, d_test %>% mutate(null_pred = 700)),  # predict mean value
  tar_target(wf_null_names, "Null model: predicting the mean value (700)"),
  tar_target(null_performance, metrics(null_pred,
                                       truth = count,
                                       estimate = null_pred)),



# wf3 : lm, full rec  ------------------------------------------------------------------
  # Preparation: start with dummyfied predictors:
  tar_target(rec3a,
             recipe(count ~ ., data = d_train) %>%
               step_dummy(all_nominal_predictors(), -date)),
  tar_target(wf3_name, "Linear model, preselected preds, only numeric preds (no dummyfying needed), typical preproc"),
  tar_target(d_train_dummy,
             prep(rec3a) %>% bake(new_data = NULL)),
  tar_target(d_test_wf3,
             prep(rec3a) %>% bake(new_data = d_test)),
  tar_target(rec3, def_recipe3(d_train_dummy)),
  tar_target(d_train3_baked, bake(prep(rec3), new_data = NULL)),
  tar_target(mod_lm, linear_reg()),
  tar_target(wf3,
             workflow() %>%
               add_recipe(rec3) %>%
               add_model(mod_lm)),
  tar_target(wf3_fitted, wf3 %>% fit(d_train_dummy)),
  tar_target(wf3_importance,
             wf3_fitted %>%
               extract_fit_parsnip() %>%
               vip(), packages = "vip"
             ),
  tar_target(wf3_pred,
             d_test_wf3 %>%
               mutate(pred_wf3 = predict(wf3_fitted, new_data = d_test_wf3)[[".pred"]])
             ),
  tar_target(wf3_preds_corrected,
             wf3_pred %>%
               mutate(func_Yes := case_when(
                 func_Yes == "No" ~ 0,
                 TRUE ~ func_Yes)) %>%
               select(id,
                      contains("count"),
                      contains("pred"),
                      contains("func"),
                      everything())
             ),
  tar_target(wf3_performance,  metrics(wf3_preds_corrected,
                                       truth = count,
                                       estimate = pred_wf3)
             ),



# wf4 : lm, simple recipe ------------------------------------------------------------------
  
tar_target(rec4,
             recipe(count ~ ., data = d_train) %>%
               update_role(id, new_role = "id")),
  tar_target(wf4_name, "Linear model, simple recipe, no preproc"),
  tar_target(d_train_wf4,
             prep(rec4) %>% bake(new_data = NULL)),
  tar_target(wf4,
             workflow() %>%
               add_recipe(rec4) %>%
               add_model(mod_lm)),
  tar_target(wf4_fitted,
             wf4 %>% fit(d_train_wf4)),
  tar_target(wf4_importance,
             wf4_fitted  %>%
               extract_fit_parsnip() %>%
               vip(), packages = "vip"),
  tar_target(wf4_pred, d_test %>%
               mutate(pred_wf4 = predict(wf4_fitted, new_data = d_test)[[".pred"]])
             ),
  tar_target(wf4_performance,  metrics(wf4_pred,
                                       truth = count,
                                       estimate = pred_wf4)
             ),



# wf5: xgb, dummyfied vars ------------------------------------------------------------------

  tar_target(rec5, def_recipe3(d_train_baked)),
  tar_target(wf5_name, "XGB with trees and learn rate tuend,  preselected preds, only numeric preds (no dummyfying needed), typical preproc"),
  tar_target(d_train5_baked, bake(prep(rec5), new_data = NULL)),
  tar_target(mod_boost2, boost_tree(mode = "regression",
                                   mtry = 2,
                                   trees = tune(),
                                   learn_rate = tune()) %>%
               set_engine(engine = "xgboost")),
  tar_target(rsmpl2, vfold_cv(d_train, v = 5, strata = count)),
  tar_target(wf5, workflow() %>%
               add_recipe(rec5) %>%
               add_model(mod_boost2)),
  tar_target(wf5_fitted, tune_race_anova(wf5, resamples = rsmpl2, grid = 50), packages = "finetune"),
  tar_target(wf5_best, wf5_fitted %>% select_best()),
  tar_target(wf5_fitted_best,
             wf5 %>%
               finalize_workflow(wf5_best) %>%
               fit(d_train5_baked)),
  tar_target(wf5_importance,
             wf5_fitted_best %>%
               extract_fit_parsnip() %>%
               vip(),
             packages = c("vip", "tidymodels")),
  tar_target(wf5_pred, d_test_wf3 %>%
               mutate(pred_wf5 = predict(wf5_fitted_best, new_data = d_test_wf3)[[".pred"]])),
  tar_target(wf5_preds_corrected, wf5_pred %>%
               mutate(func_Yes := case_when(
                 func_Yes == "No" ~ 0,
                 TRUE ~ func_Yes)) %>%
               select(id, contains("count"), contains("pred"), contains("func"), everything())),
  tar_target(wf5_performance,  metrics(wf5_preds_corrected,
                                       truth = count,
                                       estimate = pred_wf5)),






# wf6: glmnet ----------------------------------------------------
  # Preparation: start with dummyfied predictors:
  tar_target(rec6, def_recipe5(d_train_dummy)),
  tar_target(wf6_name, "glmnet with penalty and mixture tuned, all preds, typical preproc"),
  tar_target(d_train6_baked, bake(prep(rec6), new_data = NULL)),
  tar_target(mod_glmnet, linear_reg(engine = "glmnet",
                                    penalty = tune(),
                                    mixture = tune())),
  tar_target(wf6, 
             workflow() %>%
               add_recipe(rec6) %>%
               add_model(mod_glmnet)),
  # tar_target(rsmpl3, vfold_cv(d_train_dummy %>% select(-date),
  #                             v = 5, strata = count)),

  tar_target(wf6_fitted, 
             tune_race_anova(wf6,
                             resamples = rsmpl2,
                             grid = 50,
                             control = control_race(allow_par = TRUE,
                                                    pkgs = c("finetune"))),
             packages = "finetune"),
  tar_target(wf6_best, wf6_fitted %>% select_best()),
  tar_target(wf6_fitted_best, wf6 %>%
               finalize_workflow(wf6_best) %>%
               fit(d_train6_baked)),
  # tar_target(wf6_importance,
  #            wf6_fitted_best %>%
  #              extract_fit_parsnip() %>%
  #              vip(),
  #            packages = c("vip", "tidymodels")),
  tar_target(wf6_pred, d_test_wf3 %>%
               mutate(pred_wf6 = predict(wf6_fitted_best, new_data = d_test_wf3)[[".pred"]])),
  tar_target(wf6_preds_corrected,
             wf6_pred %>%
               mutate(func_Yes := case_when(
                 func_Yes == "No" ~ 0,
                 TRUE ~ func_Yes)) %>%
               select(id, contains("count"), contains("pred"), contains("func"),
                      everything())),
  tar_target(wf6_performance,  metrics(wf6_preds_corrected,
                                       truth = count,
                                       estimate = pred_wf6))







# # wf7 : lm, logged outcome, full rec, dummyfied preds ------------------------------------------------------------------
# 
#   tar_target(rec_log_dummy,
#            recipe(count_log ~ ., data = d_train_logged) %>%
#              step_dummy(all_nominal_predictors(), -date)),
# 
#   tar_target(wf7_name, "Linear mod, logged outcome, full rec, dummyfied preds"),
# 
# tar_target(d_train_wf7,
#            prep(rec_log_dummy) %>% bake(new_data = NULL)),
# 
# tar_target(d_test_wf7,
#            prep(rec_log_dummy) %>% bake(new_data = d_test_logged)),
# 
# tar_target(rec7, def_recipe2(d_train_wf7)),
# 
# tar_target(d_train7_baked, bake(prep(rec7), new_data = NULL)),
# 
# tar_target(wf7,
#            workflow() %>%
#              add_recipe(rec7) %>%
#              add_model(mod_lm)),
# 
# tar_target(wf7_fitted, wf7 %>% fit(d_train_wf7)),
# tar_target(wf7_importance,
#            wf7_fitted %>%
#              extract_fit_parsnip() %>%
#              vip(), packages = c("vip", "tidymodels")),
# 
# tar_target(wf7_pred,
#            d_test_wf7 %>%
#              mutate(pred_wf7 = predict(wf7_fitted, new_data = d_test_wf7)[[".pred"]])),
# 
# tar_target(wf7_preds_corrected,
#            wf7_pred %>%
#              select(id,
#                     contains("count"),
#                     contains("pred"),
#                     contains("func"),
#                     everything()
#                     ) %>%
#              mutate(pred_wf7_delog = exp(pred_wf7),
#                     count_delog = exp(count_log))),  # delogging needed
# 
# tar_target(wf7_performance,  metrics(wf7_preds_corrected,
#                                      truth = count_delog,
#                                      estimate = pred_wf7_delog)
#            ),
# 
# 
# # compare results of wfs --------------------------------------------------
# tar_target(wf_results_list,
#            list(
#              null = null_performance,
#              wf1 = wf1_performance,
#              wf2 = wf2_performance,
#              wf3 = wf3_performance,
#              wf4 = wf4_performance,
#              wf5 = wf5_performance,
#              wf6 = wf6_performance,
#              wf7 = wf7_performance)
#            ),
# # 
# tar_target(wf_names,
#            list(null = wf_null_names,
#                 wf1_names = wf1_names,
#                 wf2_names = wf2_names,
#                 wf3_names = wf3_names,
#                 wf4_names = wf4_names,
#                 wf5_names = wf5_names,
#                 wf6_names = wf6_names,
#                 wf7_names = wf7_names)),
# 
# tar_target(wf_ids, names(wf_results_list)),
# tar_target(wf_results_df, 
#            wf_results_list %>%
#              map2_df(.y = wf_ids, ~ bind_cols(id = .y, .x)) %>% 
#              mutate(description = map_chr(wf_names, `[`))),
# tar_target(wf_results_rmse, wf_results_df %>% filter(.metric == "rmse")),
# tar_target(results_plot, plot_results(wf_results_df),
#            packages = c("ggstatsplot", "dplyr")
#            )
# 
# 
# 
#   
)


# < 300 (Rmse) gibt's die 1, dann jweils 100 dazu pro Note
