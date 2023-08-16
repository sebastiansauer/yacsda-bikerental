
# recipe1 -----------------------------------------------------------------

# all predictors, no dummyfying, typical preprocssing

def_recipe1 <- function(d_train = d_train){
  rec <-
    recipe(count ~ ., data = d_train) %>% 
    
    # change roles:
    update_role(id, new_role = "id") %>% 
    
    # parse dates:
    step_mutate(date = dmy(date)) %>% 
    step_date(date, keep_original_cols = FALSE) %>% 
    step_mutate(year = factor(date_year)) %>% 
    
    # rm outliers:
    step_outliers_maha(all_numeric(), -all_outcomes()) |>
    step_outliers_remove(contains(".outliers")) %>% 
    
    # rm vars:
    step_corr(all_numeric_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    
    # standardize vars:
    step_normalize(all_numeric_predictors(), -hour, -date_year, -temp)
  
}



# recipe2 -----------------------------------------------------------------

# logged outcome, all preds, typical preproc, no dummyfying


def_recipe2 <- function(d_train = d_train) {
  
  # rm "count", as we use here the logged count: 
  if ("count" %in% names(d_train)) d_train$count <- NULL
  
  rec <-
    recipe(count_log ~ ., data = d_train) %>% 
    
    # change roles:
    update_role(id, new_role = "id") %>% 
    
    # parse dates:
    step_mutate(date = dmy(date)) %>% 
    step_date(date, keep_original_cols = FALSE) %>% 
    step_mutate(year = factor(date_year)) %>% 
    
    # dummify:
    step_dummy(all_nominal_predictors()) %>% 
    
    # step na omit:
    step_naomit(all_predictors()) %>% 
    
    # rm vars:
    step_corr(all_numeric_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    
    # rm outliers:
    step_outliers_lookout(all_numeric(),-contains(".outliers "),  # computationally expensive
                          -all_outcomes()) |> 
    step_outliers_remove(contains(".outliers")) %>% 
    
    # standardize vars:
    step_normalize(all_numeric_predictors(),  -contains("year"))
}





# recipe 3 ----------------------------------------------------------------

# preselected preds, only numeric preds (no dummyfying needed), typical preproc


def_recipe3 <- function(d_train){
  rec <-
    recipe(
      count ~ id + hour + temp + humidity + windspeed + humidity + season_Winter, 
           data = d_train) %>% 
    
    # change roles:
    update_role(id, new_role = "id") %>% 
    
    # rm outliers:
    step_outliers_maha(all_numeric(), -all_outcomes()) |>
    step_outliers_remove(contains(".outliers")) %>% 
    
    # step na omit:
    step_naomit(all_predictors()) %>% 
    
    # rm vars:
    step_corr(all_numeric_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    
    # standardize vars:
    step_normalize(all_numeric_predictors(), -hour, -temp)

}



# recipe4 -----------------------------------------------------------------


# preselected predictors, typacal preprocessing, only numeric cols, no windspeed as pred, very similar to recipe 3

def_recipe4 <- function(d) {
  
  rec <-
    recipe(count ~ id + hour + temp + humidity + season_Summer + season_Winter, 
           data = d) %>% 
    
    # change roles:
    update_role(id, new_role = "id") %>% 

    # step na omit:
    step_naomit(all_predictors()) %>% 
    
    # rm vars:
    step_corr(all_numeric_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    
    # rm outliers:
    step_outliers_lookout(all_numeric(),-contains(".outliers "),  # computationally expensive
                          -all_outcomes()) |> 
    step_outliers_remove(contains(".outliers")) %>% 
    
    # standardize vars:
    step_normalize(all_numeric_predictors())
}



# recipe 5 ----------------------------------------------------------------



def_recipe5 <- function(d_train = d_train) {
  
  rec <-
    recipe(count ~ ., data = d_train) %>% 
    
    # change roles:
    update_role(id, new_role = "id") %>% 
    
    # parse dates:
    step_mutate(date = dmy(date)) %>% 
    step_date(date, keep_original_cols = FALSE) %>% 
    step_mutate(year = factor(date_year)) %>% 
    
    # dummify:
    step_dummy(all_nominal_predictors()) %>% 
    
    # step na omit:
    step_naomit(all_predictors()) %>% 
    
    # rm vars:
    step_corr(all_numeric_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    
    # rm outliers:
    step_outliers_lookout(all_numeric(),-contains(".outliers "),  # computationally expensive
                          -all_outcomes()) |> 
    step_outliers_remove(contains(".outliers")) %>% 
    
    # standardize vars:
    step_normalize(all_numeric_predictors(),  -contains("year"))
}



