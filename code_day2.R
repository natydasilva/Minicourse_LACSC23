
library(tidyverse)
library(tidymodels)
library(here)
#you can use  path read_csv("yourpaht/apt_redu.csv")
data <- read_csv(here("apt_redu.csv"))

#Past class, classification problem

#Transform the response in a two class vble
new_data <- data %>%
  mutate(lpreciom2_c = as.factor(
    case_when(lpreciom2< mean(lpreciom2)~'low',
              lpreciom2 >= mean(lpreciom2) ~ 'high'))) %>%
  select(-lpreciom2,-long)
#Split the data

set.seed(2023)
data_split_c <- initial_split(new_data)

data_train_c <- training(data_split_c)
data_test_c <- testing(data_split_c)

#parsnip pkg 
# Pick a model,  Set the engine and set mod
tree_mod_cl<- decision_tree() %>%
  set_engine("rpart") %>% 
  set_mode("classification")

## it the model with the training data set
tree_fit_cl <- tree_mod_cl %>%
  fit(lpreciom2_c ~ ., data = data_train_c)

# Get the performance in the training set
augment(tree_fit_cl, new_data = data_test_c ) %>%
  accuracy(truth = lpreciom2_c , estimate = .pred_class)


# Get the performance in the training set
augment(tree_fit_cl, new_data = data_test_c ) %>%
  conf_mat(truth =lpreciom2_c , estimate = .pred_class)

augment(tree_fit_cl, new_data = data_test_c) %>%
  roc_curve(truth = lpreciom2_c, .pred_high) %>%
  autoplot()


augment(tree_fit_cl, new_data = data_test_c ) %>%
  roc_auc(truth = lpreciom2_c, .pred_high)


tree_cl_wf <-
  workflow() %>%
  add_model(spec = tree_mod_cl) %>%
  add_formula(lpreciom2_c ~ .)

tree_cl_wf


set.seed(2023)
folds <- vfold_cv(data_train_c, v = 10)
folds

tree_fit_rs <-
  tree_cl_wf %>% #classificcation tree workflow
  fit_resamples(folds) # compute metrics in across folds

tree_fit_rs 

collect_metrics(tree_fit_rs )

tree_cl_tunewf <-
  workflow() %>%
  add_model(spec = tree_mod_cl %>% 
              set_args(cost_complexity = tune())) %>%
  add_formula(lpreciom2_c ~ .)

param_grid <- grid_regular(cost_complexity(range = c(-6, -1)), 
                           levels = 10)

tune_res <- tune_grid(
  tree_cl_tunewf,
  resamples = folds,
  grid = param_grid,
  metrics = metric_set(accuracy))

autoplot(tune_res)


best_cp <- select_best(tune_res)

tree_cl_final <- finalize_workflow(tree_cl_tunewf, best_cp)


#takes a data split as an input, uses the split to generate 
#the training and test sets for the final fitting and evaluation.

tree_cl_final_fit <-  last_fit(tree_cl_final,data_split_c )


fitted_wflow <- extract_workflow(tree_cl_final_fit)

collect_metrics(tree_cl_final_fit)%>% slice(1:5)


rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_fit <- fit(rf_spec, lpreciom2_c ~ ., data = data_train_c)
rf_fit



augment(rf_fit, new_data = data_train_c) %>% 
  accuracy(truth = lpreciom2_c, estimate = .pred_class)

augment(rf_fit, new_data = data_test_c) %>% 
  accuracy(truth = lpreciom2_c, estimate = .pred_class)

augment(rf_fit, new_data = data_test_c) %>%
  roc_auc(truth = lpreciom2_c, .pred_high)

augment(rf_fit, new_data = data_test_c) %>%
  roc_curve(truth = lpreciom2_c, .pred_high) %>%
  autoplot()

library(vip)
vip(rf_fit)

library(baguette)

apt_folds <- vfold_cv(data_train_c, repeats = 5, v = 5) # 5-fold-CV-5reps
#1) Set models (CART, Bagging and Random Forest)

cart_apt <-
  decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

bag_cart_apt <- bag_tree(cost_complexity = tune(),
                         class_cost = tune() ) %>%
  set_mode("classification")

rf_apt <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")


#define for all the models variables to be use
model_vars <- 
  workflow_variables(outcomes = lpreciom2_c , 
                     predictors = everything())

#Generate a set of workflow objects from preprocessing
#and model objects
wkf <- 
  workflow_set(
    preproc = list(simple = model_vars), #A list with preproc.obj 
    models = list(CART = cart_apt, bagging = bag_cart_apt, RF = rf_apt)
  )

wkf

DONT RUN THIS CODE NOW\\

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

#will execute the same function across the workflows in the set
grid_results <-
  wkf %>%
  workflow_map( 
    seed = 1503,
    resamples = apt_folds,
    grid = 10,
    control = grid_ctrl
  )

grid_results

grid_results %>% 
  rank_results( ) %>%  # every model 
  filter(.metric == "roc_auc") %>% 
  select(model, .config, rmse = mean, rank)

autoplot(
  grid_results,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
) 


autoplot(
  grid_results,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
) 

rf_model <- 
  grid_results %>% 
  extract_workflow_set_result("simple_RF") %>% #ID in wkf obj
  select_best(metric = "roc_auc")

rf_test_results <- 
  grid_results %>% 
  extract_workflow("simple_RF") %>% 
  finalize_workflow(rf_model) %>% #update tuning parameters in workflow
  last_fit(split = data_split_c) #fit with training and evaluate test

collect_metrics( rf_test_results )