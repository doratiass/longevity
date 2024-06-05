# BUSINESS SCIENCE R TIPS ----
# R-TIP 76 | XGBoost: My 2-Step Hyperparamter Tuning Process ----

# 👉 For Weekly R-Tips, Sign Up Here:
#    https://learn.business-science.io/r-tips-newsletter

# 👉 Do you want to become the data science expert for your organization?
#   HERE'S HOW: 10 SECRETS TO BECOMING A 6-FIGURE DATA SCIENTIST
#   https://learn.business-science.io/free-rtrack-masterclass

# 👉 5-Course R-Track Program (My Program to become a data scientist):
#  https://university.business-science.io/p/5-course-bundle-machine-learning-web-apps-time-series?el=newsletter

# LIBRARIES and DATA ----
library(tidymodels)
library(tidyverse)

# MODEL AND PREPORCESSOR SPEC ----
thresh_other <- 0.1
thresh_corr <- 0.9
ctl_grid <- control_resamples(save_pred = TRUE, 
                              save_workflow = TRUE, 
                              parallel_over = 'everything')

set.seed(1234)
df_split <- initial_split(ml_df, strata = outcome)
df_train <- training(df_split)
df_test <- testing(df_split)
df_train_cv <- vfold_cv(df_train, v = 5, strata = outcome)

xgb_spec_stage_1 <- boost_tree(
  mode   = "classification",
  engine = "xgboost",
  learn_rate = tune()
)

xgb_rec <- recipe(outcome ~ ., data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_date(all_date_predictors(), keep_original_cols = FALSE, features = "month") %>%
  step_integer(all_ordered_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = thresh_other, other = "other_combined") %>%
  step_dummy(all_nominal_predictors(), naming = new_sep_names) %>%
  step_zv(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = thresh_corr, method = "spearman")


# 2 STAGE HYPERPARAMETER TUNING PROCESS ----

# STAGE 1: TUNE LEARNING RATE ----

# * Define the stage 1 tuning grid ----
set.seed(123)
grid_stage_1 <- grid_random(
  learn_rate(range = c(-3, -0.5)),
  size = 100
)

# * Create a workflow ----
wflw_xgb_stage_1 <- workflow() %>%
  add_model(xgb_spec_stage_1) %>%
  add_recipe(xgb_rec)

# * Tune the model ----
set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

tune_stage_1 <- tune_grid(
  wflw_xgb_stage_1,
  resamples = df_train_cv,
  grid      = grid_stage_1,
  metrics   = metric_set(roc_auc),
  control   = ctl_grid
)

stopCluster(cl)

tune_stage_1 %>% collect_metrics() %>% arrange(-mean)

tune_stage_1 %>%
  autoplot()

# STAGE 2: HOLD LEARN RATE CONSTANT / TUNE OTHER PARAMS ----

# * Get Best Params Stage 1 ----
best_params_stage_1 <- tune_stage_1 %>%
  collect_metrics() %>%
  arrange(-mean) %>%
  slice(1)

# * Model Spec Stage 2: Hold LR constant ----
xgb_spec_stage_2 <- xgb_spec_stage_1 %>%
  set_args(
    learn_rate     = tune(),
    tree_depth     = tune(),
    loss_reduction = tune(),
    stop_iter      = tune(),
    trees = tune(),
    min_n = tune(),
    sample_size = tune(), 
    mtry = tune()
  )

wflw_xgb_stage_2 <- wflw_xgb_stage_1 %>%
  update_model(xgb_spec_stage_2)

# * Define Stage 2 grid ----
set.seed(123)
grid_stage_2 <- grid_random(
  tree_depth(),
  loss_reduction(),
  stop_iter(),
  learn_rate(),
  trees(),
  min_n(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  size = 100
)

# * Tune stage 2 -----
set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

tune_stage_2 <- tune_grid(
  wflw_xgb_stage_2,
  resamples = df_train_cv,
  grid      = grid_stage_2,
  metrics   = metric_set(roc_auc),
  control   = ctl_grid
)

stopCluster(cl)

tune_stage_2 %>% collect_metrics() %>% arrange(-mean)

tune_stage_2 %>%
  autoplot()

# FINAL MODEL ----

# * Select Best Params ----
best_params_stage_2 <- tune_stage_2 %>%
  collect_metrics() %>%
  arrange(-mean) %>%
  slice(1)

# * Update Best Parameters Args ----
xgb_spec_final <- xgb_spec_stage_2 %>%
  set_args(
    tree_depth     = best_params_stage_2$tree_depth,
    loss_reduction = best_params_stage_2$loss_reduction,
    stop_iter      = best_params_stage_2$stop_iter,
    learn_rate          = best_params_stage_2$learn_rate,
    trees          = best_params_stage_2$trees,
    min_n          = best_params_stage_2$min_n,
    sample_size    = best_params_stage_2$sample_size,
    mtry           = best_params_stage_2$mtry
  )

# * Fit the Final Model ----
wflw_final <- wflw_xgb_stage_2 %>%
  update_model(xgb_spec_final)

wflw_final_fit <- wflw_final %>%
  last_fit(df_split)

collect_metrics(wflw_final_fit)
