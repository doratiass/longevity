#tutorial 
# https://juliasilge.com/blog/lasso-the-office/
# https://www.tidymodels.org/learn/models/calibration/
#gitcreds::gitcreds_set()
# packages --------------------------------------------------------------------
library(tidyverse)
library(MASS)
library(vip)
library(SHAPforxgboost)
library(tidymodels)
library(pdp)
library(gridExtra)
library(ggpubr)
library(parallel)
library(doParallel)
library(klaR)
library(discrim)
library(probably)
library(foreach)
library(stacks)
library(shapviz)
tidymodels_prefer()

set.seed(45)

# defs ------------------------------------------------------------------------
thresh_other <- 0.1
thresh_corr <- 0.9
ctl_grid <- control_resamples(save_pred = TRUE, 
                              save_workflow = TRUE, 
                              parallel_over = 'everything')
# build df --------------------------------------------------------------------
## add outcome to data --------------------------------------------------------
load("raw_data/final_df.RData")

df_w_miss <- final_df %>%
  select(starts_with(c("id_nivdaki", "outcome_age_lfu",
                       "dmg_","med_", "lab_","physical_",
                       "work_","family_","diet_", "comp_"))) %>%
  mutate(
    outcome = factor(ifelse(outcome_age_lfu >= 95, "centenarian", "not_centenarian"),
                     levels = c("centenarian","not_centenarian")),
    dmg_immigration_year = ifelse(dmg_immigration_year == -2, NA, dmg_immigration_year)
  ) %>%
  filter(!is.na(outcome)) %>%
  select(-starts_with(c("death_age_died_2006", "dmg_birth_date",
                        "dmg_birth_year_1", "dmg_birth_year_2",
                        "dmg_birth_year", "dmg_kibbutz","med_ecg_68",
                        "diet_data_useable", "outcome_age_lfu"))) %>%
  mutate_if(is.logical, as.factor)

## remove missing > 10 % ------------------------------------------------------
miss_10_vars <- tibble(
  name = colnames(df_w_miss),
  NAs = sapply(df_w_miss, function(x) sum(is.na(x))),
  p = round(NAs/nrow(df_w_miss)*100,2)
) %>%
  filter(p > 15) %>%
  pull(name)

ml_df <- df_w_miss %>%
  select(-one_of(miss_10_vars))

## create ML data & general variables ------------------------------------------
set.seed(1234)
df_split <- initial_split(ml_df, strata = outcome)
df_train <- training(df_split)
df_test <- testing(df_split)
df_train_cv <- vfold_cv(df_train, v = 10, strata = outcome)

# logistic regression ----------------------------------------------------------
## prepare the data -----------------------------------------------------------
log_rec_step <- recipe(outcome ~ ., data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_zv(all_numeric_predictors()) %>%
  step_poly(all_numeric_predictors(), degree = 3) %>% #-c(med_diabetes_new,med_COPD)
  step_date(all_date_predictors(), keep_original_cols = FALSE, features = "month") %>%
  step_other(all_nominal_predictors(), threshold = thresh_other, other = "other_combined") %>%
  step_corr(all_numeric_predictors(), threshold = thresh_corr)

log_prep_step <- log_rec_step %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

log_bake <- bake(log_prep_step, df_train)

## stepwise AIC ---------------------------------------------------------------
nullModel <- glm(outcome ~ 1, data = log_bake, family = binomial)
fullModel <- glm(outcome ~ ., data = log_bake, family = binomial)

step_model <- stepAIC(nullModel, # start with a model containing no variables
                      direction = 'forward', # run forward selection
                      scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                                   lower = nullModel), # the minimum to consider is a model with no variables
                      trace = 1)


summary(step_model)

step_model_formula <- reformulate(str_remove(str_remove(strsplit(as.character(step_model$formula)[[3]], " + ", fixed = TRUE)[[1]], 
                                                        regex("_poly_[1-3]")), "\n    "),"outcome")
## fit model ------------------------------------------------------------------
log_rec <- recipe(update(step_model_formula, ~ . + id_nivdaki), data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_poly(all_numeric_predictors(), degree = 3) %>%
  step_date(all_date_predictors(), keep_original_cols = FALSE, features = "month")%>%
  step_zv(all_numeric_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = thresh_other, other = "other_combined") %>%
  step_dummy(all_nominal_predictors())

log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode('classification')

log_wf <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec)

final_log_fit <- log_wf %>%
  last_fit(df_split)

collect_metrics(final_log_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

log_train_fit <- fit_resamples(log_wf,
                               resamples = df_train_cv,
                               metrics = metric_set(roc_auc, accuracy, sens,spec),
                               control = ctl_grid)

stopCluster(cl)

# LASSO imputed model ----------------------------------------------------------
## prepare the data -----------------------------------------------------------
lasso_rec <- recipe(outcome ~ ., data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_poly(all_numeric_predictors(),degree = 3) %>% #-c(med_diabetes_new,med_COPD), 
  step_date(all_date_predictors(), keep_original_cols = FALSE, features = "month")%>%
  step_other(all_nominal_predictors(), threshold = thresh_other, other = "other_combined") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = thresh_corr, method = "spearman") %>%
  step_normalize(all_numeric_predictors())

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode('classification')

lambda_grid <- grid_regular(penalty(), levels = 1000)

## setup the model & tune hyperparameters --------------------------------------
lasso_wf <- workflow() %>%
  add_recipe(lasso_rec) %>% 
  add_model(lasso_spec)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

lasso_res <- tune_grid(
  lasso_wf,
  resamples = df_train_cv,
  grid = lambda_grid,
  control = ctl_grid
)

stopCluster(cl)

lasso_res %>%
  autoplot()

## select best model -----------------------------------------------------------
lasso_res %>%
  show_best("roc_auc")

lasso_best_auc <- lasso_res %>%
  select_best("roc_auc")

final_lasso <- finalize_workflow(
  lasso_wf,
  lasso_best_auc
)

final_lasso_fit <- final_lasso %>%
  last_fit(df_split)

collect_metrics(final_lasso_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

lasso_train_fit <- fit_resamples(final_lasso,
                                 resamples = df_train_cv,
                                 metrics = metric_set(roc_auc, accuracy, sens,spec),
                                 control = ctl_grid)

stopCluster(cl)

# XGBoost imputed model -------------------------------------------------------
## prepare the data -----------------------------------------------------------
xgb_rec <- recipe(outcome ~ ., data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_date(all_date_predictors(), keep_original_cols = FALSE, features = "month") %>%
  step_integer(all_ordered_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = thresh_other, other = "other_combined") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = thresh_corr, method = "spearman")

## setup the model & tune hyperparameters -------------------------------------
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>% 
  add_model(xgb_spec)

xgb_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 500
)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

xgb_res <- tune_grid(
  xgb_wf,
  resamples = df_train_cv,
  grid = xgb_grid,
  control = ctl_grid
)

stopCluster(cl)

xgb_res %>%
  autoplot()

## select best model ----------------------------------------------------------
xgb_best_auc <- xgb_res %>%
  show_best("roc_auc", n = 4)

final_xgb <- finalize_workflow(
  xgb_wf,
  xgb_best_auc[1,]
)

final_xgb_fit <- final_xgb %>%
  last_fit(df_split)

collect_metrics(final_xgb_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

xgb_train_fit <- fit_resamples(final_xgb,
                               resamples = df_train_cv,
                               metrics = metric_set(roc_auc, accuracy, sens,spec),
                               control = ctl_grid)

stopCluster(cl)
  
# save ####
save(df_split, df_train, df_test,
     step_model, log_train_fit, final_log_fit,
     final_lasso_fit, lasso_best_auc, lasso_train_fit,
     final_xgb, final_xgb_fit, xgb_train_fit,
     xgb_shap_data, dep_plot, deps,two_pdp,
     file = "raw_data/model_data.RData")
