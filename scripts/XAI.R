# packages --------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(fastshap)
library(shapviz)
library(pdp)
library(parallel)
library(doParallel)
library(probably)
library(gridExtra)
library(doFuture)
library(ggpubr)
library(ggbump)
tidymodels_prefer()

source("~/Documents/stat_projects/longevity/longevity_shap/dict.R")
set.seed(45)
cat("\f")

# defenitions $ functions -----------------------------------------------------
var_num <- 10 # number of variables to importance plot

# prediction function for fastshap with isotonic calibration
pfun <- function(object, newdata) {  #, train = train_model
  probs <- predict(object, newdata, type = "prob") %>%
  #  cal_apply(cal_estimate_isotonic(train)) %>%
    pull(.pred_centenarian)
  return(probs)
}

# logistic reg ---------------------------------------------------------------
train_model <- log_train_fit
all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

shap_exp_log <- fastshap::explain(extract_workflow(final_log_fit), X = df_test,
                                  pred_wrapper = pfun, shap_only = FALSE,
                                  parallel = TRUE,
                                #  .export = c("train_model"),
                                  .packages = c("tidyverse","tidymodels", "probably"))

stopCluster(cl)

shap_log <- shapviz(shap_exp_log)

shap_imp_log <- sv_importance(shap_log, kind = "bar", show_numbers = TRUE,
                              max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

# LASSO -----------------------------------------------------------------------
train_model <- lasso_train_fit
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

shap_exp_lasso <- fastshap::explain(extract_workflow(final_lasso_fit), X = df_test,
                                    pred_wrapper = pfun, shap_only = FALSE,
                                    parallel = TRUE,
                                    #  .export = c("train_model"),
                                    .packages = c("tidyverse","tidymodels", "probably"))


stopCluster(cl)

shap_lasso <- shapviz(shap_exp_lasso)

shap_imp_lasso <- sv_importance(shap_lasso, kind = "bar", show_numbers = TRUE,
                                max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

# XGB -------------------------------------------------------------------------
## SHAP values ----------------------------------------------------------------
xgb_prep <- xgb_rec %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data <- bake(xgb_prep,
                      has_role("predictor"),
                      new_data = df_test,
                      composition = "matrix")

shap_xgb <- shapviz(extract_fit_engine(final_xgb_fit), X_pred = xgb_shap_data,
                    interactions = TRUE,
                    collapse = list(med_smoke_status = c("med_smoke_status_X11.20", "med_smoke_status_X20.", "med_smoke_status_ex.smoker", "med_smoke_status_never.smoked")))

shap_imp_bar_xgb <- sv_importance(shap_xgb, kind = "bar", show_numbers = TRUE,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

shap_imp_bee_xgb <- sv_importance(shap_xgb, kind = "beeswarm", show_numbers = FALSE,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

ggarrange(shap_imp_log, shap_imp_lasso,
          shap_imp_bar_xgb,
          shap_imp_bee_xgb + rremove("y.text")+ rremove("y.ticks")+ rremove("y.axis"),
          labels = "AUTO",
          ncol = 2, nrow = 2)

## DP -------------------------------------------------------------------------
deps <- sv_importance(shap_xgb, kind = "bar", show_numbers = TRUE,
                                  max_display = 30)$data %>%
  mutate(feature = as.character(feature)) %>%
  filter(feature %in% names(ml_df[,sapply(ml_df, is.numeric)]),
         feature != "med_bmi_sd") %>%
  distinct(feature) %>%
  pull(feature) 

sv_dependence(shap_xgb, v = deps[1:12], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps

for (i in 1:length(dps)) {
  dps[[i]]$labels$title <- vars_label(dps[[i]]$labels$title)
}

dps  &
  labs(x = NULL) &
  theme_classic(base_size = 10)

## interactions ---------------------------------------------------------------
int_a <- sv_dependence(shap_xgb, v = "med_sbp_mean", 
              color_var = "dmg_admission_age", 
              alpha = 0.5, interactions = TRUE)  &
  theme_classic(base_size = 10) &
  theme(legend.position = "bottom")

int_b <- sv_dependence(shap_xgb, v = "lab_glucose", 
              color_var = "med_smoke_status_X20.", 
              alpha = 0.5, interactions = TRUE)  &
  theme_classic(base_size = 10)  &
  theme(legend.position = "bottom")

int_c <- sv_dependence(shap_xgb, v = "lab_mean_hdl", 
              color_var = "med_mi_other_combined", 
              alpha = 0.5, interactions = TRUE)  &
  theme_classic(base_size = 10)  &
  theme(legend.position = "bottom")

int_d <- sv_dependence(shap_xgb, v = "med_bmi_mean", 
              color_var = "med_dm_other_combined", 
              alpha = 0.5, interactions = TRUE)  &
  theme_classic(base_size = 10)  &
  theme(legend.position = "bottom")

ggarrange(int_a, int_b, int_c, int_d,
          labels = "AUTO",
          ncol = 2, nrow = 2)

# 3 models vars ---------------------------------------------------------------
log_shap_vars <- as.character(unique(sapply(shap_imp_log$data$feature, label_get)))
lasso_shap_vars <- as.character(unique(sapply(shap_imp_lasso$data$feature, label_get)))
xgb_shap_vars <- as.character(unique(sapply(shap_imp_bar_xgb$data$feature, label_get)))
xgb_shap_vars[xgb_shap_vars == "Smoking Status (more than 20 cigarretes per day)"] <- "Smoking Status"
xgb_shap_vars[xgb_shap_vars == "Smoking Status (11-20 cigarretes per day)"] <- "Smoking Status"
xgb_shap_vars[xgb_shap_vars == "Smoking Status (never smoked)"] <- "Smoking Status"
xgb_shap_vars <- unique(xgb_shap_vars)

all <- intersect(log_shap_vars,intersect(lasso_shap_vars,xgb_shap_vars))
log_lasso <- intersect(log_shap_vars,lasso_shap_vars)[!(intersect(log_shap_vars,lasso_shap_vars) %in% all)]
log_xgb <- intersect(log_shap_vars,xgb_shap_vars)[!(intersect(log_shap_vars,xgb_shap_vars) %in% all)]
xgb_lasso <- intersect(xgb_shap_vars,lasso_shap_vars)[!(intersect(xgb_shap_vars,lasso_shap_vars) %in% all)]

tibble(
  log = log_shap_vars,
  lasso = lasso_shap_vars,
  xgb = c(xgb_shap_vars,NA)
) %>%
  pivot_longer(cols = c(log,lasso,xgb),
               names_to = "model",
               values_to = "var") %>%
  drop_na() %>%
  mutate(model = factor(model, levels = c("log", "lasso", "xgb"))) %>%
  arrange(var) %>%
  ggplot(aes(x = model, y = var, color = var, group = var)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  labs(x = "",
       y = "") +
  theme_classic(base_size = 16)+
  theme(
    legend.position = "none"
  )
  
