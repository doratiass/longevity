# packages ####
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

var_num <- 10

pfun <- function(object, newdata) {  # needs to return a numeric vector
  predict(object, newdata, type = "prob")[,".pred_centenarian",drop = T]
}

# logistic reg ####
shap_exp_log <- fastshap::explain(extract_workflow(final_log_fit), X = df_test,
                                  pred_wrapper = pfun, shap_only = FALSE)

shap_log <- shapviz(shap_exp_log)

shap_imp_log <- sv_importance(shap_log, kind = "bar", show_numbers = TRUE,
                              max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

# LASSO ####
shap_exp_lasso <- fastshap::explain(extract_workflow(final_lasso_fit), X = df_test,
                                    pred_wrapper = pfun, shap_only = FALSE)

shap_lasso <- shapviz(shap_exp_lasso)

shap_imp_lasso <- sv_importance(shap_lasso, kind = "bar", show_numbers = TRUE,
                                max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic()

# XGB ####
## SHAP values ####
xgb_prep <- xgb_rec %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data <- bake(xgb_prep,
                      has_role("predictor"),
                      new_data = df_test,
                      composition = "matrix")

shap_xgb <- shapviz(extract_fit_engine(final_xgb_fit), X_pred = xgb_shap_data,
                    interactions = TRUE)

shap_imp_bar_xgb <- sv_importance(shap_xgb, kind = "bar", show_numbers = TRUE,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic(base_size = 16)


shap_imp_bee_xgb <- sv_importance(shap_xgb, kind = "beeswarm", show_numbers = FALSE,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic(base_size = 16)

ggarrange(shap_imp_bar_xgb,
          shap_imp_bee_xgb + rremove("y.text")+ rremove("y.ticks")+ rremove("y.axis"),
          labels = "AUTO",
          ncol = 2)

## PDP ####
sv_dependence(shap_xgb, v = deps, 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE)  &
  labs(x = NULL) &
  theme_classic(base_size = 12)

deps <- shap_imp_bar_xgb$data %>%
  mutate(feature = as.character(feature)) %>%
  distinct(feature) %>%
  pull(feature)

dep_plot_list <- lapply(deps, pdp::partial, 
                        object = extract_fit_engine(final_xgb_fit), 
                        train = xgb_shap_data)

for (i in 1:length(dep_plot_list)) {
  dep_plot_list[[i]][,"feature"] <- colnames(dep_plot_list[[i]])[1]
  names(dep_plot_list[[i]])[1] <- "shap"
}

dep_plot <- bind_rows(dep_plot_list)

dep_plot %>%
  ggplot(aes(shap, yhat)) +
  geom_path() + 
  # geom_rug(sides = "b", color = "grey") +
  facet_wrap(~feature, scales = "free") +
  theme_classic()

# 3 models vars ####
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
  geom_line(size = 1.5) +
  labs(x = "",
       y = "") +
  theme_classic(base_size = 16)+
  theme(
    legend.position = "none"
  )
  
