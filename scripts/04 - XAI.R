# packages --------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(glmnet)
library(fastshap)
library(shapviz)
library(pdp)
library(parallel)
library(doParallel)
library(probably)
library(gridExtra)
library(doFuture)
library(ggpubr)
library(ggtext)
library(ggbump)
tidymodels_prefer()
source("~/Documents/stat_projects/longevity/scripts/00 - funcs_def.R")
set.seed(45)
cat("\f")

# SHAP objects ----------------------------------------------------------------
## logistic reg ---------------------------------------------------------------
step_model$anova %>%
  rename(var = Step) %>%
  mutate(var = str_remove(str_split_i(var, "\\+ ", i=2),regex("_poly_[1-3]")),
         Deviance = round(100*Deviance/step_model$null.deviance,2)) %>%
  drop_na(var) %>%
  head(var_num) %>%
  arrange(desc(Deviance)) -> log_imp_vars

glm(outcome ~ ., data = imp_train_df, family = "binomial") -> full_log_model

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

log_sum_dev <- foreach (i = 1:(ncol(imp_train_df)-1),
                        .combine = 'rbind',
                        .packages = c("tidyverse")) %dopar% {
                          glm(outcome ~ ., data = imp_train_df[-i], family = "binomial") -> model
                          
                          tibble(
                            var = colnames(imp_train_df)[i],
                            full_deviance = full_log_model$deviance,
                            null_deviance = model$null.deviance,
                            residual_deviance = model$deviance,
                            deviance = full_deviance-residual_deviance,
                            dev.ratio=1-deviance/full_deviance
                          )
                          
                        }

stopCluster(cl)

## LASSO ----------------------------------------------------------------------
glmnet(model.matrix(outcome~., imp_train_df), 
       imp_train_df %>%
         select(outcome) %>%
         unlist() %>%
         as.numeric(), 
       alpha = 1, lambda = lasso_best_auc$penalty) -> full_lasso_model

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores-2)
registerDoParallel(cl)

lasso_sum_dev <- foreach (i = 1:(ncol(imp_train_df)-1),
                          .combine = 'rbind',
                          .packages = c("glmnet","tidyverse")) %dopar% {
                            x = model.matrix(outcome~., imp_train_df[-i])[,-1] 
                            y = imp_train_df %>%
                              select(outcome) %>%
                              unlist() %>%
                              as.numeric()
                            
                            glmnet(x, y, alpha = 1, lambda = lasso_best_auc$penalty) -> model
                            
                            tibble(
                              var = colnames(imp_train_df)[i],
                              full_deviance = deviance(full_lasso_model),
                              null_deviance = model$nulldev,
                              dev.ratio = model$dev.ratio,
                              res_deviance = (1-dev.ratio)*null_deviance,
                              deviance = res_deviance-full_deviance)
                          }
stopCluster(cl)

lasso_sum_dev %>%
  top_n(10, res_deviance) %>%
  arrange(desc(res_deviance)) -> lasso_imp_vars

## XGB ------------------------------------------------------------------------
xgb_prep <- xgb_rec %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data <- bake(xgb_prep,
                      has_role("predictor"),
                      new_data = df_train,
                      composition = "matrix")

shap_xgb <- shapviz(extract_fit_engine(final_xgb_fit), X_pred = xgb_shap_data,
                    interactions = TRUE)
### SHAP combined -------------------------------------------------------------
shap_xgb_collapse <- shap_xgb

shap_xgb_collapse$S <- collapse_shap(shap_xgb$S, 
                                     collapse = list(med_smoke_status = c("med_smoke_status_@_ex.smoker",
                                                                          "med_smoke_status_@_X1.10",
                                                                          "med_smoke_status_@_X11.20",
                                                                          "med_smoke_status_@_X20.")))


shap_xgb_collapse$S_inter <- collapse_shap(shap_xgb$S_inter, 
                                           collapse = list(med_smoke_status = c("med_smoke_status_@_ex.smoker",
                                                                                "med_smoke_status_@_X1.10",
                                                                                "med_smoke_status_@_X11.20",
                                                                                "med_smoke_status_@_X20.")))

shap_xgb_collapse$X <- shap_xgb$X %>%
  mutate(med_smoke_status = case_when(`med_smoke_status_@_ex.smoker` == 1 ~ 1,
                                      `med_smoke_status_@_X1.10` == 1 ~ 2,
                                      `med_smoke_status_@_X11.20` == 1 ~ 3,
                                      `med_smoke_status_@_X20.` == 1 ~ 4,
                                      TRUE ~ 0)) %>%
  select(-c("med_smoke_status_@_ex.smoker",
            "med_smoke_status_@_X1.10",
            "med_smoke_status_@_X11.20",
            "med_smoke_status_@_X20."))

# Figure 2 - variable importance -----------------------------------------------
var_num <- 10 # number of variables to importance plot

log_features = c("#F78311FF","#fca50a","#fca50a","#A52C60FF","#fca50a",
                 "#CF4446FF","#fca50a","#F5DB4BFF","#781C6DFF","#4B0C6BFF")
shap_imp_log <- log_imp_vars %>%
  ggplot(aes(x = reorder(var, Deviance), y = Deviance, fill = var)) +
  geom_col() +
  scale_x_discrete(labels = vars_label) +
  labs(x = "",#"Variable",
       y = "Deviance (%)") +
  scale_fill_manual(values = log_features) +
  # scale_y_continuous(labels = scales::percent) +
  coord_flip()

lasso_features = c("#fca50a","#fca50a","#fca50a","#F78311FF","#A52C60FF",
                   "#CF4446FF","#F5DB4BFF","#781C6DFF","#4B0C6BFF","#fca50a")

shap_imp_lasso <- lasso_imp_vars %>%
  ggplot(aes(x = reorder(var, deviance), y = deviance, fill = var)) +
  geom_col() +
  scale_x_discrete(labels = vars_label) +
  labs(x = "",#"Variable",
       y = "Deviance (%)") +
  scale_fill_manual(values = lasso_features) +
  # scale_y_continuous(labels = scales::percent) +
  coord_flip()

xgb_features = c("#4B0C6BFF","#781C6DFF","#fca50a","#A52C60FF","#CF4446FF",
                 "#fca50a","#fca50a","#fca50a","#F78311FF","#fca50a")

shap_imp_bar_xgb <- sv_importance(shap_xgb_collapse, kind = "bar", show_numbers = TRUE,
                                  fill = xgb_features,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label)

shap_imp_bee_xgb <- sv_importance(shap_xgb_collapse, kind = "beeswarm", show_numbers = FALSE,
                                  max_display = var_num) +
  scale_y_discrete(labels = vars_label) +
  theme_classic() +
  plot_theme

log_shap_vars <- as.character(unique(sapply(shap_imp_log$data$var, label_all)))
lasso_shap_vars <- as.character(unique(sapply(shap_imp_lasso$data$var, label_all)))
xgb_shap_vars <- as.character(unique(str_split(sapply(shap_imp_bar_xgb$data$feature, label_all), " - ", simplify = TRUE)[,1]))

all <- intersect(log_shap_vars,intersect(lasso_shap_vars,xgb_shap_vars))
log_lasso <- intersect(log_shap_vars,lasso_shap_vars)[!(intersect(log_shap_vars,lasso_shap_vars) %in% all)]
log_xgb <- intersect(log_shap_vars,xgb_shap_vars)[!(intersect(log_shap_vars,xgb_shap_vars) %in% all)]
xgb_lasso <- intersect(xgb_shap_vars,lasso_shap_vars)[!(intersect(xgb_shap_vars,lasso_shap_vars) %in% all)]

linewidth_log <- rep(0, length(shap_imp_log$data$var))
linewidth_log[vars_label(shap_imp_log$data$var) %in% all] <- .5
linetype_log <- rep(0, length(shap_imp_log$data$var))
linetype_log[vars_label(shap_imp_log$data$var) %in% all] <- 1

shap_imp_log_high <- shap_imp_log +
  scale_x_discrete(labels = ~ if_else(
    vars_label(.x) %in% c(all,log_lasso,log_xgb),
    paste0("<span style='color: red4'><b>", vars_label(.x), "</b></span>"),
    vars_label(.x)
  )) +
  theme_classic() +
  plot_theme +
  theme(legend.position = "none",
        axis.text.y = element_markdown(box.colour = "red4", linewidth = rev(linewidth_log), 
                                       linetype = rev(linetype_log), r = unit(5, "pt"),
                                       padding = unit(3, "pt")))

linewidth_lasso <- rep(0, length(shap_imp_lasso$data$var))
linewidth_lasso[vars_label(shap_imp_lasso$data$var) %in% all] <- .5
linetype_lasso <- rep(0, length(shap_imp_lasso$data$var))
linetype_lasso[vars_label(shap_imp_lasso$data$var) %in% all] <- 1

shap_imp_lasso_high <- shap_imp_lasso +
  scale_x_discrete(labels = ~ if_else(
    vars_label(.x) %in% c(all,log_lasso,xgb_lasso),
    paste0("<span style='color: red4'><b>", vars_label(.x), "</b></span>"),
    vars_label(.x)
  )) +
  theme_classic() +
  plot_theme +
  theme(legend.position = "none",
        axis.text.y = element_markdown(box.colour = "red4", linewidth = rev(linewidth_lasso), 
                                       linetype = rev(linetype_lasso), r = unit(5, "pt"),
                                       padding = unit(3, "pt")))

linewidth_xgb <- rep(0, length(shap_imp_bar_xgb$data$feature))
linewidth_xgb[str_split(sapply(shap_imp_bar_xgb$data$feature, label_all), 
                        " - ", simplify = TRUE)[,1] %in% all] <- .5
linetype_xgb <- rep(0, length(shap_imp_bar_xgb$data$feature))
linetype_xgb[str_split(sapply(shap_imp_bar_xgb$data$feature, label_all), 
                       " - ", simplify = TRUE)[,1] %in% all] <- 1

shap_imp_xgb_high <- shap_imp_bar_xgb +
  scale_y_discrete(labels = ~ if_else(
    str_split(vars_label(.x)," - ", simplify = TRUE)[,1] %in% c(all,log_xgb,xgb_lasso),
    paste0("<span style='color: red4'><b>", vars_label(.x), "</b></span>"),
    vars_label(.x)
  )) +
  theme_classic() +
  plot_theme +
  theme(axis.text.y = element_markdown(box.colour = "red4", linewidth = rev(linewidth_xgb), 
                                       linetype = rev(linetype_xgb), r = unit(5, "pt"),
                                       padding = unit(3, "pt")))

ggarrange(shap_imp_log_high, shap_imp_lasso_high,
          shap_imp_xgb_high,
          shap_imp_bee_xgb + rremove("y.text")+ rremove("y.ticks")+ rremove("y.axis"),
          labels = c("(A) Logistic Regression",
                     "(B) LASSO","(C) XGBoost",
                     "(D) Bee Swarm"),
          label.y = 1,
          font.label = list(size = 20, color = "black", face = "bold"),
          ncol = 2, nrow = 2)

ggsave(filename = file.path("graphs","fig2.pdf"), plot = ggplot2::last_plot(), 
       width = 50, height = 35, dpi = 400, units = "cm", bg = "white")

# save files -----------------------------------------------------------------
save(imp_train_df, log_sum_dev, lasso_sum_dev, 
     log_imp_vars, lasso_imp_vars, shap_xgb,
     file = "raw_data/plots_shap.RData")

save(shap_xgb, file = "/Users/doratias/Documents/stat_projects/private_data/longevity/shap.RData")
