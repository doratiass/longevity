# packages ####
library(tidyverse)
library(tidymodels)
library(ggpubr)

set.seed(45)
cat("\f")

cal_plot <- function(final_fit, train_fit, split = c("train", "test","stack"), plat = FALSE) {
  if (split == "train") {
    if (plat) {
      df <- train_fit %>%
        collect_predictions() %>% 
        cal_apply(cal_estimate_logistic(train_fit))
    } else {
      df <- train_fit %>% collect_predictions()
    } 
  } else if (split == "test") {
    if (plat) {
      df <- final_fit %>%
        collect_predictions () %>%
        cal_apply(cal_estimate_logistic(train_fit))
    } else {
      df <- final_fit %>%
        collect_predictions()
    }
  } else if (split == "stack") {
    df <- final_fit
  }
  
  lm_cal <- lm(outcome ~ .pred_centenarian, data = df %>%
                 mutate(outcome = as.numeric(outcome == "centenarian")))
  
  plot_txt <- tibble(
    txt = paste0("Intercept ", round(lm_cal$coefficients[1],2),
                 "\nslope ", round(lm_cal$coefficients[2],2)))
  
  title <- ifelse(split == "train","Training set","Testing set")
  
  df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    ggplot(aes(.pred_centenarian, outcome)) +
    geom_rug(sides = "tb", color = "grey") +
    geom_smooth(method = "loess",
                color = "black",
                se = TRUE, fullrange = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
    geom_text(data = plot_txt, aes(x = 0.1, y = 0.95, label = txt)) +
    labs(x = "Predicted risk",
         y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()) + 
    ggtitle(title) -> plot
  
  return(plot)
}

# ROC ####
## build ROC ####
log_roc <- final_log_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("Logistic Regression", round(collect_metrics(final_log_fit)[2,3],3)))

lasso_roc <- final_lasso_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("LASSO", round(collect_metrics(final_lasso_fit)[2,3],3)))

xgb_roc <- final_xgb_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("XGBoost", round(collect_metrics(final_xgb_fit)[2,3],3)))
# 
# knn_roc <- final_knn_fit %>%
#   collect_predictions() %>%
#   roc_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("KNN", round(collect_metrics(final_knn_fit)[2,3],3)))
# 
# nnet_roc <- final_nnet_fit %>%
#   collect_predictions() %>%
#   roc_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("Nnet", round(collect_metrics(final_nnet_fit)[2,3],3)))
# 
# stack_roc <- stack_model_pred %>%
#   roc_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("Stack", round(roc_auc(data = stack_model_pred, truth = outcome,`.pred_centenarian`)[1,3],3)))


## visualize models ####
rbind(log_roc, lasso_roc, xgb_roc) %>% #, knn_roc, nnet_roc, stack_roc
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = 1.2
  ) +
  geom_path() +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# PR ####
## build PR ####
log_pr <- final_log_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("Logistic Regression", round(pr_auc(data = final_log_fit %>%
                                                             collect_predictions(),
                                                           truth = outcome,
                                                           .pred_centenarian)[1,3],3)))

lasso_pr <- final_lasso_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("LASSO", round(pr_auc(data = final_lasso_fit %>%
                                               collect_predictions(),
                                             truth = outcome,
                                             .pred_centenarian)[1,3],3)))

xgb_pr <- final_xgb_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste("XGBoost", round(pr_auc(data = final_xgb_fit %>%
                                                 collect_predictions(),
                                               truth = outcome,
                                               .pred_centenarian)[1,3],3)))

# knn_pr <- final_knn_fit %>%
#   collect_predictions() %>%
#   pr_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("KNN", round(pr_auc(data = final_knn_fit %>%
#                                              collect_predictions(),
#                                            truth = outcome,
#                                            .pred_centenarian)[1,3],3)))
# 
# nnet_pr <- final_nnet_fit %>%
#   collect_predictions() %>%
#   pr_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("Nnet", round(pr_auc(data = final_nnet_fit %>%
#                                               collect_predictions(),
#                                             truth = outcome,
#                                             .pred_centenarian)[1,3],3)))
# 
# stack_pr <- stack_model_pred %>%
#   pr_curve(outcome, .pred_centenarian) %>%
#   mutate(model = paste("Stack", round(pr_auc(data = stack_model_pred, truth = outcome,`.pred_centenarian`)[1,3],3)))

## visualize models ####
rbind(log_pr, lasso_pr, xgb_pr) %>% #, knn_pr, nnet_pr, stack_pr
  filter(!is.infinite(.threshold),
         !is.infinite(precision)) %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path() +
  coord_equal(ratio = 1/0.4) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Calibration ####
## logistic reg ####
log_tr <- cal_plot(final_log_fit,log_train_fit, "train", TRUE)
log_ts <- cal_plot(final_log_fit,log_train_fit, "test", TRUE)

cal_fig_log <- ggarrange(log_tr + rremove("xylab"),
                         log_ts + rremove("xylab"),
                         labels = "AUTO", label.y = 0.96,
                         ncol = 2)

annotate_figure(cal_fig_log,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure X - Calibration Plot",
                fig.lab.face = "bold", fig.lab.size = 14)
## LASSO ####
lasso_tr <- cal_plot(final_lasso_fit,lasso_train_fit, "train", TRUE)
lasso_ts <- cal_plot(final_lasso_fit,lasso_train_fit, "test", TRUE)

cal_fig_lasso <- ggarrange(lasso_tr + rremove("xylab"),
                         lasso_ts + rremove("xylab"),
                         labels = "AUTO", label.y = 0.96,
                         ncol = 2)

annotate_figure(cal_fig_lasso,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure X - Calibration Plot",
                fig.lab.face = "bold", fig.lab.size = 14)
## XGBoost ####
xgb_tr <- cal_plot(final_xgb_fit,xgb_train_fit, "train", TRUE)
xgb_ts <- cal_plot(final_xgb_fit,xgb_train_fit, "test", TRUE)

cal_fig_xgb <- ggarrange(xgb_tr + rremove("xylab"),
                         xgb_ts + rremove("xylab"),
                           labels = "AUTO", label.y = 0.96,
                           ncol = 2)

annotate_figure(cal_fig_xgb,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure X - Calibration Plot",
                fig.lab.face = "bold", fig.lab.size = 14)

# ## KNN ####
# knn_tr <- cal_plot(final_knn_fit,knn_train_fit, "train", TRUE)
# knn_ts <- cal_plot(final_knn_fit,knn_train_fit, "test", TRUE)
# 
# cal_fig_knn <- ggarrange(knn_tr + rremove("xylab"),
#                          knn_ts + rremove("xylab"),
#                          labels = "AUTO", label.y = 0.96,
#                          ncol = 2)
# 
# annotate_figure(cal_fig_knn,
#                 bottom = text_grob("Predicted risk", color = "black", vjust = -1),
#                 left = text_grob("Observed proportion", color = "black", rot = 90),
#                 fig.lab = "Figure X - Calibration Plot",
#                 fig.lab.face = "bold", fig.lab.size = 14)
# 
# ## Nnet ####
# nnet_tr <- cal_plot(final_nnet_fit,nnet_train_fit, "train", TRUE)
# nnet_ts <- cal_plot(final_nnet_fit,nnet_train_fit, "test", TRUE)
# 
# cal_fig_nnet <- ggarrange(nnet_tr + rremove("xylab"),
#                          nnet_ts + rremove("xylab"),
#                          labels = "AUTO", label.y = 0.96,
#                          ncol = 2)
# 
# annotate_figure(cal_fig_nnet,
#                 bottom = text_grob("Predicted risk", color = "black", vjust = -1),
#                 left = text_grob("Observed proportion", color = "black", rot = 90),
#                 fig.lab = "Figure X - Calibration Plot",
#                 fig.lab.face = "bold", fig.lab.size = 14)
# 
# ## Stack ####
# stack_ts <- cal_plot(stack_model_pred,NULL, "stack", FALSE)
# 
# annotate_figure(stack_ts + rremove("xylab"),
#                 bottom = text_grob("Predicted risk", color = "black", vjust = -1),
#                 left = text_grob("Observed proportion", color = "black", rot = 90),
#                 fig.lab = "Figure X - Calibration Plot",
#                 fig.lab.face = "bold", fig.lab.size = 14)
