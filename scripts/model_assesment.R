# packages --------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(gtsummary)
library(ggpubr)

set.seed(45)
cat("\f")

# functions -------------------------------------------------------------------
bootstrap_pr <- function(splits) {
  x <- analysis(splits)
  pr_auc(x, outcome, .pred_centenarian)$.estimate
}

bootstrap_roc <- function(splits) {
  x <- analysis(splits)
  roc_auc(x, outcome, .pred_centenarian)$.estimate
}

cal_plot_three <- function(final_fit, train_fit, split = c("train", "test") ,plat = FALSE) {
  if (split == "train") {
    if (plat) {
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[1]])) %>% 
          mutate(model = "Logistic reg"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[2]])) %>% 
          mutate(model = "LASSO"),
        train_fit[[3]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[3]])) %>% 
          mutate(model = "XGBoost")) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    } else {
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "Logistic reg"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        train_fit[[3]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    }
    
  } else if (split == "test") {
    if (plat) {
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[1]])) %>% 
          mutate(model = "Logistic reg"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[2]])) %>% 
          mutate(model = "LASSO"),
        final_fit[[3]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[3]])) %>% 
          mutate(model = "XGBoost")) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    } else {
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "Logistic reg"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        final_fit[[3]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    }
  }
  
  coef_int <- function(y,x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    
    return(round(lm_cal$coefficients[1],2))
  }
  
  coef_slope <- function(y,x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    
    return(round(lm_cal$coefficients[2],2))
  }
  
  plot_txt <- df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    group_by(model) %>%
    summarise(int = coef_int(outcome,`.pred_centenarian`),
              slope = coef_slope(outcome,`.pred_centenarian`)) %>%
    mutate(txt = paste0(model,", int: ", int,", slope ", slope),
           inx = c(2,1,3))
  
  title <- ifelse(plat,
                  paste("Calibration plot -",split,"set - platt scaling"),
                  paste("Calibration plot -",split,"set"))
  
  df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    ggplot(aes(.pred_centenarian, outcome, color = model)) +
    geom_rug(aes(group=model),sides = "tb",alpha = 0.2) + #, color = "grey"
    geom_smooth(linewidth = line_size, method = "loess", se = TRUE, fullrange = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
    geom_point(data = plot_txt, aes(x = 0.5, y = 0.2-0.05*inx), shape = 15, size = 3) +
    geom_text(data = plot_txt, aes(x = 0.55, y = 0.2-0.05*inx, label = txt, size = 15),
              color = "black", hjust = 0) +
    labs(x = "Predicted risk",
         y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    ) -> plot #+ ggtitle(title)
  
  return(plot)
}

# Theme -----------------------------------------------------------------------
plot_theme <- theme(
  plot.title = element_text(size = 25, hjust = 0.5),
  axis.title = element_text(size = 17),
  axis.text = element_text(size = 15),
  legend.text = element_text(size = 15),
  strip.text = element_text(size = 15))

color_pal <- 'Dark2'

line_size <- 1.2



# ROC + PR ---------------------------------------------------------------------
## summarise metrics -----------------------------------------------------------
model_list <- list(final_log_fit,final_lasso_fit,final_xgb_fit)
train_model_list <- list(log_train_fit,lasso_train_fit,xgb_train_fit)

model_names <- c("Logistic regression","LASSO","XGBoost")

sum_models_list <- list()

for (i in 1:length(model_list)) {
  bs_df <- bootstraps(model_list[[i]] %>%
                        collect_predictions(), 
                      times = 1000)
  
  
  par_sum <- bs_df %>% 
    mutate(
      pr = map_dbl(splits, bootstrap_pr), 
      roc = map_dbl(splits, bootstrap_roc)
    ) %>%
    pivot_longer(cols = !c(splits, id),
                 values_to = "val",
                 names_to = "par") %>%
    group_by(par) %>%
    mutate(model = model_names[i])
  
  sum_models_list <- append(sum_models_list, list(par_sum))
}

sum_models <- bind_rows(sum_models_list)

sum_models %>%
  filter(par == "roc") %>%
  group_by(model) %>%
  summarise(mean = mean(val),
            ul = quantile(val, 0.975),
            ll = quantile(val, 0.025)) -> roc_bootstrap

sum_models %>%
  filter(par == "pr") %>%
  group_by(model) %>%
  summarise(mean = mean(val),
            ul = quantile(val, 0.975),
            ll = quantile(val, 0.025)) -> pr_bootstrap

## build ROC -------------------------------------------------------------------
log_roc <- final_log_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(roc_bootstrap[2,1]," - ", round(roc_bootstrap[2,2],3), " (", 
                        round(roc_bootstrap[2,4,drop=T],3),"-",
                        round(roc_bootstrap[2,3,drop=T],3),")"),
         inx = 1)

lasso_roc <- final_lasso_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(roc_bootstrap[1,1]," - ", round(roc_bootstrap[1,2],3), " (", 
                        round(roc_bootstrap[1,4,drop=T],3),"-",
                        round(roc_bootstrap[1,3,drop=T],3),")"),
         inx = 2)

xgb_roc <- final_xgb_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(roc_bootstrap[3,1]," - ", round(roc_bootstrap[3,2],3), " (", 
                        round(roc_bootstrap[3,4,drop=T],3),"-",
                        round(roc_bootstrap[3,3,drop=T],3),")"),
         inx = 3)

## build PR ####
log_pr <- final_log_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(pr_bootstrap[2,1]," - ", round(pr_bootstrap[2,2],3), " (", 
                        round(pr_bootstrap[2,4,drop=T],3),"-",
                        round(pr_bootstrap[2,3,drop=T],3),")"),
         inx = 1)

lasso_pr <- final_lasso_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(pr_bootstrap[1,1]," - ", round(pr_bootstrap[1,2],3), " (", 
                        round(pr_bootstrap[1,4,drop=T],3),"-",
                        round(pr_bootstrap[1,3,drop=T],3),")"),
         inx = 2)

xgb_pr <- final_xgb_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian) %>%
  mutate(model = paste0(pr_bootstrap[3,1]," - ", round(pr_bootstrap[3,2],3), " (", 
                        round(pr_bootstrap[3,4,drop=T],3),"-",
                        round(pr_bootstrap[3,3,drop=T],3),")"),
         inx = 3)

## visualize models -----------------------------------------------------------
### ROC -----------------------------------------------------------------------
roc_plot <- rbind(log_roc, lasso_roc, xgb_roc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = line_size
  ) +
  geom_path(linewidth = line_size) +
  geom_point(aes(x = 0.3, y = 0.2-0.05*inx), shape = 15, size = 3) +
  geom_text(aes(x = 0.35, y = 0.2-0.05*inx, label = model, size = 15),
            color = "black", hjust = 0, check_overlap = T) +
  coord_equal() +
  theme_bw() +
  plot_theme +
  scale_color_brewer(palette=color_pal) +
  theme(legend.position = "none",
        legend.title = element_blank())

roc_plot

### PR ------------------------------------------------------------------------
pr_plot <- rbind(log_pr, lasso_pr, xgb_pr) %>% 
  filter(!is.infinite(.threshold)) %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path() +
  geom_point(aes(x = 0, y = 1-0.05*inx), shape = 15, size = 3) +
  geom_text(aes(x = 0.05, y = 1-0.05*inx, label = model, size = 15),
            color = "black", hjust = 0, check_overlap = T) +
  coord_equal() +
  theme_bw() +
  plot_theme +
  scale_color_brewer(palette=color_pal) +
  theme(legend.position = "none",
        legend.title = element_blank())

### Calibration ---------------------------------------------------------------
cal_train <- cal_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                            list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                            split = "train",
                            plat = TRUE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

cal_train

cal_test <- cal_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                           list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                           split = "test",
                           plat = TRUE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

cal_test

### All together --------------------------------------------------------------
sums_plot <- ggarrange(roc_plot,  pr_plot, cal_train, cal_test,
                       labels = "AUTO",# label.y = 0.96,
                       ncol = 2, nrow = 2)

sums_plot

ggsave(filename = file.path("graphs","fig1.pdf"), plot = ggplot2::last_plot(), 
       width = 35, height = 35, dpi = 300, units = "cm", bg = "white")

