# packages --------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(gtsummary)
library(ggpubr)
library(modelr)
library(readr)
# Theme -----------------------------------------------------------------------
plot_theme <- theme(
  plot.title = element_text(size = 25, hjust = 0.5),
  axis.title = element_text(size = 17),
  axis.text = element_text(size = 15),
  legend.text = element_text(size = 15),
  strip.text = element_text(size = 15))

color_pal <- 'Set1'

line_size <- 1.2

leg_size_4 <- 16

# functions -------------------------------------------------------------------
vars_dict <- read_csv("longevity_shap/vars_dict.csv",show_col_types = FALSE)

label_get <- function(x) {ifelse(x %in% vars_dict$var,vars_dict[vars_dict$var == x,"name",drop = TRUE],x)}
level_get <- function(x) {
  if(str_detect(x, "X[0-9][0-9]\\.[0-9][0-9]")) {
    str_replace_all(str_remove(x,"X"),"\\.","-")
  } else if (str_detect(x, "X[0-9][0-9]\\.")) {
    str_replace_all(str_remove(x,"X"),"\\.","+")
  } else if (str_detect(x, "_")) {
    str_replace_all(x,"_", " ")
  } else {x}}

label_all <- function(x) {
  if(str_detect(x, "_@_")) {
    paste0(label_get(str_split(x, "_@_", simplify = TRUE)[,1])," - ",
           level_get(str_split(x, "_@_", simplify = TRUE)[,2]))}
  else if (str_detect(x, "_poly_")) {
    paste0(label_get(str_split(x, "_poly_", simplify = TRUE)[,1])," - poly ",
           label_get(str_split(x, "_poly_", simplify = TRUE)[,2]))}
  else {label_get(x)}
}

vars_label <- function(x) {sapply(x, label_all, USE.NAMES = FALSE)}
var_get <- function(x) {ifelse(x %in% vars_dict$name,vars_dict[vars_dict$name == x,"var",drop = TRUE],x)}

int_labeler <- function(x) {
  x <- str_remove_all(x, "s\\(|\\)|\\(")
  case_when(
    x == "med_bmi_mean" ~ "BMI",
    str_detect(x, "med_bmi_mean") ~ paste0("BMI (",str_remove_all(x,"med_bmi_mean"),")"),
    x == "med_sbp_mean" ~ "SBP")
}

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
    geom_rug(color = "grey",sides = "tb",alpha = 0.2) + 
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

cal_scam_plot_three <- function(final_fit, train_fit, split = c("train", "test") ,plat = FALSE) {
  
  scam_model_1 <- scam(outcome ~ s(.pred_centenarian, bs = "mpi", m = 1),
                       data = train_fit[[1]] %>%
                         collect_predictions() %>%
                         mutate(outcome = if_else(outcome == "centenarian", 1, 0)),
                       family = binomial)
  
  scam_model_2 <- scam(outcome ~ s(.pred_centenarian, bs = "mpi", m = 1),
                       data = train_fit[[2]] %>%
                         collect_predictions() %>%
                         mutate(outcome = if_else(outcome == "centenarian", 1, 0)),
                       family = binomial)
  
  scam_model_3 <- scam(outcome ~ s(.pred_centenarian, bs = "mpi", m = 1),
                       data = train_fit[[3]] %>%
                         collect_predictions() %>%
                         mutate(outcome = if_else(outcome == "centenarian", 1, 0)),
                       family = binomial)
  
  
  if (split == "train") {
    if (plat) {
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_1, type = "response") %>%
          mutate(model = "Logistic reg"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_2, type = "response") %>%
          mutate(model = "LASSO"),
        train_fit[[3]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_3, type = "response") %>%
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
        rename(pred = .pred_centenarian) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    }
    
  } else if (split == "test") {
    if (plat) {
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_1, type = "response") %>%
          mutate(model = "Logistic reg"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_2, type = "response") %>%
          mutate(model = "LASSO"),
        final_fit[[3]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_3, type = "response") %>%
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
        rename(pred = .pred_centenarian) %>%
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
    summarise(int = coef_int(outcome,pred),
              slope = coef_slope(outcome,pred)) %>%
    mutate(txt = paste0(model,", int: ", sprintf(int, fmt = '%#.2f'),", slope ", sprintf(slope, fmt = '%#.2f')),
           inx = c(2,1,3))
  
  title <- ifelse(plat,
                  paste("Calibration plot -",split,"set - platt scaling"),
                  paste("Calibration plot -",split,"set"))
  
  df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    ggplot(aes(pred, outcome, color = model)) +
    geom_rug(color = "grey",sides = "tb",alpha = 0.2) + 
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
