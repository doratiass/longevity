library(tidyverse)
library(gtsummary)
library(smd)
library(gt)
source("~/Documents/stat_projects/longevity/scripts/00 - funcs_def.R")
set.seed(45)
cat("\f")

# Table 2 - cohort characteristics --------------------------------------------
vars <- unique(c(shap_imp_log$data$var,
                 as.character(shap_imp_lasso$data$feature),
                 as.character(shap_imp_bar_xgb$data$feature)))
str_split(vars, "_@_", simplify = TRUE) %>%
  as_tibble() %>%
  select(V1) %>%
  distinct() %>%
  pull() %>%
  sort() -> table_vars

ml_df %>%
  mutate(across(c("dmg_SES", "dmg_num_room"), as.numeric),
         outcome = ifelse(outcome == "centenarian", "Centenarian",
                          "Not centenarian")) %>%
  select("dmg_admission_age","dmg_origin","dmg_martial_status",
         "dmg_num_children","dmg_salary", "dmg_education",
         all_of(table_vars), "med_dm", "outcome") %>%
  rename_all(function(x) sapply(x, label_get,USE.NAMES = FALSE)) %>%
  tbl_summary(by = "outcome",
              type = list(`Socioeconomic Status quartile` ~ "continuous",
                          `Number of Occupants in the Household` ~ "continuous",
                          `Number of Rooms at Household` ~ "continuous"),
              value = list(`Diabetes mellitus` ~ "Present",
                           `Myocardial Infarction` ~ "Present",
                           `Insulin use` ~ TRUE),
              missing = "no") %>%
  add_overall() %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  modify_header(estimate = "**SMD**") %>%
  add_p()

# supp table 1 - variables values ---------------------------------------------
ml_df %>%
  rename_all(function(x) sapply(x, label_get,USE.NAMES = FALSE)) 

tibble(
  vars = colnames(ml_df),
  values = case_when(
    sapply(ml_df, is.factor) ~ map_chr(ml_df, ~paste(unique(.x, na.rm = TRUE), collapse = ", ")),
    sapply(ml_df, is.numeric) ~ map_chr(ml_df, ~paste(range(as.numeric(.x), na.rm = TRUE), collapse = " - ")),
    TRUE ~ NA_character_),
  label = map_chr(vars, label_get)
) -> vars_table

vars_table %>%
  gt() %>%
  tab_header(
    title = "Supplementary Table 1 - Variables Values",
    subtitle = "This table shows the range of values for each variable in the dataset."
  ) %>%
  fmt_number(columns = vars, decimals = 2) %>%
  fmt_number(columns = values, decimals = 0) %>%
  fmt_number(columns = label, decimals = 0) %>%
  tab_spanner(
    label = "Variable",
    columns = vars
  ) %>%
  tab_spanner(
    label = "Values",
    columns = values
  ) %>%
  tab_spanner(
    label = "Label",
    columns = label
  ) %>%
  tab_source_note(
    "Source: Authors' analysis of the dataset."
  ) %>%
  save(., file = "gt_vars_table.html")

# supp table 2 - logistic variables -------------------------------------------
step_model$anova %>%
  rename(Variable = Step) %>%
  mutate(Variable = str_split_i(Variable, "\\+ ", i=2),
         Deviance = round(100*Deviance/step_model$null.deviance,2)) %>%
  drop_na(Variable) %>%
  select(Variable, Deviance) %>%
  mutate(Variable = map_chr(Variable, label_all)) %>%
  arrange(desc(Deviance)) %>%
  gt() %>%
  tab_header(
    title = "Supplementary Table 2 - Logistic regression stepwise variables") #%>%
  save(., file = "log_stepwise_vars_table.html")

# supp table 3 - lasso variables ----------------------------------------------
final_lasso_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  select(Variable = term,
         Estimate = estimate) %>%
  filter(Variable != "(Intercept)",
         Estimate > 0) %>%
  mutate(Variable = map_chr(Variable, label_all),
         Estimate = round(Estimate,4)) %>%
  arrange(desc(Estimate)) %>%
  gt() %>%
  tab_header(
    title = "Supplementary Table 3 - Lasso Variables") #%>%
  save(., file = "lasso_vars_table.html")

# supp table 4 - ROC comparisons -----------------------------------------------
bs_roc_log <- function(splits) {
  x <- analysis(splits)
  roc_auc(x, outcome, pred_log)$.estimate
}

bs_roc_lasso <- function(splits) {
  x <- analysis(splits)
  roc_auc(x, outcome, pred_lasso)$.estimate
}

bs_roc_xgb <- function(splits) {
  x <- analysis(splits)
  roc_auc(x, outcome, pred_xgb)$.estimate
}

left_join(final_xgb_fit %>%
            collect_predictions() %>%
            select(.row, outcome, pred_xgb = .pred_centenarian),
          final_lasso_fit %>%
            collect_predictions() %>%
            select(.row, pred_lasso = .pred_centenarian), by = ".row") %>%
  left_join(final_log_fit %>%
              collect_predictions() %>%
              select(.row, pred_log = .pred_centenarian), by = ".row") -> final_preds 

bs_pred <- bootstraps(final_preds %>% drop_na(), times = 1000)

bs_pred %>% 
  mutate(
    roc_log = map_dbl(splits, bs_roc_log),
    roc_lasso = map_dbl(splits, bs_roc_lasso),
    roc_xgb = map_dbl(splits, bs_roc_xgb)
  ) %>%
  mutate(
    xgb_lasso = roc_xgb - roc_lasso,
    xgb_log = roc_xgb - roc_log,
    lasso_log = roc_lasso - roc_log
  ) %>%
  pivot_longer(cols = !c(splits, id),
               values_to = "val",
               names_to = "par") %>%
  group_by(par) %>%
  summarise(mean = mean(val),
            ul = quantile(val, 0.975),
            ll = quantile(val, 0.025)) #%>%
  # filter(par %in% c("roc_log", "roc_xgb")) %>%
  # ggplot(aes(x = val, color = par)) +
  # geom_density()

# Figure 1 - pre-post calibration ----------------------------------------------
pre_cal_train <- cal_scam_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                                     list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                                     split = "train",
                                     plat = FALSE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

post_cal_train <- cal_scam_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                                      list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                                      split = "train",
                                      plat = TRUE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

pre_cal_test <- cal_scam_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                                    list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                                    split = "test",
                                    plat = FALSE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

post_cal_test <- cal_scam_plot_three(list(final_log_fit, final_lasso_fit, final_xgb_fit),
                                     list(log_train_fit,lasso_train_fit,xgb_train_fit), 
                                     split = "test",
                                     plat = TRUE) +
  plot_theme +
  scale_color_brewer(palette=color_pal)

ggarrange(pre_cal_train,post_cal_train,pre_cal_test,post_cal_test,
          labels = "AUTO",
          ncol = 2, nrow = 2)

ggsave(filename = file.path("graphs","supp_fig1.png"), plot = ggplot2::last_plot(), 
       width = 35, height = 35, dpi = 300, units = "cm", bg = "white")
