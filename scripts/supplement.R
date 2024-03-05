library(tidyverse)
library(gtsummary)
library(smd)
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
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
              type = list(`dmg_SES` ~ "continuous",
                          `Number of Occupants in the Household` ~ "continuous",
                          `Number of Rooms at Household` ~ "continuous"),
              value = list(Diabetes ~ "Present",
                           `Myocardial Infarction` ~ "Present"),
              missing = "no") %>%
  add_overall() %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  modify_header(estimate = "**SMD**") %>%
  add_p()

# Table 3 - ROC comparisons ---------------------------------------------------
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

left_join(final_log_fit %>%
            collect_predictions() %>%
            select(.row, outcome, pred_log = .pred_centenarian),
          final_lasso_fit %>%
            collect_predictions() %>%
            select(.row, pred_lasso = .pred_centenarian), by = ".row") %>%
  left_join(final_xgb_fit %>%
              collect_predictions() %>%
              select(.row, pred_xgb = .pred_centenarian), by = ".row") -> final_preds

bs_pred <- bootstraps(final_preds, times = 1000)

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
            ll = quantile(val, 0.025)) %>%
  filter(!str_detect(par, "roc_"))

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

# Figure 1 - interactions -----------------------------------------------------
leg_size_4 <- 14

int_a <- sv_dependence(shap_xgb, v = "med_sbp_mean", 
                       color_var = "med_smoke_status_X20.", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_a$labels$x <- vars_label(int_a$labels$x)
int_a$labels$colour <- vars_label(int_a$labels$colour)

int_b <- sv_dependence(shap_xgb, v = "lab_glucose", 
                       color_var = "dmg_education", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_b$labels$x <- vars_label(int_b$labels$x)
int_b$labels$colour <- vars_label(int_b$labels$colour)

int_c <- sv_dependence(shap_xgb, v = "lab_mean_hdl", 
                       color_var = "med_mi_other_combined", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_c$labels$x <- vars_label(int_c$labels$x)
int_c$labels$colour <- vars_label(int_c$labels$colour)

int_d <- sv_dependence(shap_xgb, v = "med_bmi_mean", 
                       color_var = "med_dm_other_combined", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_d$labels$x <- vars_label(int_d$labels$x)
int_d$labels$colour <- vars_label(int_d$labels$colour)

ggarrange(int_a, int_b, int_c, int_d,
          labels = "AUTO",
          font.label = list(size = 20, color = "black", face = "bold"),
          ncol = 2, nrow = 2)

ggsave(filename = file.path("graphs","supp_fig1.pdf"), plot = ggplot2::last_plot(), 
       width = 45, height = 35, dpi = 300, units = "cm", bg = "white")
