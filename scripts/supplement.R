library(tidyverse)
library(ggplot2)

vars <- as.character(unique(c(shap_imp_log$data$feature,
                              shap_imp_lasso$data$feature,
                              shap_imp_bar_xgb$data$feature)))

table_vars <- sort(vars[!(vars %in% c("med_smoke_status_X11.20",
                                 "med_smoke_status_X20."))])
table_vars <- gsub("_other_combined", "", table_vars)

ml_df %>%
  mutate(across(c("comp_SES_4cat", "dmg_num_room"), as.numeric)) %>%
  select("dmg_admission_age","dmg_origin","dmg_martial_status",
         "dmg_num_children","dmg_salary", "dmg_education",
         all_of(table_vars), "med_dm", "outcome") %>%
  rename_all(function(x) sapply(x, label_get,USE.NAMES = FALSE)) %>%
  tbl_summary(by = "outcome",
              type = list(`Socioeconomic Status Category` ~ "continuous",
                          `Number of Occupants in the Household` ~ "continuous",
                          `Number of Rooms at Household` ~ "continuous"),
              value = list(Diabetes ~ "Present",
                           `Myocardial Infarction` ~ "Present"),
              missing = "no")
