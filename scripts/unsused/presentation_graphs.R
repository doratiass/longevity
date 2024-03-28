library(tidyverse)
library(ggstats)
library(shapviz)
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
set.seed(45)
cat("\f")

# EDA plot --------------------------------------------------------------------
cat_plot <- function(data, var) {
  data %>%
    mutate(outcome = factor(ifelse(outcome == "centenarian", "Centenarian", "Not centenarian"),
                            levels = c("Not centenarian","Centenarian"))) %>%
    filter(!is.na({{var}})) %>%
    ggplot(aes(x = {{var}}, fill = outcome, by = outcome, y = after_stat(prop))) +
    geom_bar(stat = "prop", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = vars_label(as.character(substitute(var))),
         y = "",
         fill = "") +
    geom_text(
      mapping = aes(
        label = scales::percent(after_stat(prop), accuracy = .1),
        y = after_stat(0.01)
      ),
      vjust = "bottom",
      position = position_dodge(.9),
      stat = "prop"
    ) +
    theme_classic() +
    plot_theme +
    scale_fill_brewer(palette="Dark2")
}

cont_plot <- function(data, var) {
  mu <- plyr::ddply(data %>%
                      mutate(outcome = factor(ifelse(outcome == "centenarian", "Centenarian", "Not centenarian"),
                                              levels = c("Not centenarian","Centenarian"))), 
                    "outcome", summarise, grp.mean=median({{var}}, na.rm=TRUE))
  
  data %>%
    mutate(outcome = factor(ifelse(outcome == "centenarian", "Centenarian", "Not centenarian"),
                            levels = c("Not centenarian","Centenarian"))) %>%
    filter(!is.na({{var}})) %>%
    ggplot(aes(x = {{var}}, fill = outcome)) +
    geom_density(alpha=0.4)+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=outcome),
               linetype="dashed") + 
    labs(x = vars_label(as.character(substitute(var))),
         y = "",
         fill = "") +
    theme_classic() +
    plot_theme +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2")
}

ggarrange(cat_plot(ml_df, dmg_origin),
          cat_plot(ml_df %>%
                     mutate(dmg_salary = str_replace_all(as.character(dmg_salary),
                                                         "Salary","Salary\n")), dmg_salary),
          cont_plot(ml_df, med_sbp_mean),
          cont_plot(ml_df, lab_mean_cholesterol),
          cat_plot(ml_df, med_smoke_status),
          cat_plot(ml_df, med_mi),
          labels = "AUTO",
          common.legend = TRUE,
          font.label = list(size = 20, color = "black", face = "bold"),
          ncol = 2, nrow = 3)

ggsave(filename = file.path("graphs","pres_eda.pdf"), plot = ggplot2::last_plot(), 
       width = 50, height = 35, dpi = 300, units = "cm", bg = "white")

# PDP plot --------------------------------------------------------------------
deps <- c("med_sbp_mean", "med_dbp_mean","med_bmi_mean",
          "lab_mean_cholesterol","lab_mean_hdl", "lab_glucose",
          "diet_gms_animal_protein_wk","dmg_num_room", "dmg_num_pp_living_together")

sv_dependence(shap_xgb, v = deps, 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE) -> dps

sv_dependence(shap_xgb, v = deps[1:9], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps_int

for (i in 1:length(dps)) {
  dps[[i]]$labels$title <- vars_label(dps[[i]]$labels$title)
  dps[[i]] <- dps[[i]] &
    labs(x = NULL) &
    scale_color_brewer(palette=color_pal) &
    geom_line(data = dps_int[[i]]$data,color = "black", linewidth = 1) &
    plot_theme &
    theme_classic(base_size = 14)
}

dps

ggsave(filename = file.path("graphs","pres_pdp.pdf"), plot = ggplot2::last_plot(), 
       width = 50, height = 35, dpi = 300, units = "cm", bg = "white")
