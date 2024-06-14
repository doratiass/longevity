# packages --------------------------------------------------------------------
library(systemfonts)
library(tidyverse)
library(tidymodels)
library(fastshap)
library(shapviz)
library(pdp)
library(ggtext)
library(ggbump)
library(grid)
tidymodels_prefer()
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
set.seed(45)
cat("\f")

# plot theme ------------------------------------------------------------------
# body_font <- "Gill Sans"
# 
# plot_theme <- theme(
#   text = element_text(family = body_font),
#   plot.title = element_text(size = 25, hjust = 0.5),
#   axis.title = element_text(size = 17),
#   axis.text = element_text(size = 15),
#   legend.text = element_text(size = 15),
#   strip.text = element_text(size = 15))

# data ------------------------------------------------------------------------
labels <- list(
  list("med_dbp_mean",tibble(x_mark = c(80, 84, 93))),
  list("lab_mean_hdl",tibble(x_mark = c(42,55))),
  list("lab_glucose",tibble(x_mark = c(85,100,110,130, 140))),
  list("med_bmi_mean",tibble(x_mark = c(20,25,27.5, 31)))
)

sv_dependence(shap_xgb, v = labels[[1]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE) -> dp_line_1

sv_dependence(shap_xgb, v = labels[[1]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps_int_line_1

sv_dependence(shap_xgb, v = labels[[2]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE) -> dp_line_2

sv_dependence(shap_xgb, v = labels[[2]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps_int_line_2

sv_dependence(shap_xgb, v = labels[[3]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE) -> dp_line_3

sv_dependence(shap_xgb, v = labels[[3]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps_int_line_3

sv_dependence(shap_xgb, v = labels[[4]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = FALSE) -> dp_line_4

sv_dependence(shap_xgb, v = labels[[4]][[1]], 
              color_var = NULL, 
              alpha = 0.5, interactions = TRUE) -> dps_int_line_4
# Plot ------------------------------------------------------------------------
dp_line_1 +
  labs(x = vars_label(labels[[1]][[1]])) +
  scale_color_brewer(palette=color_pal) +
  geom_line(data = dps_int_line_1$data,color = "black", linewidth = 1) +
  geom_segment(
    data = labels[[1]][[2]],
    mapping = aes(x = x_mark, xend = x_mark, 
                  y = min(dp_line_1$data$shap), 
                  yend = max(dp_line_1$data$shap)*0.9),
    linetype = "dashed",
    alpha = 0.4
  ) +
  coord_cartesian(xlim = c(60, 120)) +
  geom_text(
    data = labels[[1]][[2]],
    mapping = aes(x = x_mark, y = max(dp_line_1$data$shap), label = x_mark),
    # family = body_font,
    size = 5
  ) +
  theme_classic(base_size = 14) +
  plot_theme -> mp_1

dp_line_2 +
  labs(x = vars_label(labels[[2]][[1]])) +
  scale_color_brewer(palette=color_pal) +
  geom_line(data = dps_int_line_2$data,color = "black", linewidth = 1) +
  geom_segment(
    data = labels[[2]][[2]],
    mapping = aes(x = x_mark, xend = x_mark, 
                  y = min(dp_line_2$data$shap), 
                  yend = max(dp_line_2$data$shap)*0.9),
    linetype = "dashed",
    alpha = 0.4
  ) +
  coord_cartesian(xlim = c(min(dp_line_2$data$lab_mean_hdl), 75)) +
  geom_text(
    data = labels[[2]][[2]],
    mapping = aes(x = x_mark, y = max(dp_line_2$data$shap), label = x_mark),
    # family = body_font,
    size = 5
  ) +
  theme_classic(base_size = 14) +
  plot_theme -> mp_2

dp_line_3 +
  labs(x = vars_label(labels[[3]][[1]])) +
  scale_color_brewer(palette=color_pal) +
  geom_line(data = dps_int_line_3$data,color = "black", linewidth = 1) +
  geom_segment(
    data = labels[[3]][[2]],
    mapping = aes(x = x_mark, xend = x_mark, 
                  y = min(dp_line_3$data$shap), 
                  yend = max(dp_line_3$data$shap)*0.9),
    linetype = "dashed",
    alpha = 0.4
  ) +
  coord_cartesian(xlim = c(min(dp_line_3$data$lab_glucose), 200)) +
  geom_text(
    data = labels[[3]][[2]],
    mapping = aes(x = x_mark, y = max(dp_line_3$data$shap), label = x_mark),
    # family = body_font,
    size = 5
  ) +
  theme_classic(base_size = 14) +
  plot_theme -> mp_3

dp_line_4 +
  labs(x = vars_label(labels[[4]][[1]])) +
  scale_color_brewer(palette=color_pal) +
  geom_line(data = dps_int_line_4$data,color = "black", linewidth = 1) +
  geom_segment(
    data = labels[[4]][[2]],
    mapping = aes(x = x_mark, xend = x_mark, 
                  y = min(dp_line_4$data$shap), 
                  yend = max(dp_line_4$data$shap)*0.9),
    linetype = "dashed",
    alpha = 0.4
  ) +
  coord_cartesian(xlim = c(min(dp_line_4$data$med_bmi_mean), 35)) +
  geom_text(
    data = labels[[4]][[2]],
    mapping = aes(x = x_mark, y = max(dp_line_4$data$shap), label = x_mark),
    # family = body_font,
    size = 5
  ) +
  theme_classic(base_size = 14) +
  plot_theme -> mp_4

annotate_figure(ggarrange(mp_1 + rremove("y.title"), mp_2 + rremove("y.title"), 
                          mp_3 + rremove("y.title"), mp_4 + rremove("y.title"), 
                          labels = "AUTO",
                          ncol = 2, nrow = 2), 
                left = textGrob("SHAP value", rot = 90, vjust = 1, gp = gpar(cex = 1.8)))

ggsave(filename = file.path("graphs",paste0("fig_3.png")), plot = ggplot2::last_plot(), 
       width = 50, height = 30, dpi = 400, units = "cm", bg = "white")
