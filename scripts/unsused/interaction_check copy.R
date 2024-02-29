# packages --------------------------------------------------------------------
library(tidyverse)
library(shapviz)
library(gridExtra)
library(mgcv)
library(tidygam)
library(viridis)
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
source("~/Documents/stat_projects/longevity/longevity_shap/dict.R")
set.seed(45)
cat("\f")

# data ------------------------------------------------------------------------
#potential_interactions(shap_xgb, v = "med_sbp_mean")[1:50]
int_df <- ml_df %>% 
  filter(med_smoke_status %in% c("never smoked", "20+")) %>%
  drop_na(med_sbp_mean,med_smoke_status) %>%
  transmute(med_sbp_mean,med_smoke_status,
            med_smoke_status_dic = factor(
              case_when(med_smoke_status %in% c("never smoked") ~ "non-smoker", 
                        # med_smoke_status%in% c("ex-smoker",
                        #                        "1-10")~ "low-smoker",
                        TRUE ~ "smoker"),
              levels = c("non-smoker", "low-smoker","smoker")),
            outcome = factor(outcome,
                             levels = c("not_centenarian", "centenarian")))
shap_xgb_dic <- shap_xgb

shap_xgb_dic$X <- shap_xgb_dic$X %>%
  mutate(med_smoke_status = factor(case_when(
    med_smoke_status_X20. == 1 ~ "smoker",
    med_smoke_status_X11.20 == 1 ~ "smoker",
    med_smoke_status_X1.10 == 1 ~ "smoker",
    med_smoke_status_ex.smoker == 1 ~ "smoker",
    TRUE ~ "non-smoker"
  )))
# models ----------------------------------------------------------------------
glm(outcome ~ med_sbp_mean + med_smoke_status,# + dmg_admission_age + med_smoke_status + med_mi + med_dm,
    data = int_df, family = "binomial") -> glm_no_int

summary(glm_no_int)

plot_txt_no_int <- tidy(glm_no_int) %>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", vars_label(term)),
         inx = 1:n(),
         txt = paste0(term, ", coef ",round(estimate, 2),", p.value ", round(p.value, 3)))

glm(outcome ~ med_sbp_mean * med_smoke_status,# + dmg_admission_age + med_smoke_status + med_mi + med_dm, 
    data = int_df, family = "binomial") -> glm_int

summary(glm_int)

plot_txt_int <- tidy(glm_int) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                          str_detect(term, ":") ~ "Interaction",
                          TRUE ~ vars_label(term)),
         inx = 1:n(),
         txt = paste0(term, ", coef ",round(estimate, 2),", p.value ", round(p.value, 3)))

gam(outcome ~ s(med_sbp_mean) + med_smoke_status_dic, data = int_df, family = "binomial") -> gam_no_int
plot_txt_no_int <- tidy(gam_no_int) %>%
  mutate(term = vars_label(str_remove_all(term, "s\\(|\\)")),
         inx = 1:n(),
         txt = paste0(term, ", edf ",round(edf, 2),", p.value ", round(p.value, 3)))
summary(gam_no_int)
par(mfrow = c(2, 2))
plot(gam_no_int)

# gam(outcome ~ s(med_sbp_mean) + s(med_smoke_status) +
#       ti(med_sbp_mean, med_smoke_status), data = int_df, family = "binomial") -> gam_int
gam(outcome ~ s(med_sbp_mean) + med_smoke_status_dic +
      s(med_sbp_mean, by = med_smoke_status_dic), data = int_df, family = "binomial") -> gam_int
plot_txt_int <- tidy(gam_int) %>%
  mutate(term = str_remove_all(term, "s\\(|\\)"),
         term = case_when(str_detect(term, ",") ~ "Interaction",
                          TRUE ~ vars_label(term)),
         inx = 1:n(),
         txt = paste0(term, ", edf ",round(edf, 2),", p.value ", round(p.value, 3)))
summary(gam_int)
par(mfrow = c(2, 2))
plot(gam_int)

gratia::draw(gam_int)
sm <- gratia::smooth_estimates(gam_int, smooth = "ti(med_sbp_mean, med_smoke_status)", dist = 0.1)
ggplot(sm, aes(x = x, y = y)) +
  geom_raster(aes(fill = est)) +
  geom_point(data = df, alpha = 0.2) + # add a point layer for original data
  scale_fill_viridis_c(option = "plasma")

gam_pred <- predict_gam(gam_int, 
                         values = list(med_sbp_mean = seq(60, 120, length.out = 100),
                                       med_smoke_status = seq(15, 70, length.out = 100))
                        #med_smoke_status = c(20,30,40, 45, 50, 55)),
                        # tran_fun = function(x) {exp(x)/(1+exp(x))}
                        ) 
gam_pred %>%
  ggplot(aes(x = med_sbp_mean, y = outcome)) +
  geom_point(aes(color = med_smoke_status)) +
  geom_smooth(color = "gray30") +
  scale_color_viridis_c(option = "plasma")  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

gam_pred %>%
  ggplot(aes(x = med_smoke_status, y = outcome)) +
  geom_point(aes(color = med_sbp_mean)) +
  geom_smooth(color = "gray30") +
  scale_color_viridis_c(option = "plasma")  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))


gam_pred %>%
  ggplot(aes(med_sbp_mean, med_smoke_status, z = outcome)) +
  geom_raster(aes(fill = outcome)) +
  scale_fill_viridis_c(option = "plasma")  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

# gam_pred %>%
#   plot_ly(x= ~med_sbp_mean, y= ~med_smoke_status, z= ~outcome, intensity = ~outcome, type = "mesh3d")
# plot ------------------------------------------------------------------------
## Pan A ----------------------------------------------------------------------
int_a <- sv_dependence(shap_xgb, v = "med_sbp_mean", 
                       color_var = "med_sbp_mean", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_a$labels$x <- vars_label(int_a$labels$x)
int_a$labels$colour <- vars_label(int_a$labels$colour)
int_a

## Pan B ----------------------------------------------------------------------
int_b <- sv_dependence(shap_xgb_dic, v = "med_sbp_mean", 
                       color_var = "med_smoke_status_X20.", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_b$labels$x <- vars_label(int_b$labels$x)
int_b$labels$colour <- vars_label(int_b$labels$colour)
int_b

## Pan C ----------------------------------------------------------------------
x_start <- 90
int_c <- int_df %>%
  mutate(prob = predict(gam_no_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = med_smoke_status_dic)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(data = plot_txt_no_int, aes(x = x_start, y = -5-0.25*inx, color = NULL), shape = 15, size = 3) +
  geom_text(data = plot_txt_no_int, aes(x = x_start + 2, y = -5-0.25*inx, label = txt), size = 5,
            color = "black", hjust = 0) +
  scale_y_continuous(limits = c(-6,-1.8)) +
  scale_color_viridis_d(begin = 0.25, end = 0.85, option = "inferno") +
  theme_classic() +
  labs(y = "Probebility for Centerianism \n(log odds)",
       color = vars_label("med_smoke_status")) +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_c$labels$x <- vars_label(int_c$labels$x)
int_c$labels$colour <- vars_label(int_c$labels$colour)
int_c
## Pan D ----------------------------------------------------------------------
int_d <- int_df %>%
  mutate(prob = predict(gam_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = med_smoke_status_dic)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(data = plot_txt_int, aes(x = x_start, y = -5-0.25*inx, color = NULL), shape = 15, size = 3) +
  geom_text(data = plot_txt_int, aes(x = x_start + 2, y = -5-0.25*inx, label = txt), size = 5,
            color = "black", hjust = 0) +
  scale_y_continuous(limits = c(-6,-1.8)) +
  scale_color_viridis_d(begin = 0.25, end = 0.85, option = "inferno") +
  theme_classic() +
  labs(y = "",
       color = vars_label("med_smoke_status")) +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_d$labels$x <- vars_label(int_d$labels$x)
int_d$labels$colour <- vars_label(int_d$labels$colour)
int_d
## combine --------------------------------------------------------------------
ggarrange(
  ggarrange(int_a, int_b,
            labels = c("A", "B"),
            common.legend = TRUE,
            legend = "none", 
            font.label = list(size = 20, color = "black", face = "bold"),
            ncol = 2, nrow = 1),
  ggarrange(int_c,int_d + rremove("y.axis") + 
              rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
            labels = c("C", "D"),
            common.legend = TRUE,
            legend = "bottom", 
            font.label = list(size = 20, color = "black", face = "bold"),
            ncol = 2, nrow = 1),
  ncol = 1, nrow = 2)

ggsave(filename = file.path("graphs","fig4.pdf"), plot = ggplot2::last_plot(), 
       width = 40, height = 30, dpi = 300, units = "cm", bg = "white")
