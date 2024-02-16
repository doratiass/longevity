# packages --------------------------------------------------------------------
library(tidyverse)
library(shapviz)
library(gridExtra)
library(mgcv)
library(viridis)
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
source("~/Documents/stat_projects/longevity/longevity_shap/dict.R")
set.seed(45)
cat("\f")
potential_interactions(shap_xgb, v = "med_dbp_mean")[1:50]
# data ------------------------------------------------------------------------
int_df <- ml_df %>% 
  drop_na(med_dbp_mean,lab_hematocrit) %>%
  transmute(med_dbp_mean,lab_hematocrit,
            lab_hematocrit_dic = ifelse(lab_hematocrit > 44,"Higher than 44","44 and lower"),
            outcome = factor(outcome,
                             levels = c("not_centenarian", "centenarian")))
shap_xgb_dic <- shap_xgb

shap_xgb_dic$X <- shap_xgb_dic$X %>%
  mutate(lab_hematocrit = ifelse(lab_hematocrit > 44,"Higher than 44","44 and lower")) 

# models ----------------------------------------------------------------------
glm(outcome ~ med_dbp_mean + lab_hematocrit,# + dmg_admission_age + med_smoke_status + med_mi + med_dm,
    data = int_df, family = "binomial") -> glm_no_int

summary(glm_no_int)

plot_txt_no_int <- tidy(glm_no_int) %>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", vars_label(term)),
         inx = 1:n(),
         txt = paste0(term, ", coef ",round(estimate, 2),", p.value ", round(p.value, 3)))

glm(outcome ~ med_dbp_mean * lab_hematocrit,# + dmg_admission_age + med_smoke_status + med_mi + med_dm, 
    data = int_df, family = "binomial") -> glm_int

summary(glm_int)

plot_txt_int <- tidy(glm_int) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                          str_detect(term, ":") ~ "Interaction",
                          TRUE ~ vars_label(term)),
         inx = 1:n(),
         txt = paste0(term, ", coef ",round(estimate, 2),", p.value ", round(p.value, 3)))

# gam(outcome ~ s(med_dbp_mean) + s(lab_hematocrit) +
#       s(med_dbp_mean, lab_hematocrit), data = int_df, family = "binomial") -> gam_no_int
# summary(gam_no_int)
# par(mfrow = c(2, 2))
# plot(gam_no_int)

# plot ------------------------------------------------------------------------
## Pan A ----------------------------------------------------------------------
int_a <- sv_dependence(shap_xgb, v = "lab_hematocrit", 
                       color_var = "lab_hematocrit", 
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
int_b <- sv_dependence(shap_xgb_dic, v = "med_dbp_mean", 
                       color_var = "lab_hematocrit", 
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
int_c <- int_df %>%
  mutate(prob = predict(glm_no_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_dbp_mean, y = prob, color = lab_hematocrit_dic)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(data = plot_txt_no_int, aes(x = 50, y = -5-0.25*inx, color = NULL), shape = 15, size = 3) +
  geom_text(data = plot_txt_no_int, aes(x = 52, y = -5-0.25*inx, label = txt), size = 5,
            color = "black", hjust = 0) +
  scale_y_continuous(limits = c(-6,-1.8)) +
  scale_color_brewer(palette=color_pal) +
  theme_classic() +
  labs(y = "Probebility for Centerianism \n(log odds)",
       color = vars_label("lab_hematocrit")) +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_c$labels$x <- vars_label(int_c$labels$x)
int_c$labels$colour <- vars_label(int_c$labels$colour)
int_c
## Pan D ----------------------------------------------------------------------
int_d <- int_df %>%
  mutate(prob = predict(glm_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_dbp_mean, y = prob, color = lab_hematocrit_dic)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(data = plot_txt_int, aes(x = 50, y = -5-0.25*inx, color = NULL), shape = 15, size = 3) +
  geom_text(data = plot_txt_int, aes(x = 52, y = -5-0.25*inx, label = txt), size = 5,
            color = "black", hjust = 0) +
  scale_y_continuous(limits = c(-6,-1.8)) +
  scale_color_brewer(palette=color_pal) +
  theme_classic() +
  labs(y = "",
       color = vars_label("lab_hematocrit")) +
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
            legend = "top", 
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
