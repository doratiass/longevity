# packages --------------------------------------------------------------------
library(tidyverse)
library(shapviz)
library(gridExtra)
source("~/Documents/stat_projects/longevity/scripts/funcs_def.R")
source("~/Documents/stat_projects/longevity/longevity_shap/dict.R")
set.seed(45)
cat("\f")

# data ------------------------------------------------------------------------
int_df <- ml_df %>% 
  drop_na(med_sbp_mean, dmg_admission_age) %>%
  transmute(outcome = factor(outcome,
                             levels = c("not_centenarian", "centenarian")),
            med_sbp_mean,
            dmg_admission_age = factor(case_when(
              dmg_admission_age <= 46 ~ "46 and younger",
              dmg_admission_age > 58 ~ "59 and older",
              TRUE ~ "47-58"
            ), levels = c("46 and younger","47-58","59 and older"))) 

# models ----------------------------------------------------------------------
glm(outcome ~ med_sbp_mean + dmg_admission_age, data = int_df, family = "binomial") -> glm_no_int
glm(outcome ~ med_sbp_mean * dmg_admission_age, data = int_df, family = "binomial") -> glm_int
summary(glm_no_int)
summary(glm_int)

# plot ------------------------------------------------------------------------
## fig 1 ----------------------------------------------------------------------
leg_size_4 <- 14

int_a <- sv_dependence(shap_xgb, v = "med_sbp_mean", 
                       color_var = "dmg_admission_age", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_a$labels$x <- vars_label(int_a$labels$x)
int_a$labels$colour <- vars_label(int_a$labels$colour)
int_a

## fig 2 ----------------------------------------------------------------------
int_b <- sv_dependence(shap_xgb, v = "dmg_admission_age", 
                       color_var = "dmg_admission_age", 
                       alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_b$labels$x <- vars_label(int_b$labels$x)
int_b$labels$colour <- vars_label(int_b$labels$colour)
int_b

## fig 3 ----------------------------------------------------------------------
int_c <- int_df %>%
  mutate(prob = predict(glm_no_int, data = ., type = "response")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = dmg_admission_age)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

int_c$labels$x <- vars_label(int_c$labels$x)
int_c$labels$colour <- vars_label(int_c$labels$colour)
int_c
## fig 4 ----------------------------------------------------------------------
int_d <- int_df %>%
  mutate(prob = predict(glm_int, data = ., type = "response")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = dmg_admission_age)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  theme_classic() +
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
ggarrange(int_c,int_d,
          labels = c("C", "D"),
          common.legend = TRUE,
          legend = "bottom", 
          font.label = list(size = 20, color = "black", face = "bold"),
          ncol = 2, nrow = 1),
  ncol = 1, nrow = 2)
