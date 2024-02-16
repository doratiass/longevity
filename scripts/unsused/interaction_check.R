potential_interactions(shap_xgb, v = "med_sbp_mean")[1:30]

sv_dependence(shap_xgb, v = "lab_mean_hdl", 
              color_var = "med_smoke_status_X20.", 
              alpha = 0.5, interactions = TRUE)  +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2))

df_train %>% 
  filter(#med_sbp_mean < 150,
         med_smoke_status == "20+") -> sample_sbp_low_smoke_20

sample_sbp_low_smoke_20 %>%
  mutate(
     med_smoke_status = "never smoked",
    #med_sbp_mean = 220
  ) -> sample_sbp_high_smoke_20


sample_sbp_low_smoke_20_bake <- bake(xgb_prep,
                                     has_role("predictor"),
                                     new_data = sample_sbp_low_smoke_20,
                                     composition = "matrix")
sample_sbp_high_smoke_20_bake <- bake(xgb_prep,
                                      has_role("predictor"),
                                      new_data = sample_sbp_high_smoke_20,
                                      composition = "matrix")

tibble(
  smoke = "20",
  sbp = sample_sbp_low_smoke_20$med_sbp_mean,
  sbp_low = predict(extract_fit_engine(final_xgb_fit), sample_sbp_low_smoke_20_bake, type = "prob"), 
  sbp_high = predict(extract_fit_engine(final_xgb_fit), sample_sbp_high_smoke_20_bake, type = "prob"),
  diff = sbp_high - sbp_low) -> diff_20

df_train %>% 
  filter(#med_sbp_mean < 150,
         med_smoke_status == "never smoked") -> sample_sbp_low_smoke_never

sample_sbp_low_smoke_never %>%
  mutate(
     med_smoke_status = "20+",
    #med_sbp_mean = 220
  ) -> sample_sbp_high_smoke_never


sample_sbp_low_smoke_never_bake <- bake(xgb_prep,
                                     has_role("predictor"),
                                     new_data = sample_sbp_low_smoke_never,
                                     composition = "matrix")

sample_sbp_high_smoke_never_bake <- bake(xgb_prep,
                                      has_role("predictor"),
                                      new_data = sample_sbp_high_smoke_never,
                                      composition = "matrix")

tibble(
  smoke = "never",
  sbp = sample_sbp_low_smoke_never$med_sbp_mean,
  sbp_low = predict(extract_fit_engine(final_xgb_fit), sample_sbp_low_smoke_never_bake, type = "prob"), 
  sbp_high = predict(extract_fit_engine(final_xgb_fit), sample_sbp_high_smoke_never_bake, type = "prob"),
  diff = sbp_high - sbp_low) -> diff_never

bind_rows(diff_20, diff_never) %>%
  ggplot(aes(x = sbp, y = diff, color = smoke)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  plot_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = leg_size_4),
        legend.text = element_text(size = leg_size_4-2)) +
  labs(title = "Effect of smoking on probability",
       x = "SBP",
       y = "Change in probability")


ml_df %>% 
  mutate(outcome = factor(outcome,
                          levels = c("not_centenarian", "centenarian"))) %>%
  #filter(med_smoke_status %in% c("20+","never smoked")) %>%
  glm(outcome ~ med_sbp_mean * med_mi, data = ., family = "binomial") -> glm_chol_int

ml_df %>% 
  mutate(outcome = factor(outcome,
                          levels = c("not_centenarian", "centenarian"))) %>%
 # filter(med_smoke_status %in% c("20+","never smoked")) %>%
  glm(outcome ~ med_sbp_mean + med_mi, data = ., family = "binomial") -> glm_chol

ml_df %>%
 # filter(med_smoke_status %in% c("20+","never smoked")) %>%
  drop_na(med_sbp_mean, med_mi) %>%
  transmute(
    med_sbp_mean, med_mi,
    p = predict(glm_chol, data = ., type = "response"),
    p_int = predict(glm_chol_int, data = ., type = "response")
  ) %>%
  pivot_longer(cols = c(p, p_int), names_to = "model", values_to = "prob") %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = med_mi, linetype = model)) +
  geom_line()












