df_train %>%
  filter(med_smoke_status %in% c("20+", "never smoked")) %>%
  ggplot(aes(x=lab_glucose)) +
  geom_histogram(binwidth=5, fill="lightblue", color="black") +
  facet_grid(outcome ~ med_smoke_status, scales = "free_y") +
  theme_minimal()
  
df_train %>%
  filter(med_smoke_status %in% c("20+", "never smoked")) %>%
  mutate(lab_glucose = lab_glucose > 100,
         gluc_smoke = case_when(
           lab_glucose == TRUE & med_smoke_status == "20+" ~ "High Glucose, Heavy Smoker",
           lab_glucose == TRUE & med_smoke_status == "never smoked" ~ "High Glucose, Never Smoked",
           lab_glucose == FALSE & med_smoke_status == "20+" ~ "Normal Glucose, Heavy Smoker",
           lab_glucose == FALSE & med_smoke_status == "never smoked" ~ "Normal Glucose, Never Smoked"
         )) %>%
  select(gluc_smoke, outcome) %>%
  gtsummary::tbl_summary(by = outcome) 

df_train %>%
  filter(med_smoke_status %in% c("20+", "1-10")) %>%
  mutate(lab_glucose_b = lab_glucose > 100,
         gluc_smoke = case_when(
           lab_glucose_b == TRUE & med_smoke_status == "20+" ~ "High Glucose, Heavy Smoker",
           lab_glucose_b == TRUE & med_smoke_status == "1-10" ~ "High Glucose, Never Smoked",
           lab_glucose_b == FALSE & med_smoke_status == "20+" ~ "Normal Glucose, Heavy Smoker",
           lab_glucose_b == FALSE & med_smoke_status == "1-10" ~ "Normal Glucose, Never Smoked"
         )) %>%
  drop_na(gluc_smoke) -> a

df_train %>%
  filter(med_smoke_status %in% c("20+", "1-10")) %>%
  mutate(lab_glucose_b = lab_glucose > 100,
         gluc_smoke = case_when(
           lab_glucose_b == TRUE & med_smoke_status == "20+" ~ "High Glucose, Heavy Smoker",
           lab_glucose_b == TRUE & med_smoke_status == "1-10" ~ "High Glucose, Never Smoked",
           lab_glucose_b == FALSE & med_smoke_status == "20+" ~ "Normal Glucose, Heavy Smoker",
           lab_glucose_b == FALSE & med_smoke_status == "1-10" ~ "Normal Glucose, Never Smoked"
         )) %>%
  drop_na(gluc_smoke) %>%
 mutate(#
   lab_glucose = ifelse(lab_glucose > 100, 80, 110),
   #med_smoke_status = ifelse(med_smoke_status == "20+", "1-10", "20+")
   ) -> b

a_data <- bake(xgb_prep,
                      has_role("predictor"),
                      new_data = a,
                      composition = "matrix")

b_data <- bake(xgb_prep,
               has_role("predictor"),
               new_data = b,
               composition = "matrix")

tibble(
  group = a$gluc_smoke,
  p_a = predict(extract_fit_engine(final_xgb_fit), a_data, outputmargin=TRUE),
  p_b = predict(extract_fit_engine(final_xgb_fit), b_data, outputmargin=TRUE),
  diff = p_b-p_a) %>%
  group_by(group) %>%
  summarise(
    mean_diff = mean(diff),
    sd_diff = sd(diff))
  print(n=25)

  [115] "med_smoke_status_X11.20"                            
  [116] "med_smoke_status_X20."                              
  [117] "med_smoke_status_ex.smoker"                         
  [118] "med_smoke_status_never.smoked"   
  
  
  glm(outcome ~ med_smoke_status + lab_glucose,#+ lab_glucose*med_smoke_status, 
      data = df_train %>% filter(med_smoke_status %in% c("20+", "never smoked")) %>%
        mutate(outcome = ifelse(outcome == "not_centenarian", 0, 1),
               med_smoke_status =  factor(med_smoke_status,
                                          levels = c( "never smoked","20+"))),
      family = "binomial") %>%
    summary()


  tibble(
    gluc = c(seq(60, 230, 5), seq(60, 230, 5)),
    smoke = rep(c(0,1),length(gluc)/2),
    p = 1/(1+exp(-(-1.304353-0.012080*gluc - 0.959154*smoke))),
    p_int = 1/(1+exp(-(-1.156964-0.013811*gluc - 1.494574*smoke + 0.006319*gluc*smoke)))
  ) %>%
    pivot_longer(cols = c(p, p_int), names_to = "outcome", values_to = "p") %>%
    ggplot(aes(x=gluc, y=p, color=factor(smoke))) +
    geom_line() +
    facet_grid(outcome~., scales = "free_y")
    