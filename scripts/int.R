library(haven)
library(tidyverse)
library(lubridate)
library(Rfast)
library(gtExtras)

# Functions ####
post_issue_dor <- function(title, body = "") {
  gh::gh(
    endpoint = "POST /repos/doratiass/iihd/issues",
    title = title,
    body = body
  )
}

post_issue_saar <- function(title, body = "") {
  gh::gh(
    endpoint = "POST /repos/saar104/iihd/issues",
    title = title,
    body = body
  )
}

check_two_vars <- function(var_1, var_2, df = raw_data) {
  df %>% 
    select((!!sym(var_1)), (!!sym(var_2))) %>% 
    mutate(eq = (!!sym(var_1)) == (!!sym(var_2))) %>% 
    group_by(eq,(!!sym(var_1)), (!!sym(var_2))) %>% 
    summarise(n = n())
}


# Initial data clean ####
raw_data <- read_dta("raw_data/New_42_yr_but_Shortened_DEC2022.dta")
raw_data[] <- lapply(raw_data, function(x) { attributes(x) <- NULL; x })

clean_df <- raw_data %>%
  transmute(
    id_nivdaki = nivdaki,
    # id_zehut = zehut,
    # id_id = id, # TODO same?
    exam_date_63 = make_date(Year_exam, Month_exam), 
    death_date = make_date(1900+shana, hodesh),
    dmg_admission_age = age,
    lab_nonhdl_63 = nonhdl,
    dmg_ethnic = factor(case_when(
      area == 1 ~ "Israel",
      area == 2 ~ "Europe",#"East Europe",
      area == 3 ~ "Europe",#"Central Europe",
      area == 4 ~ "Balkan",
      area == 5 ~ "Middle East & North Africa",#"Middle East",
      area == 6 ~ "Middle East & North Africa"#"North Africa"
    )),
    dmg_birth_date = as.Date(Exact_birth, origin = "1970-01-01"),
    dmg_birth_year_1 = birthyr,
    dmg_birth_year_2 = Year_Birth, 
    #  dmg_birth_country = factor(ifelse(country == -2, "Israel", "Out of Israel")), 
    dmg_immigration_year = case_when(
      immigyr < 0  ~ NA,
      TRUE ~ immigyr
    ), 
    dmg_martial_status = factor(case_when(
      famstat == 1 ~ "Remarried",
      famstat == 2 ~ "Married Once",
      famstat == 3 ~ "Seperated",
      famstat == 4 ~ "Widower",
      famstat == 5 ~ "Bachelor"
    )),
    dmg_num_children = ifelse(Number_children == -1, NA, Number_children), 
    dmg_num_pp_living_together = case_when(
      Number_live_together < 0  ~ NA,
      TRUE ~ Number_live_together
    ),
    dmg_num_room = case_when(
      Number_roomd < 0  ~ NA,
      TRUE ~ Number_roomd
    ),
    dmg_kibbutz = Number_roomd == -3 | Number_live_together == -3,
    dmg_work_grade = factor(case_when(
      Work_grade == 0 ~ "Administrative",
      Work_grade == 1 ~ "Professional",
      Work_grade == 2 ~ "Laborer",
      Work_grade == 3 ~ "Special",
      Work_grade == 4 ~ "Technicians",
      Work_grade == 8 ~ "Teachers",
    )),
    dmg_salary = factor(case_when(
      Salari == 1 ~ "Basic Salary 345+",
      Salari == 2 ~ "Basic Salary 201-344",
      Salari == 3 ~ "Basic Salary 135-200",
      Salari == 4 ~ "Basic Salary 100-134",
      Salari == 5 ~ "Apprentice 90",
      Salari == 6 ~ "Special",
      Salari == 7 ~ "Unknown",
      Salari == 8 ~ "Teachers"
    )), 
    dmg_education = factor(case_when(
      educ == 1 ~ "No formal school",
      educ == 2 ~ "Partial Elementary",
      educ == 3 ~ "Full Elementary",
      educ == 4 ~ "Partial High School",
      educ == 5 ~ "Full High School",
      educ == 6 ~ "Seminar - no bagrut",
      educ == 7 ~ "Seminar - with bagrut",
      educ == 8 ~ "Partial University",
      educ == 9 ~ "Full University",
    ), ordered = TRUE), 
    dmg_wife_education = factor(case_when(
    #  wifeedc == 0 ~ "No Wife",
      wifeedc == 1 ~ "No formal school",
      wifeedc == 2 ~ "Partial Elementary",
      wifeedc == 3 ~ "Full Elementary",
      wifeedc == 4 ~ "Partial High School",
      wifeedc == 5 ~ "Full High School",
      wifeedc == 6 ~ "Seminar - no bagrut",
      wifeedc == 7 ~ "Seminar - with bagrut",
      wifeedc == 8 ~ "Partial University",
      wifeedc == 9 ~ "Full University",
    ), ordered = TRUE), 
    dmg_wife_occupation = factor(case_when(
      q19 == 0 ~ "No Wife",
      q19 == 1 ~ "Housewife",
      q19 == 2 ~ "Additional Work At Home",
      q19 == 3 ~ "Work Outside Home",
    ), ordered = TRUE),
    dmg_wife_work_outside = factor(case_when(
      q20 == 0 ~ "Housewife or No Wife",
      q20 == 1 ~ "1-9",
      q20 == 2 ~ "10-19",
      q20 == 3 ~ "20-39",
      q20 == 4 ~ "40 or more"
    ), ordered = TRUE),
    med_weight_admission = weight,
    med_height_admission = height,
    med_bmi_admission = med_weight_admission / ((med_height_admission/100)^2), 
    med_right_arm_circum_cm = q25,
    med_triceps_thickness_mm = triceps,
    med_subscapular_thickness_mm = Subscapular,
    med_past_MI = Previous_Hattack_360men,
    med_past_peptic = ifelse(peptic == 1, TRUE, FALSE),
    med_past_angina_pectoris = factor(case_when(
      ap == 1 ~ "Definite",
      ap == 2 ~ "Suspect",
      ap == 3 ~ "Functional Pain",
      ap == 4 ~ "None of the above",
      ap == 5 ~ "Functional Pain (II)",
      ap == 6 ~ "Not Functional Pain",
      ap == 9 ~ "No diagnosis"
    )), 
    med_past_intermittent_claudication = factor(case_when(
      intcld == 1 ~ "IC",
      intcld == 2 ~ "Negative",
      intcld == 9 ~ "Multiple answers",
    )),
    med_smoke_63 = factor(case_when(
      smok63 == 1 ~ "1-10",
      smok63 == 2 ~ "11-20",
      smok63 == 3 ~ "20+",
      smok63 == 4 ~ "5 pipes or more",
      smok63 == 5 ~ "less than 5 pipes",
      smok63 == 6 ~ "ex-smoker",
      smok63 == 7 ~ "never smoked"
    )),
    med_sbp_63 = sbp63,
    med_dbp_63 = dbp63,
    med_puls = factor(case_when(
      pulse == 0 ~ "<50",
      pulse == 1 ~ "51-60",
      pulse == 2 ~ "61-70",
      pulse == 3 ~ "71-80",
      pulse == 4 ~ "81-90",
      pulse == 5 ~ "91-100",
      pulse == 6 ~ "101-110",
      pulse == 7 ~ "111-120",
      pulse == 8 ~ "121-130",
      pulse == 9 ~ "131-140",
      pulse == 10 ~ ">141",
    ), ordered = TRUE, 
    levels = c("<50","51-60","61-70","71-80","81-90","91-100",
               "101-110","111-120","121-130","131-140",">141")),
    med_peripheral_art_dis = factor(case_when(
      perifhd == 1 ~ "Definite",
      perifhd == 2 ~ "Suspect",
      perifhd == 3 ~ "None",
    )),
    med_anxiety = anxiety,
    lab_cholesterol_63 = cholesterol63,
    lab_hdl_63 = HDL_1963,
    lab_uacid_63 = uacid63,
    lab_glucose_63 = BloodGlucose1963,
    lab_hemoglobin_63 = Hemoglobin_1963,
    lab_hematocrit_63 = Hematocrit_1963,
    med_varified_MI = ifelse(verified_Heart_Attack == 1, TRUE, FALSE), 
    physical_activity_work = factor(case_when(
      physwork == 1 ~ "Mainly Sitting",#"Mainly Sitting at Desk",
      physwork == 2 ~ "Mainly Sitting",#"Mainly Sitting at car",
      physwork == 3 ~ "Mainly Standing/Walking",#"Mainly Standing",
      physwork == 4 ~ "Mainly Standing/Walking",#"Mainly Walking",
      physwork == 5 ~ "Physical Work",
      physwork == 6 ~ "Does not work",
      physwork == 7 ~ "Other activity",
    )),
    physical_activity_after_work = factor(case_when(
      leisure == 1 ~ "Almost None",
      leisure == 2 ~ "Sporadic",
      leisure == 3 ~ "Light",
      leisure == 4 ~ "Energetic",
    )),
    med_smoke_65 = factor(case_when(
      smok65 == 1 ~ "1-10",
      smok65 == 2 ~ "11-20",
      smok65 == 3 ~ "20+",
      smok65 == 4 ~ "5 pipes or more",
      smok65 == 5 ~ "less than 5 pipes",
      smok65 == 6 ~ "never smoked",
      smok65 == 7 ~ "ex-smoker"
    )),
    med_sbp_65 = sbp65,
    med_dbp_65 = dbp65,
    lab_glucose_65 = BloodGlucose1965,
    lab_cholesterol_65 = cholesterol65,
    lab_hdl_65 = hdl65,
    lab_nonhdl_65 = nonhdl65,
    lab_blood_group = factor(case_when(
      blood == 1 ~ "A",#"A1",
      blood == 2 ~ "A",#"A2",
      blood == 3 ~ "A",#"A unspecified",
      blood == 4 ~ "AB",#"A1B",
      blood == 5 ~ "AB",#"A2B",
      blood == 6 ~ "AB",#"AB unspecified",
      blood == 7 ~ "B",
      blood == 8 ~ "O",
     # blood == 9 ~ "Unkown",
    )),
    med_sbp_68 = sbp68,
    med_dbp_68 = dbp68,
    med_vital_capacity_index = vc_index, #page 28 old doc
    med_fev = fev,
    work_past_finance_trouble = factor(case_when(
      pafinan == 1 ~ "Very Serious",
      pafinan == 2 ~ "Serious",
      pafinan == 3 ~ "Not Serious",
      pafinan == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_present_finance_trouble = factor(case_when(
      prfinan == 1 ~ "Very Serious",
      prfinan == 2 ~ "Serious",
      prfinan == 3 ~ "Not Serious",
      prfinan == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    family_past_finance_trouble = factor(case_when(
      pafam == 1 ~ "Very Serious",
      pafam == 2 ~ "Serious",
      pafam == 3 ~ "Not Serious",
      pafam == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    family_present_finance_trouble = factor(case_when(
      prfam == 1 ~ "Very Serious",
      prfam == 2 ~ "Serious",
      prfam == 3 ~ "Not Serious",
      prfam == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    family_conflict_with_wife = factor(case_when(
      conflict_with_wife == 1 ~ "Always show it",
      conflict_with_wife == 2 ~ "Generally show it",
      conflict_with_wife == 3 ~ "Generally keep to himself",
      conflict_with_wife == 4 ~ "Always keep to himself",
    ), ordered = TRUE, levels = c("Always show i", "Generally show it", 
                                  "Generally keep to himself","Always keep to himself")),
    family_hurt_by_wife_child_forget = factor(case_when(
      Hurt_by_wife_or_child_forget == 1 ~ "Usually Forget",
      Hurt_by_wife_or_child_forget == 2 ~ "Tend to Forget",
      Hurt_by_wife_or_child_forget == 3 ~ "Tend to Brood",
      Hurt_by_wife_or_child_forget == 4 ~ "Usually Brood",
      #     Hurt_by_wife_or_child_forget == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Usually Forget", "Tend to Forget", 
                                  "Tend to Brood","Usually Brood")),
    family_hurt_by_wife_child_retaliate = factor(case_when(
      Hurt_by_wife_or_child_retaliate == 1 ~ "Very Often",
      Hurt_by_wife_or_child_retaliate == 2 ~ "Sometimes",
      Hurt_by_wife_or_child_retaliate == 3 ~ "Seldom",
      Hurt_by_wife_or_child_retaliate == 4 ~ "Never",
      #   Hurt_by_wife_or_child_retaliate == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    family_hurt_by_wife_child_restrain_retaliate = factor(case_when(
      q48 == 1 ~ "Very Often",
      q48 == 2 ~ "Sometimes",
      q48 == 3 ~ "Seldom",
      q48 == 4 ~ "Never",
      #      q48 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    family_wife_shows_love = factor(case_when(
      wife_shows_life == 1 ~ "Shows it often",
      wife_shows_life == 2 ~ "Shows it seldom",
      wife_shows_life == 3 ~ "Not as much as Id like",
      wife_shows_life == 4 ~ "Never shows it",
      wife_shows_life == 5 ~ "Doesnt love me",
   #   wife_shows_life == 6 ~ "No wife",
    ), ordered = TRUE),
    family_wife_children_listen = factor(case_when(
      Wife_children_listen == 1 ~ "Always",
      Wife_children_listen == 2 ~ "Usually",
      Wife_children_listen == 3 ~ "Sometimes",
      Wife_children_listen == 4 ~ "Never",
      #     Wife_children_listen == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Always", "Usually", 
                                  "Sometimes","Never")),
    family_affected_by_family_not_listen = factor(case_when(
      Family_not_listening == 1 ~ "Very Upset",
      Family_not_listening == 2 ~ "Little Upset",
      Family_not_listening == 3 ~ "Not Affected",
      Family_not_listening == 4 ~ "Never Happens",
      #      Family_not_listening == -4 ~ "Refused to Answer",
    )),
    work_past_work_probs = factor(case_when(
      past_work_probs == 1 ~ "Very Serious",
      past_work_probs == 2 ~ "Serious",
      past_work_probs == 3 ~ "Not Serious",
      past_work_probs == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_present_work_probs = factor(case_when(
      present_work_probs == 1 ~ "Very Serious",
      present_work_probs == 2 ~ "Serious",
      present_work_probs == 3 ~ "Not Serious",
      present_work_probs == 4 ~ "None",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_try_improve = factor(case_when(
      q54 == 1 ~ "Certain to succeed",
      q54 == 2 ~ "Not certain to succeed",
      q54 == 3 ~ "Not trying - no change",
      q54 == 4 ~ "Not trying - satisfied",
      #     q54 == -4 ~ "Refused to Answer",
    )),
    work_coworker_like = factor(case_when(
      q55 == 1 ~ "Very often",
      q55 == 2 ~ "Sometimes",
      q55 == 3 ~ "Not as much as Id like",
      q55 == 4 ~ "Never",
      q55 == 5 ~ "Dont like me",
      #      q55 == -4 ~ "Refused to Answer",
    )),
    work_supervisor_appreciate = factor(case_when(
      q56 == 1 ~ "Very often",
      q56 == 2 ~ "Sometimes",
      q56 == 3 ~ "Not as much as Id like",
      q56 == 4 ~ "Never",
      q56 == 5 ~ "Dont like me",
      #      q56 == -4 ~ "Refused to Answer",
    )),
    work_past_coworker_probs = factor(case_when(
      q57 == 1 ~ "Very Serious",
      q57 == 2 ~ "Serious",
      q57 == 3 ~ "Not Serious",
      q57 == 4 ~ "None",
      #   q57 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_present_coworker_probs = factor(case_when(
      q58 == 1 ~ "Very Serious",
      q58 == 2 ~ "Serious",
      q58 == 3 ~ "Not Serious",
      q58 == 4 ~ "None",
      #    q58 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_past_superior_probs = factor(case_when(
      q59 == 1 ~ "Very Serious",
      q59 == 2 ~ "Serious",
      q59 == 3 ~ "Not Serious",
      q59 == 4 ~ "None",
      #     q59 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_present_superior_probs = factor(case_when(
      q60 == 1 ~ "Very Serious",
      q60 == 2 ~ "Serious",
      q60 == 3 ~ "Not Serious",
      q60 == 4 ~ "None",
      #     q60 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Serious", "Serious", "Not Serious","None")),
    work_hurt_by_coworker_forget = factor(case_when(
      q61 == 1 ~ "Usually Forget",
      q61 == 2 ~ "Tend to Forget",
      q61 == 3 ~ "Tend to Brood",
      q61 == 4 ~ "Usually Brood",
      #      q61 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Usually Forget", "Tend to Forget", 
                                  "Tend to Brood","Usually Brood")),
    work_hurt_by_superior_forget = factor(case_when(
      q62 == 1 ~ "Usually Forget",
      q62 == 2 ~ "Tend to Forget",
      q62 == 3 ~ "Tend to Brood",
      q62 == 4 ~ "Usually Brood",
      #     q62 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Usually Forget", "Tend to Forget", 
                                  "Tend to Brood","Usually Brood")),
    work_hurt_by_coworker_shout = factor(case_when(
      q63 == 1 ~ "Very Often",
      q63 == 2 ~ "Sometimes",
      q63 == 3 ~ "Seldom",
      q63 == 4 ~ "Never",
      #     q63 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    work_hurt_by_superior_shout = factor(case_when(
      q64 == 1 ~ "Very Often",
      q64 == 2 ~ "Sometimes",
      q64 == 3 ~ "Seldom",
      q64 == 4 ~ "Never",
      #     q64 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    work_hurt_by_coworker_restrain_retaliate = factor(case_when(
      q65 == 1 ~ "Very Often",
      q65 == 2 ~ "Sometimes",
      q65 == 3 ~ "Seldom",
      q65 == 4 ~ "Never",
      #     q65 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    work_hurt_by_superior_restrain_retaliate = factor(case_when(
      q66 == 1 ~ "Very Often",
      q66 == 2 ~ "Sometimes",
      q66 == 3 ~ "Seldom",
      q66 == 4 ~ "Never",
      #     q66 == -4 ~ "Refused to Answer",
    ), ordered = TRUE, levels = c("Very Often", "Sometimes", 
                                  "Seldom","Never")),
    work_closed_person = factor(case_when(
      Closed_person == 1 ~ "Yes",
      Closed_person == 2 ~ "Generally yes",
      Closed_person == 3 ~ "Somtimes talk",
      Closed_person == 4 ~ "Often talk",
      #    Closed_person == -4 ~ "Refused to Answer"
    )),
    family_prob_reach_living_standart = factor(case_when( 
      Reach_living_Standards == 1 ~ "Very many",
      Reach_living_Standards == 2 ~ "Many",
      Reach_living_Standards == 3 ~ "Some",
      Reach_living_Standards == 4 ~ "None",
      #      Reach_living_Standards == -4 ~ "Refused to Answer"
    )),
    family_fight_injustice = factor(case_when(
      DoYou_Fight_justice == 1 ~ "Yes",
      DoYou_Fight_justice == 1 ~ "Cant do Anything",
      DoYou_Fight_justice == 1 ~ "Not personally concerned",
      DoYou_Fight_justice == 1 ~ "Never came across it",
      #    DoYou_Fight_justice == -4 ~ "Refused to Answer"
    )),
    diet_type = factor(case_when(
      on_Diet_Which_kind == 0 ~ "Not On Diet",
      on_Diet_Which_kind == 1 ~ "Slimming",
      on_Diet_Which_kind == 2 ~ "CHD",
      on_Diet_Which_kind == 3 ~ "HTN",
      on_Diet_Which_kind == 4 ~ "DM",
      on_Diet_Which_kind == 5 ~ "Ulcer",
      on_Diet_Which_kind == 6 ~ "Fat Reduction",
      on_Diet_Which_kind == 7 ~ "7",
      on_Diet_Which_kind == 8 ~ "8" #TODO need to complete values Vol 2 p 129
    )),
    diet_after_main_meal_activity = factor(After_main_mail #TODO complete
                                           # case_when(
                                           # After_main_mail == 1 ~ "??",
                                           # After_main_mail == 2 ~ "??",
                                           # After_main_mail == 3 ~ "??",
                                           # After_main_mail == 4 ~ "??",
                                           # After_main_mail == 5 ~ "??",
                                           # After_main_mail == 6 ~ "??",
                                           # After_main_mail == 7 ~ "??",
                                           # After_main_mail == 8 ~ "??",
                                           # After_main_mail == 9 ~ "??",
                                           # After_main_mail == 10 ~ "Previously code X") #TODO need to complete values Vol 2 p 129 + what is code X?
    ),
    diet_after_second_main_meal_activity = factor(After_second_mail #TODO complete
                                                  # case_when(
                                                  # After_second_mail == 1 ~ "??",
                                                  # After_second_mail == 2 ~ "??",
                                                  # After_second_mail == 3 ~ "??",
                                                  # After_second_mail == 4 ~ "??",
                                                  # After_second_mail == 5 ~ "??",
                                                  # After_second_mail == 6 ~ "??",
                                                  # After_second_mail == 7 ~ "??",
                                                  # After_second_mail == 8 ~ "??",
                                                  # After_second_mail == 9 ~ "??",
                                                  # After_second_mail == 10 ~ "Previously code X") #TODO need to complete values Vol 2 p 129 + what is code X?
    ),
    diet_food_snacks = factor(Food_snacks #TODO complete
                              # case_when(
                              # Food_snacks == 1 ~ "??",
                              # Food_snacks == 2 ~ "??",
                              # Food_snacks == 3 ~ "??",
                              # Food_snacks == 4 ~ "??",
                              # Food_snacks == 5 ~ "??",
                              # Food_snacks == 6 ~ "??",
                              # Food_snacks == 7 ~ "??") #TODO need to complete values Vol 2 p 129
    ),
    diet_drink_snacks = factor(Drink_snacks #TODO complete
                               # case_when(
                               # Drink_snacks == 1 ~ "??",
                               # Drink_snacks == 2 ~ "??",
                               # Drink_snacks == 3 ~ "??",
                               # Drink_snacks == 4 ~ "??",
                               # Drink_snacks == 5 ~ "??",
                               # Drink_snacks == 6 ~ "??",
                               # Drink_snacks == 7 ~ "??") #TODO need to complete values Vol 2 p 129
    ),
    diet_main_meal_work = factor(Main_meal_work #TODO complete
                                 # case_when(
                                 # Main_meal_work == 1 ~ "??",
                                 # Main_meal_work == 2 ~ "??",
                                 # Main_meal_work == 3 ~ "??",
                                 # Main_meal_work == 4 ~ "??",
                                 # Main_meal_work == 5 ~ "??",
                                 # Main_meal_work == 6 ~ "??",
                                 # Main_meal_work == 7 ~ "??",
                                 # Main_meal_work == 8 ~ "??",
                                 # Main_meal_work == 9 ~ "??") #TODO need to complete values Vol 2 p 130?
    ),
    diet_main_meal_home = factor(Main_meal_home #TODO complete
                                 # case_when(
                                 # Main_meal_home == 1 ~ "??",
                                 # Main_meal_home == 2 ~ "??",
                                 # Main_meal_home == 3 ~ "??",
                                 # Main_meal_home == 4 ~ "??",
                                 # Main_meal_home == 5 ~ "??",
                                 # Main_meal_home == 6 ~ "??",
                                 # Main_meal_home == 7 ~ "??",
                                 # Main_meal_home == 8 ~ "??",
                                 # Main_meal_home == 9 ~ "??") #TODO need to complete values Vol 2 p 130?
    ),
    diet_main_meal_elsewhere = factor(Main_meal_elsewhere #TODO complete
                                      # case_when(
                                      # Main_meal_elsewhere == 1 ~ "??",
                                      # Main_meal_elsewhere == 2 ~ "??",
                                      # Main_meal_elsewhere == 3 ~ "??",
                                      # Main_meal_elsewhere == 4 ~ "??",
                                      # Main_meal_elsewhere == 5 ~ "??",
                                      # Main_meal_elsewhere == 6 ~ "??",
                                      # Main_meal_elsewhere == 7 ~ "??",
                                      # Main_meal_elsewhere == 8 ~ "??",
                                      # Main_meal_elsewhere == 9 ~ "??") #TODO need to complete values Vol 2 p 130?
    ),
    med_revised_MI_hx = ifelse(q88 == 1, TRUE, FALSE),
    diet_data_useable = case_when( # same as other variable `Diet_data_useable`
      diet == 1 ~ FALSE,
      diet == 0 ~ TRUE),
    diet_gms_protein_wk = protein,    # changed the order to fit old doc
    diet_gms_animal_protein_wk = anprot,
    diet_gms_tot_fat_wk = totfat,
    diet_gms_tot_sat_fat_wk = satfat,
    diet_oleic = oleic,
    diet_linoleic = linoleic,
    diet_tot_cho = totcho,
    diet_cho_strach = chostarc,
    diet_calories_eggs_wk = eggscal,
    diet_calories_bread_wk = breadcal,
    diet_calories_suger_wk = sugarcal,
    diet_calories_potatoes_wk = potatcal,
    diet_calories_rice_wk = ricecal,
    diet_calories_cereal_wk = cerlcal,
    diet_tot_calories_wk = totcal,
    death_reason_70 = factor(case_when( 
      siba70 == 1 ~ "MI",
      siba70 == 2 ~ "presumed MI",
      siba70 == 3 ~ "CVA",
      siba70 == 4 ~ "Malignancy",
      siba70 == 5 ~ "Trauma",
      siba70 == 6 ~ "Other",
      siba70 == 9 ~ "Unknown",
    )),
    death_reason_86 = siba86,
    med_cancer_diagnosis = yrdiag, 
    med_cancer_86 = make_date(sumyear, summnth),
    death_all_cause_86 = ifelse(mort == 1, TRUE, FALSE),
    med_diabetes_at_admission_63 = ifelse(diabprev == 1, TRUE, FALSE), 
    death_date_86 = as_date(date_die),
    death_timetoevent_86 = follow_up, #Censoring date = 1/7/86,
    med_cancer_68 = cancer, 
    #unk_entdate = entdate, # dates between 1973-1974. don't know what
    death_stroke_86 = ifelse(dstrok == 1, TRUE, FALSE),
    med_ihd_unk = ifelse(ihd == 1, TRUE, FALSE), # TODO not sure which year, there is a variable "verified_Heart_Attack" labeled in clean date "med_past_MI", all the positive there are also here, there are 264 new positive, i assume it's a follow up. 65? 68? 
    med_dm_test_68 = factor(case_when( 
      alldiab == 0 ~ "All DM tests Negative",
      alldiab == 2 ~ "Abnormal GTT",
      alldiab == 1 ~ "Any postive test out 3",
    )),
    exam_date_65 = make_date(Year_exam65, Month_exam65), 
    physical_activity_work_65 = factor(case_when( 
      q107 == 1 ~ "Mainly Sitting at Desk",
      q107 == 2 ~ "Mainly Sitting at car",
      q107 == 3 ~ "Mainly Standing",
      q107 == 4 ~ "Mainly Walking",
      q107 == 5 ~ "Physical Work",
      q107 == 6 ~ "Does not work",
      q107 == 7 ~ "Other type of activity",
    )),
    physical_activity_after_work = factor(case_when(
      q108 == 1 ~ "Almost None",
      q108 == 2 ~ "Sporadic",
      q108 == 3 ~ "Light",
      q108 == 4 ~ "Energetic",
    )),
    med_weight_65 = weight65,
    med_bmi_65 = med_weight_65 / ((med_height_admission/100)^2), 
    med_shoulder_measure_cm_biacromial = case_when(
      q112 < 0  ~ NA,
      TRUE ~ q112
    ),
    med_pelvic_measure_cm_bicristal = case_when(
      q113 < 0  ~ NA,
      TRUE ~ q113
    ),
    med_left_eye_pressure = case_when(
      q114 < 0  ~ NA,
      TRUE ~ q114
    ),
    med_right_eye_pressure = case_when(
      q115 < 0  ~ NA,
      TRUE ~ q115
    ),
    med_vital_capacity = case_when(
      q116 < 0  ~ NA,
      TRUE ~ q116
    ),
    med_timed_vital_capacity = case_when(
      q117 < 0  ~ NA,
      TRUE ~ q117
    ),
    med_past_diabetes_65 = factor(case_when(
      q118 == 1 ~ "Yes, new case",
      q118 == 2 ~ "Yes, old case",
      q118 == 3 ~ "Negative"
    )),
    med_past_MI_65 = factor(case_when(
      q119 == 1 ~ "Yes, new case",
      q119 == 2 ~ "Yes, old case",
      q119 == 3 ~ "Negative"
    )),
    med_takes_any_medication_65 = case_when( 
      q120 == 2 ~ TRUE,
      q120 == 1 ~ FALSE,
    ),
    med_past_angina_pectoris_65 = factor(case_when(
      q121 == 1 ~ "Definite AP",
      q121 == 2 ~ "Suspect AP",
      q121 == 3 ~ "Undefined chest pain",
      q121 == 4 ~ "No Chest Pain",
      q121 == 5 ~ "Secondary AP",
      q121 == 6 ~ "Susp. Sec. AP",
      q121 == 7 ~ "Sec. undefined Chest Pain"
    )), 
    med_past_intermittent_claudication_65 = factor(case_when(
      q122 == 1 ~ "Positive",
      q122 == 2 ~ "Negative",
      q122 == 3 ~ "No Diagnosis",
    )),
    med_current_smoke_65 = factor(case_when(
      Current_smoking_2 == 1 ~ "1-10",
      Current_smoking_2 == 2 ~ "11-20",
      Current_smoking_2 == 3 ~ "20+",
      Current_smoking_2 == 4 ~ "5 pipes or more",
      Current_smoking_2 == 5 ~ "less than 5 pipes",
      Current_smoking_2 == 6 ~ "never smoked",
      Current_smoking_2 == 7 ~ "ex-smoker"
    )),
    med_past_smoke_65 = factor(case_when(
      Previous_smoking_2 == 1 ~ "1-10",
      Previous_smoking_2 == 2 ~ "11-20",
      Previous_smoking_2 == 3 ~ "20+",
      Previous_smoking_2 == 4 ~ "5 pipes or more",
      Previous_smoking_2 == 5 ~ "less than 5 pipes",
      Previous_smoking_2 == 6 ~ "never smoked",
      Previous_smoking_2 == 7 ~ "Smoker Now"
    )),
    med_when_stoped_smoke_65 = factor(case_when(
      When_Discont_smoking_2 == 1 ~ "In the past 2 years",
      When_Discont_smoking_2 == 2 ~ "2-5 years",
      When_Discont_smoking_2 == 3 ~ "Over 5 years ago",
      When_Discont_smoking_2 == 6 ~ "Never Smoked",
      When_Discont_smoking_2 == 7 ~ "Current Smoker",
    )),
    med_diabetes_details_63 = factor(case_when( 
      q166 == 0 ~ "Normal",
      q166 == 1 ~ "Definite diabetes",
      q166 == 2 ~ "Definite diabetes",
      q166 == 3 ~ "Probable diabetes",
      q166 == 4 ~ "Definite diabetes",
      q166 == 5 ~ "Possible diabetes",
      q166 == 6 ~ "Probable diabetes",
      q166 == 7 ~ "Abnormal GTT",
      q166 == 8 ~ "Possible diabetes",
      q166 == 9 ~ "Abnormal GTT",
      q166 == 10 ~ "Normal GTT",
      q166 == 11 ~ "Inadequate data",
      q166 == 12 ~ "Not a suspect"
    )),
    med_diabetes_summary_63 = factor(case_when(
      q167 == 0 ~ "Negative",
      q167 == 1 ~ "Diabetes",
      q167 == 2 ~ "Abnormal GTT"
    )),
    med_diabetes_details_66 = factor(case_when( 
      q168 == 0 ~ "Normal",
      q168 == 1 ~ "Definite diabetes",
      q168 == 2 ~ "Definite diabetes",
      q168 == 3 ~ "Probable diabetes",
      q168 == 4 ~ "Definite diabetes",
      q168 == 5 ~ "Possible diabetes",
      q168 == 6 ~ "Probable diabetes",
      q168 == 7 ~ "Abnormal GTT",
      q168 == 8 ~ "Possible diabetes",
      q168 == 9 ~ "Abnormal GTT",
      q168 == 10 ~ "Normal GTT",
      q168 == 11 ~ "Inadequate data",
      q168 == 12 ~ "Not a suspect"
    )),
    med_diabetes_summary_66 = factor(case_when(
      q169 == 0 ~ "Negative",
      q169 == 1 ~ "Diabetes",
      q169 == 2 ~ "Abnormal GTT"
    )),
    med_diabetes_history_63 = factor(case_when(
      q170 == 0 ~ "Negative",
      q170 == 1 ~ "Positive"
    )),
    unk_month_of_illnes_unk = make_date(1900+q176, q175), 
    unk_outcome_of_illnes_unk = factor(case_when( 
      q178 == 1 ~ "Alive",
      q178 == 2 ~ "Dead",
    )), 
    med_conclusion_MI = factor(case_when(
      q199 == 0 ~ "No MI",
      q199 == 1 ~ "MI diagnosis based on ECG",
      q199 == 2 ~ "MI diagnosis based on Clinic and lab insufficient ECG",
      q199 == 3 ~ "Physician diagnosis of MI not accepted",
      q199 == 9 ~ "Physician diagnosis of MI not enough data"
    )),
    death_date_68 = make_date(1900+q214, q213), 
    death_sudden_68 = factor(case_when(
      q224 == 0 ~ "No",
      q224 == 1 ~ "Less than 1h",
      q224 == 2 ~ "1-24 hours"
    )),
    death_time_occurrence_since_MI_68 = factor(case_when(
      q225 == 1 ~ "day to week",
      q225 == 2 ~ "1-3 weeks",
      q225 == 3 ~ "3-6 weeks",
      q225 == 4 ~ "6 weeks to 4 months"
    )),
    death_autopsy_68 = factor(case_when(
      q226 == 1 ~ "Yes",
      q226 == 0 ~ "No"
    )),
    death_reason_conculsion_68 = factor(case_when( 
      q231 == 1 ~ "MI",
      q231 == 2 ~ "presumed MI",
      q231 == 3 ~ "CVA",
      q231 == 4 ~ "Malignancy",
      q231 == 5 ~ "Trauma",
      q231 == 6 ~ "Other"
    )),
    exam_date_68 = make_date(Year_exam68, Month_exam68), 
    med_past_diabetes_68 = factor(case_when(
      q274 == 0 ~ "None",
      q274 == 1 ~ "Since exam II",
      q274 == 2 ~ "Previously"
    )),
    med_past_HA_coronary_68 = factor(case_when(
      q275 == 0 ~ "None",
      q275 == 1 ~ "Since exam II",
      q275 == 2 ~ "Previously"
    )),
    med_past_HTN_68 = factor(case_when(
      q276 == 0 ~ "None",
      q276 == 1 ~ "Since exam II",
      q276 == 2 ~ "Previously"
    )),
    med_past_peptic_68 = factor(case_when(
      q277 == 0 ~ "None",
      q277 == 1 ~ "Since exam II",
      q277 == 2 ~ "Previously"
    )),
    med_past_respiratory_68 = factor(case_when(
      q278 == 0 ~ "None",
      q278 == 1 ~ "Since exam II",
      q278 == 2 ~ "Previously"
    )),
    med_past_kidney_68 = factor(case_when(
      q279 == 0 ~ "None",
      q279 == 1 ~ "Since exam II",
      q279 == 2 ~ "Previously"
    )),
    med_past_htn_medication_68 = case_when(
      q280 == 1 ~ TRUE,
      q280 == 0 ~ FALSE
    ),
    med_past_insulin_68 = case_when(
      q281 == 1 ~ TRUE,
      q281 == 0 ~ FALSE
    ),
    med_past_angina_pectoris_68 = factor(case_when(
      q282 == 1 ~ "Definite",
      q282 == 2 ~ "Suspect",
      q282 == 3 ~ "Undefined chest pain",
      q282 == 4 ~ "No Chest Pain",
      q282 == 5 ~ "Emotional functional chest pain",
      q282 == 6 ~ "Susp. Emotional functional chest pain",
      q282 == 7 ~ "Undefined chest pain"
    )), 
    med_past_intermittent_claudication_68 = factor(case_when( 
      q283 == 1 ~ "Present",
      q283 == 2 ~ "Not present"
    )),
    med_past_lungs_68 = factor(case_when(
      q284 == 0 ~ "NAD",
      q284 == 1 ~ "Congested ling bases",
      q284 == 2 ~ "Asthma or bronchitis",
      q284 == 3 ~ "Other"
    )),
    med_past_limbs_68 = factor(case_when(
      q285 == 0 ~ "NAD",
      q285 == 1 ~ "Hemiplegia",
      q285 == 2 ~ "Other"
    )),
    med_sbp_start_68 = case_when(
      q291 < 0  ~ NA,
      TRUE ~ q291
    ),
    med_dbp_start_68 = case_when(
      q292 < 0  ~ NA,
      TRUE ~ q292
    ),
    med_sbp_end_68 = case_when(
      q286 < 0  ~ NA,
      TRUE ~ q286
    ),
    med_dbp_end_68 = case_when(
      q287 < 0  ~ NA,
      TRUE ~ q287
    ),
    med_weight_68 = weight68,
    med_bmi_68 = med_weight_68 / ((med_height_admission/100)^2), 
    med_ecg_68 = factor(case_when(
      q290 == 0 ~ "Not Taken",
      q290 == 1 ~ "Paper Only",
      q290 == 2 ~ "Paper and Tape"
    )),
    med_fvc_68 = case_when(
      q293 < 0  ~ NA,
      TRUE ~ q293
    ),
    # med_diabetes_1963 = factor(case_when(
    #   q370 == 0 ~ "Negative",
    #   q370 == 1 ~ "Diabetes",
    #   q370 == 2 ~ "Abnormal GTT"
    # )),
    # med_diabetes_1965 = factor(case_when(
    #   q371 == 0 ~ "Negative",
    #   q371 == 1 ~ "Diabetes",
    #   q371 == 2 ~ "Abnormal GTT"
    # )),
    # med_diabetes_1968 = factor(case_when(
    #   q372 == 0 ~ "Negative",
    #   q372 == 1 ~ "Diabetes",
    #   q372 == 2 ~ "Abnormal GTT"
    # )),
    med_past_peptic_summary = factor(case_when( # TODO please verify me on the levels. what 9 + 0
      q373 == 0 ~ "well",
      q373 == 1 ~ "Incidence",
      q373 == 8 ~ "Not at risk"
    )),
    dmg_origin = factor(case_when(
      Origin_2 == 1 ~ "Israel 2nd generation",
      Origin_2 == 2 ~ "East europe",
      Origin_2 == 3 ~ "Central europe",
      Origin_2 == 4 ~ "South europe",
      Origin_2 == 5 ~ "Asia",
      Origin_2 == 6 ~ "North america",
      Origin_2 == 7 ~ "Israel 1st gen - son to europian",
      Origin_2 == 8 ~ "Israel 1st gen - son to asian-africans",
      Origin_2 == 9 ~ "Israel 1st gen - son to unknown"
    )),
    physical_activity_65 = factor(case_when(
      q377 == 1 ~ "Lowest",
      q377 == 2 ~ "Low",
      q377 == 3 ~ "Middle",
      q377 == 4 ~ "High",
      q377 == 5 ~ "Highest"
    )),
    #   unk_sociological_data = ifelse(q380 == 1, TRUE, FALSE), 
    dmg_rlgeduc = factor(case_when(
      rlgeduc == 1 ~ "Only Religious",
      rlgeduc == 2 ~ "Religious and Secular",
      rlgeduc == 3 ~ "Only Secular",
      rlgeduc == 4 ~ "didn't learn or no category was checked",
    )),
    dmg_rlgself = factor(case_when(
      rlgself == 1 ~ "Most Religious",
      rlgself == 2 ~ "Less Religious",
      rlgself == 3 ~ "Traditional",
      rlgself == 4 ~ "Secular",
      rlgself == 5 ~ "Agnostic"
    )),
    dmg_income_inx_source_salary = ifelse(q383 != 0 & q383 != 9, TRUE, FALSE),
    dmg_income_inx_source_extra_work = ifelse(q383 == 2 | q383 == 5 | q383 == 6 | q383 == 8, TRUE, FALSE),
    dmg_income_inx_source_additional = ifelse(q383 == 3 | q383 == 5 | q383 == 7 | q383 == 8, TRUE, FALSE),
    dmg_income_inx_source_wife = ifelse(q383 == 4 | q383 == 6 | q383 == 7 | q383 == 8, TRUE, FALSE),
    dmg_income_inx_source_other = ifelse(q383 == 9, TRUE, FALSE),
    dmg_income_inx_total = factor(case_when(
      q384 == 0 ~ "No Income",
      q384 == 1 ~ "<29",
      q384 == 2 ~ "30-44",
      q384 == 3 ~ "45-59",
      q384 == 4 ~ "60-79",
      q384 == 5 ~ "80-99",
      q384 == 6 ~ "100-149",
      q384 == 7 ~ "150-199",
      q384 == 8 ~ "200-249",
      q384 == 9 ~ "250<",
    ),
    levels = c("No Income","<29","30-44","45-59","60-79","80-99","100-149","150-199","200-249","250<"), ordered = TRUE),
    dmg_father_birth_country = factor(q385), #TODO var 321, values in appendix II 
    dmg_residenses_num_after_start_work = q386, #is numeric, but >9 is listed as 9
    dmg_countries_num_after_start_work = q387, #is numeric, but >9 is listed as 9
    work_satisfaction = work_satisfaction, 
    work_pressure = q389,
    family_private_life_satisfaction = q390,
    work_durable_goods = q391, #TODO what the is that? 
    dmg_parent_class_discrepancy = case_when(
      q392 == 77 ~ 7,
      q392 > -8 ~ q392
    ),
    unk_occupational_pattern_code = q393, #TODO couldn't understand their code for it or what it represent
    unk_occupational_pattern_index = q394, #TODO couldn't understand their code for it or what it represent
    unk_effort_disappointment_trouble_work = factor(case_when( #TODO on file only 0,1,9 are describes but there is more levels
      q395 == 0 ~ "None",
      q395 == 1 ~ "Only one of three",
      q395 == 2 ~ "",
      q395 == 3 ~ "",
      q395 == 4 ~ "",
      q395 == 5 ~ "",
      q395 == 6 ~ "",
      q395 == 7 ~ "",
      q395 == 8 ~ "",
      q395 == 9 ~ "All three",
    )),
    unk_effort_disappointment_trouble_married_life = factor(case_when( #TODO on file only 0,1,9 are describes but there is more levels
      q396 == 0 ~ "None",
      q396 == 1 ~ "Only one of three",
      q396 == 2 ~ "",
      q396 == 3 ~ "",
      q396 == 4 ~ "",
      q396 == 5 ~ "",
      q396 == 6 ~ "",
      q396 == 7 ~ "",
      q396 == 8 ~ "",
      q396 == 9 ~ "All three",
    )),
    unk_effort_disappointment_trouble_standard_life = factor(case_when( #TODO on file only 0,1,9 are describes but there is more levels
      q397 == 0 ~ "None",
      q397 == 1 ~ "Only one of three",
      q397 == 2 ~ "",
      q397 == 3 ~ "",
      q397 == 4 ~ "",
      q397 == 5 ~ "",
      q397 == 6 ~ "",
      q397 == 7 ~ "",
      q397 == 8 ~ "",
      q397 == 9 ~ "All three",
    )),
    unk_effort_disappointment_trouble_social_life = factor(case_when( #TODO on file only 0,1,9 are describes but there is more levels
      q398 == 0 ~ "None",
      q398 == 1 ~ "Only one of three",
      q398 == 2 ~ "",
      q398 == 3 ~ "",
      q398 == 4 ~ "",
      q398 == 5 ~ "",
      q398 == 6 ~ "",
      q398 == 7 ~ "",
      q398 == 8 ~ "",
      q398 == 9 ~ "All three",
    )),
    unk_profession = profess, #TODO new doc num 77, listed as s355, in decription socio.tape see appendix 1.  
    unk_profession_other = q399, #TODO what is it, and how is it different from the one above? different valus
    dmg_birth_order = factor(case_when( 
      q400 == 1 ~ "1st born",
      q400 == 2 ~ "Neither 1sr nor last",
      q400 == 3 ~ "Last born",
      q400 == 4 ~ "One in pair of twins"
    ), 
    levels = c("1st born","Neither 1sr nor last","Last born","One in pair of twins"),
    ordered = TRUE),
    dmg_crisis_father_death = factor(case_when( #TODO 0  not in file
      q401 == 1 ~ "No crisis",
      q401 == 2 ~ "age 0-6",
      q401 == 3 ~ "age 7-12",
      q401 == 4 ~ "0-12",
      q401 == 5 ~ "age 13-18",
      q401 == 8 ~ "0-18",
      q401 == 9 ~ "19",
      q401 == 10 ~ "0-6 + 19",
      q401 == 11 ~ "7-12 + 19",
      q401 == 15 ~ "7-19"
    )),
    dmg_crisis_mother_death = factor(case_when(
      q402 == 1 ~ "No crisis",
      q402 == 2 ~ "age 0-6",
      q402 == 3 ~ "age 7-12",
      q402 == 4 ~ "0-12",
      q402 == 5 ~ "age 13-18",
      q402 == 8 ~ "0-18",
      q402 == 9 ~ "19"
    )),
    dmg_concentration_camp = factor(case_when( # TODO rest of the levels not listed
      q403 == 0 ~ "age unknown, unknown with whom",
      q403 == 1 ~ "Never was",
      q403 == 5 ~ "5",
      q403 == 9 ~ "9",
      q403 == 11 ~ "11",
      q403 == 17 ~ "17",
      q403 == 21 ~ "21",
      q403 == 25 ~ "25",
      q403 == 29 ~ "29",
      q403 == 37 ~ "37",
      q403 == 41 ~ "41",
      q403 == 45 ~ "45",
      q403 == 77 ~ "77",
    )),
    work_family_interested = factor(case_when(
      q404 == 1 ~ "Yes",
      q404 == 2 ~ "Not very much",
      q404 == 3 ~ "Only in certain aspects",
      q404 == 4 ~ "No",
      q404 == 5 ~ "No wife"
    )),
    dmg_success_marriage = factor(case_when(
      q405 == 1 ~ "Very successful",
      q405 == 2 ~ "Rather successful",
      q405 == 3 ~ "Not so successful",
      q405 == 4 ~ "Unsuccessful",
      q405 == 5 ~ "No wife"
    )),
    #  comp_restrain_retaliation = restrain_retaliation, #dochtomous q48
    work_station = factor(case_when( # TODO what does it mean
      station == 1 ~ "Jerusalem",
      station == 2 ~ "Tel Aviv",
      station == 3 ~ "Haifa"
    )),
    comp_ihd_incidence = case_when(
      ihd_wide == 0 ~ FALSE,
      ihd_wide == 1 ~ TRUE
    ),
    unk_never_discuss = never_discuss,
    comp_wt_change_63_65 = factor(case_when( # TODO cant see 0 + 3 + 4
      wt_change == 1 ~ "increased serially",
      wt_change == 2 ~ "decreased serially",
      wt_change == 3 ~ "3",
      wt_change == 4 ~ "4"
    )),
    comp_no_changewt = no_changewt,
    comp_incr_changewt = incr_changewt,
    comp_decl_changewt = decl_changewt,
    comp_wtlow_high = wtlow_high,
    comp_wthigh_low = wthigh_low,
    comp_fluctuate = fluctuate,
    comp_pcpoly = pcpoly,
    comp_CHD63_wide = CHD63_wide,
    comp_SBPdiff = SBPdiff,
    comp_MedianSBPdiff = MedianSBPdiff,
    death_died_all_2006 = died_all,
    death_age_died_2006 = age_died,
    death_year_died_2006 = year_died,
    comp_reach_80_2006 = reach_80,
    comp_reach_85_2006 = reach_85,
    comp_max_age_2006 = max_age,
    comp_Dwt68_63 = Dwt68_63,
    unk_Stroke_42yr = Stroke_42yr,
    comp_HighBP63 = factor(case_when(
      HighBP63 == 1 ~ ">140",
      HighBP63 == 0 ~ "140<"
    )),
    comp_died_strok = died_strok,
    comp_agnostic = `_agnostic_`,
    comp_SES_4cat = SES_4cat,
    comp_Metasynd = Metasynd,
    comp_num_Metasynd = num_Metasynd,
    comp_trunk_periph_fat = trunk_periph_fat,
    comp_Trunk_quartile = Trunk_quartile,
    comp_father_crisis = father_crisis,
    comp_mother_crisis = mother_crisis,
    comp_parent_crisis = parent_crisis,
    dmg_concentration = case_when(
      concentration == 0 ~ FALSE,
      concentration == 1 ~ TRUE
    ),
    comp_year_imm_group = factor(case_when(
      year_imm_group == 1 ~ "<1935",
      year_imm_group == 2 ~ "1936-48",
      year_imm_group == 3 ~ "1949-50",
      year_imm_group == 4 ~ ">1950"
    )),
    comp_restrictive_lung = restrictive_lung,
    comp_obstructive_lung = obstructive_lung,
    comp_Lung_ratio = Lung_ratio,
    comp_Lungs_anyside = Lungs_anyside,
    comp_Forced_VC = Forced_VC,
    comp_FEV_FVC_ratio = FEV_FVC_ratio,
    comp_FEVtert = FEVtert,
    comp_tert_ratio_1 = tert_ratio_1,
    comp_satisfaction_marital = satisfaction_marital,
    comp_prctfat_quart = prctfat_quart,
    comp_meanChol_40 = meanChol_40,
    comp_bicristal = bicristal,
    comp_acrom_crist_ratio = acrom_crist_ratio,
    comp_acrom_quart = acrom_quart,
    comp_incid_MI = incid_MI,
    comp_generation = generation,
    death_died_97 = died_97,
    comp_cal_oleic = cal_oleic,
    comp_PP63 = PP63,
    comp_Rel_PP_index63 = Rel_PP_index63,
    comp_sedentary = sedentary,
    comp_seperate_widower = seperate_widower,
    comp_MAP63 = MAP63,
    comp_fathercrisis_rerank = fathercrisis_rerank,
    comp_alive_asses = alive_asses,
    comp_diameter_ratio_quart = diameter_ratio_quart,
    comp_Holocaust = Holocaust,
    comp_HDL_percent = HDL_percent,
    comp_Concent_Europe = Concent_Europe,
    comp_nonhdl65_Gr = nonhdl65_Gr,
    comp_CKD = CKD,
    unk_examined_1965 = examined_1965,
    unk_holocaust_Threeway = holocaust_Threeway,
    death_died_CHD_97 = died_CHD_97,
    comp_COPD = COPD,
    comp_RENAL_chronic = RENAL_chronic,
    comp_Type_MI_Incidence = Type_MI_Incidence,
    comp_unrecognized_MI = unrecognized_MI,
    comp_Incid_AP = Incid_AP,
    comp_forced_pv = forced_pv,
    comp_Pastsmok_65 = Pastsmok_65,
    comp_Discont_65 = Discont_65,
    comp_Visit_by_SBP = Visit_by_SBP,
    FAMPROB = FAMPROB
  ) %>%
  filter(!is.na(id_nivdaki))

# Comp ####

## BMI ####
weigh_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"med_weight")]
bmi_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"med_bmi")]
sbp_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"med_sbp")]
dbp_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"med_dbp")]
height_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"med_height")]
bmi_sbp_vars <- c(weigh_vars,bmi_vars,height_vars,
                  sbp_vars,dbp_vars)

bmi_sbp_data <- clean_df %>%
  rowwise() %>%
  mutate(    
    med_sbp_mean = mean(c_across(all_of(sbp_vars)), na.rm = TRUE),
    med_sbp_sd = sd(c_across(all_of(sbp_vars)), na.rm = TRUE),
    med_dbp_mean = mean(c_across(all_of(dbp_vars)), na.rm = TRUE),
    med_dbp_sd = sd(c_across(all_of(dbp_vars)), na.rm = TRUE),
    #med_weight_sd = sd(c_across(starts_with("med_weight")), na.rm = TRUE),
    med_weight_range = case_when(
      !is.na(med_weight_admission) & !is.na(med_weight_68) ~ med_weight_admission-med_weight_68,
      !is.na(med_weight_admission) & !is.na(med_weight_65) ~ med_weight_admission-med_weight_65,
      !is.na(med_weight_65) & !is.na(med_weight_68) ~ med_weight_65-med_weight_68,
      TRUE ~ 0
    ),
    med_weight_mean = mean(c_across(all_of(weigh_vars)), na.rm = TRUE),
    med_bmi_sd = sd(c_across(all_of(bmi_vars)), na.rm = TRUE),
    med_bmi_mean = mean(c_across(all_of(bmi_vars)), na.rm = TRUE),
    med_height_mean = mean(c_across(all_of(height_vars)), na.rm = TRUE)) %>%
  ungroup() %>%
  # mutate(med_weight_change = case_when(
  #   comp_wtlow_high == 1 ~ "gain",
  #   comp_wthigh_low == 1 ~ "lose",
  #   TRUE ~ "No change"),
  #   levels = c("No change","lose","gain")
  # ) %>% 
  select(id_nivdaki,med_sbp_mean,med_sbp_sd,med_dbp_mean,med_dbp_sd, #med_weight_change med_weight_sd
         med_weight_mean,med_weight_range,med_bmi_mean,med_bmi_sd,med_height_mean)

## labs ####
labs_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"lab")]

labs_data <- clean_df %>%
  rowwise() %>%
  transmute(
    id_nivdaki,
    lab_mean_cholesterol = mean(c_across(starts_with("lab_cholesterol")), na.rm = TRUE),
    lab_range_cholesterol = case_when(
      !is.na(lab_cholesterol_65) & !is.na(lab_cholesterol_63) ~ lab_cholesterol_63 - lab_cholesterol_65,
      TRUE ~ 0),
    lab_mean_nonhdl = mean(c_across(starts_with("lab_nonhdl")), na.rm = TRUE),
    lab_range_nonhdl = case_when(
      !is.na(lab_nonhdl_65) & !is.na(lab_nonhdl_63) ~ lab_nonhdl_63 - lab_nonhdl_65,
      TRUE ~ 0),
    lab_mean_hdl = mean(c_across(starts_with("lab_hdl")), na.rm = TRUE),
    lab_range_hdl = case_when(
      !is.na(lab_hdl_65) & !is.na(lab_hdl_63) ~ lab_hdl_63 - lab_hdl_65,
      TRUE ~ 0),
    lab_glucose = lab_glucose_65,
    lab_hemoglobin = lab_hemoglobin_63,
    lab_hematocrit = lab_hematocrit_63,
    lab_uacid = lab_uacid_63,
    lab_blood_group
  ) %>%
  ungroup()

## smoking ####
smoke_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"smok")]

smoke_data <- clean_df %>%
  select(id_nivdaki, all_of(smoke_vars)) %>%
  mutate(num_63 = case_when(
    med_smoke_63 == "never smoked" ~ 0,
    med_smoke_63 == "ex-smoker" ~ 0,
    med_smoke_63 == "1-10" ~ 1,
    med_smoke_63 == "11-20" ~ 2,
    med_smoke_63 == "20+" ~ 3,
    med_smoke_63 == "less than 5 pipes" ~ 2,
    med_smoke_63 == "5 pipes or more" ~ 3
  ),
  num_65 = case_when(
    med_smoke_65 == "never smoked" ~ 0,
    med_smoke_65 == "ex-smoker" ~ 0,
    med_smoke_65 == "1-10" ~ 1,
    med_smoke_65 == "11-20" ~ 2,
    med_smoke_65 == "20+" ~ 3,
    med_smoke_65 == "less than 5 pipes" ~ 2,
    med_smoke_65 == "5 pipes or more" ~ 3
  ),
  diff = num_65 - num_63,
  med_smoke_status = ifelse(!is.na(med_smoke_63),
                            as.character(med_smoke_63),
                            as.character(med_smoke_65)),
  med_smoke_status = factor(case_when(
    med_smoke_status == "less than 5 pipes" ~ "11-20",
    med_smoke_status == "5 pipes or more" ~ "20+",
    TRUE ~ as.character(med_smoke_status)
  )),
  med_smoke_change = factor(case_when(
    diff > 0 ~ "Increased",
    diff < 0 ~ "Decreased",
    diff == 0 ~ "No change",
  ))) %>%
  select(id_nivdaki, med_smoke_status, med_smoke_change)


## diabetes ####
diabetes_vars <- c(colnames(clean_df)[str_detect(colnames(clean_df),"diabetes")],
                   "med_dm_test_68") #TODO should we keep med_dm_test_unk?
diabetes_data <- clean_df %>%
  select(id_nivdaki, all_of(diabetes_vars)) %>%
  mutate(
    # med_diabetes_status = factor(ifelse(!is.na(med_diabetes_summary_63),
    #                                  as.character(med_diabetes_summary_63),
    #                                  as.character(med_diabetes_summary_66))),
    med_diabetes_status = factor(case_when(
      med_diabetes_summary_63 == "Diabetes" |
        med_diabetes_summary_66 == "Diabetes" ~ "Diabetes",
      med_diabetes_summary_63 == "Abnormal GTT" |
        med_diabetes_summary_66 == "Abnormal GTT" ~ "Abnormal GTT",
      med_diabetes_summary_63 == "Negative" | 
        med_diabetes_summary_66 == "Negative" ~ "Negative",
    )),
    num_63 = case_when(
      med_diabetes_summary_63 == "Negative" ~ 0,
      med_diabetes_summary_63 == "Abnormal GTT" ~ 1,
      med_diabetes_summary_63 == "Diabetes" ~ 2
    ),
    num_65 = case_when(
      med_diabetes_summary_66 == "Negative" ~ 0,
      med_diabetes_summary_66 == "Abnormal GTT" ~ 1,
      med_diabetes_summary_66 == "Diabetes" ~ 2
    ),
    med_diabetes_new = num_65-num_63 > 0
  ) %>%
  transmute(id_nivdaki,
            med_dm = factor(case_when(
              med_diabetes_status == "Diabetes" ~ "Present",
              med_diabetes_status == "Abnormal GTT" ~ "Present",
              med_diabetes_at_admission_63 == TRUE ~ "Present",
              med_diabetes_new == TRUE ~ "Present",
              TRUE ~ "Negative"
            )))

## MI ####
mi_vars <- c(colnames(clean_df)[str_detect(colnames(clean_df),"MI")],
             colnames(clean_df)[str_detect(colnames(clean_df),"ihd")],
             "med_past_HA_coronary_68","comp_CHD63_wide")

mi_data <- clean_df %>%
  select(id_nivdaki, all_of(mi_vars)) %>%
  mutate(
    med_past_MI = ifelse(med_past_MI == 1, TRUE, FALSE),
    med_varified_MI,
    med_new_MI = ifelse(comp_incid_MI == 1, TRUE, FALSE),
    med_new_MI_unrecognized = ifelse(comp_unrecognized_MI == 1, TRUE, FALSE),
    med_past_ihd = med_ihd_unk,
    med_new_ihd = comp_ihd_incidence
  ) %>%
  transmute(
    id_nivdaki,
    med_mi = factor(case_when(
      med_past_HA_coronary_68 == "Previously" ~ "Present",
      med_past_HA_coronary_68 == "Since exam II" ~ "Present",
      med_past_MI == TRUE ~ "Present",
      med_varified_MI == TRUE ~ "Present",
      med_new_MI == TRUE ~ "Present",
      med_new_MI_unrecognized == TRUE ~ "Present",
      TRUE ~ "Negative"
    )),
    med_ihd = factor(case_when(
      comp_CHD63_wide == 1 ~ "Present",
      med_past_ihd == TRUE ~ "Present",
      med_new_ihd == TRUE ~ "Present",
      TRUE ~ "Negative"
    )))

## angina ####
angina_vars <- c(colnames(clean_df)[str_detect(colnames(clean_df),"angina")],
                 "comp_Incid_AP")

angina_data <- clean_df %>%
  select(id_nivdaki, all_of(angina_vars)) %>%
  mutate(
    med_angina_63 = factor(case_when( #TODO check my grouping
      med_past_angina_pectoris == "Definite" ~ "Definite AP",
      med_past_angina_pectoris == "Suspect" ~ "Suspect AP",
      med_past_angina_pectoris == "Functional Pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "Functional Pain (II)" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "Not Functional Pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "None of the above" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "No diagnosis" ~ "No Chest Pain",
    )),
    med_angina_65 = factor(case_when(
      med_past_angina_pectoris_65 == "Definite AP" ~ "Definite AP",
      med_past_angina_pectoris_65 == "Suspect AP" ~ "Suspect AP",
      med_past_angina_pectoris_65 == "Secondary AP" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "Susp. Sec. AP" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "Undefined chest pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "Sec. undefined Chest Pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "No Chest Pain" ~ "No Chest Pain",
    )),
    med_angina_68 = factor(case_when(
      med_past_angina_pectoris_68 == "Definite" ~ "Definite AP",
      med_past_angina_pectoris_68 == "Suspect" ~ "Suspect AP",
      med_past_angina_pectoris_68 == "Emotional functional chest pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris_68 == "Susp. Emotional functional chest pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris_68 == "Undefined chest pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris_68 == "No Chest Pain" ~ "No Chest Pain",
    )),
  ) %>% 
  transmute(
    id_nivdaki,
    med_angina = factor(case_when(
      med_angina_63 == "Definite AP" ~ "Present",
      med_angina_63 == "Suspect AP" ~ "Present",
      med_angina_65 == "Definite AP" ~ "Present",
      med_angina_65 == "Suspect AP" ~ "Present",
      med_angina_68 == "Definite AP" ~ "Present",
      med_angina_68 == "Suspect AP" ~ "Present",
      TRUE ~ "Negative"
    ))
  )

## Cancer #### 
#TODO should we drop it? is it after 68?
cancer_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"cancer")]

cancer_data <- clean_df %>%
  select(id_nivdaki, all_of(angina_vars)) %>%
  transmute(
    id_nivdaki
  )

## Stroke ####

## pulmonary ####
#TODO need explanations regarding those vars
pulmonary_vars <- c("comp_obstructive_lung", "comp_Lung_ratio", "comp_Lungs_anyside",
                    "comp_Forced_VC", "comp_FEV_FVC_ratio", "comp_FEVtert", 
                    "comp_tert_ratio_1", "comp_COPD","med_past_lungs_68",
                    "med_past_respiratory_68")

pulmonary_data <- clean_df %>%
  mutate(
    med_obstructive_lung = ifelse(comp_obstructive_lung == 1, TRUE, FALSE),
    med_Lungs_anyside = ifelse(comp_Lungs_anyside == 1, TRUE, FALSE),
    med_COPD = ifelse(comp_COPD == 1, TRUE, FALSE)
  ) %>%
  transmute(
    id_nivdaki, 
    # med_lung_ratio = comp_Lung_ratio, 
    # med_forced_VC = comp_Forced_VC,
    # med_FEV_FVC_ratio = comp_FEV_FVC_ratio,
    med_lung = factor(case_when(
      med_past_lungs_68 == "Asthma or bronchitis" ~ "Present",
      med_past_lungs_68 == "Other" ~ "Present",
      med_past_respiratory_68 == "Previously" ~ "Present",
      med_past_respiratory_68 == "Since exam II" ~ "Present",
      med_obstructive_lung == TRUE ~ "Present",
      med_Lungs_anyside == TRUE ~ "Present",
      med_COPD == TRUE ~ "Present",
      TRUE ~ "Negative"
    ))
  )

## general ####
gen_med_vars <- c("med_past_HTN_68","med_past_limbs_68","med_past_intermittent_claudication",
                  "med_past_intermittent_claudication_65","med_past_intermittent_claudication_68",
                  "med_past_peptic_summary", "med_past_peptic", "med_past_peptic_68","med_peripheral_art_dis",
                  "med_takes_any_medication_65","med_past_htn_medication_68","comp_CKD",
                  "med_past_kidney_68","comp_RENAL_chronic")

gen_med <- clean_df %>%
  transmute(
    id_nivdaki,
    med_peripheral_art_dis = factor(case_when(
      med_peripheral_art_dis == "Definite" ~ "Present",
      med_peripheral_art_dis == "Suspect" ~ "Present",
      med_peripheral_art_dis == "None" ~ "Negative",
    )),
    med_htn = factor(case_when(
      med_past_HTN_68 == "Since exam II" ~ "Present",
      med_past_HTN_68 == "Previously" ~ "Present",
      med_past_HTN_68 == "None" ~ "Negative",
    )),
    med_limbs = factor(case_when(
      med_past_limbs_68 == "Hemiplegia" ~ "Present",
      med_past_limbs_68 == "Other" ~ "Present",
      med_past_limbs_68 == "NAD" ~ "Negative",
    )),
    med_intermittent_claudication = factor(case_when(
      med_past_intermittent_claudication == "IC" ~ "Present",
      med_past_intermittent_claudication == "Multiple answers" ~ "Present",
      med_past_intermittent_claudication_65 == "Positive" ~ "Present",
      med_past_intermittent_claudication_68 == "Present" ~ "Present",
      TRUE ~ "Negative")),
    med_peptic = factor(case_when(
      med_past_peptic_summary == "Incidence" ~ "Present",
      med_past_peptic == TRUE ~ "Present",
      med_past_peptic_68 == "Previously" ~ "Present",
      med_past_peptic_68 == "Since exam II" ~ "Present",
      TRUE ~ "Negative"
    )),
    med_renal = factor(case_when(
      comp_CKD == 1 ~ "Present",
      comp_CKD == 2 ~ "Present",
      med_past_kidney_68 == "Previously" ~ "Present",
      med_past_kidney_68 == "Since exam II" ~ "Present",
      comp_RENAL_chronic == 1 ~ "Present",
      comp_RENAL_chronic == 2 ~ "Present",
      TRUE ~ "Negative"
    )),
    med_medication = factor(case_when(
      med_takes_any_medication_65 == TRUE ~ "Yes",
      med_past_htn_medication_68 == TRUE ~ "Yes",
      TRUE ~ "No"
    ))
  )

## comp ####
comp_vars <- colnames(clean_df)[str_detect(colnames(clean_df),"comp")]

comp_data <- clean_df %>%
  transmute(
    id_nivdaki,
    comp_pcpoly,
    comp_SES_4cat = factor(comp_SES_4cat, ordered = TRUE),
    comp_forced_pv)

#  outcome ####
reach_95 <- read_csv("raw_data/IIHD_reach95_Dor&Saar.csv", show_col_types = FALSE) %>%
  transmute(
    death_date_2019 = as.Date(dateptira_2019, format = "%m/%d/%Y"),
    exam_start_fu = as.Date(beginfu, format = "%m/%d/%Y"),
    dmg_birth_year = yearob,
    outcome_age_lfu = age_lfu,
    id_nivdaki = nivdaki)

# create final df ####
death_vars <- colnames(clean_df)[str_starts(colnames(clean_df),"death")]
unk_vars <- colnames(clean_df)[str_starts(colnames(clean_df),"unk")]
final_df <- clean_df %>%
  select(-c(all_of(smoke_vars),all_of(diabetes_vars),
            all_of(labs_vars),all_of(bmi_sbp_vars),
            all_of(mi_vars), all_of(angina_vars),
            all_of(pulmonary_vars),all_of(gen_med_vars),
            all_of(cancer_vars), all_of(comp_vars),
            "dmg_concentration",
            all_of(death_vars), all_of(unk_vars))) %>%
  left_join(bmi_sbp_data, by = "id_nivdaki") %>%
  left_join(smoke_data, by = "id_nivdaki") %>%
  left_join(diabetes_data, by = "id_nivdaki") %>%
  left_join(angina_data, by = "id_nivdaki") %>%
  left_join(mi_data, by = "id_nivdaki") %>%
  left_join(gen_med, by = "id_nivdaki") %>%
  left_join(labs_data, by = "id_nivdaki") %>%
  left_join(pulmonary_data, by = "id_nivdaki") %>%
  left_join(comp_data, by = "id_nivdaki") %>%
  left_join(reach_95, by = "id_nivdaki") %>%
  mutate(dmg_time_abroad = case_when(
    dmg_immigration_year - dmg_birth_year >= 0 ~ dmg_immigration_year - dmg_birth_year,
    TRUE ~ 0
  ),
  dmg_concentration_camp = factor(ifelse(dmg_concentration_camp == "Never was", 
                                         "Never was", "Was")))

save(final_df, file = "raw_data/final_df.RData")


