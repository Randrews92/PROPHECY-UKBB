install.packages("gtsummary")
install.packages("broom.mixed")
install.packages("tidyverse")
library(gtsummary)
library(broom.mixed)
library(data.table)
library(tidyverse)
library(dplyr)
# Loading libraries
install.packages('car')
library(car) # For Anova and repeated measures functions
# Load necessary libraries
install.packages('afex')
install.packages('emmeans')
library(afex) # for mixed ANOVA
library(emmeans) # for post-hoc and extracting SD_within
# Load necessary libraries
install.packages("mediation")
library(mediation)
library(survival)


### Now we have a sample of 166736 women. 
# dx download final_version.csv
reproductive_clean_rm <- read.csv("final_version.csv")

n_distinct(reproductive_clean_rm $Participant.ID)

## Set reference levels 

# dx download final_1.csv
#reproductive_clean_rm <- read.csv("final_1.csv")
reproductive_clean_rm$hrt_group <- relevel(as.factor(reproductive_clean_rm$hrt_group), ref = "None")
reproductive_clean_rm$meno_group3 <- relevel(as.factor(reproductive_clean_rm$meno_group3), ref = "Average (45–55)")
reproductive_clean_rm$oc_group  <- relevel(as.factor(reproductive_clean_rm$oc_group),  ref = "None")
reproductive_clean_rm$number_births_group  <- relevel(as.factor(reproductive_clean_rm$number_births_group),  ref = "No children")
#reproductive_clean_rm$smoker_status  <- relevel(as.factor(reproductive_clean_rm$smoker_status),  ref = "Never")
#reproductive_clean_rm$alcohol_status  <- relevel(as.factor(reproductive_clean_rm$alcohol_status),  ref = "Never")

reproductive_clean_2 <-reproductive_clean_rm
reproductive_clean_2%>%
  group_by(meno_group3)%>%
  summarise(cnt=n_distinct(Participant.ID))


##----------------------------------
## Model 1: Crude (meno_group3 only)
##----------------------------------


cox_m1 <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3,
  data = reproductive_clean_2
)

summary(cox_m1)

##---------------------------------------------------------
## Model 2: + Demographic / SEP / Reproductive / APOE
##---------------------------------------------------------
# age, ethnicity, SEP, education, deprivation,
# reproductive history & surgeries, APOE

cox_m2 <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 +
    APOE4 +
    white_yn +
    deprivation_quintiles +
    tertiary_degree +
    age_menarche +
    number_births_group +
    oc_group +
    bilateral_oophorectomy +
    hysterectomy,
  data = reproductive_clean_2
)

summary(cox_m2)

##---------------------------------------------
## Model 3: + Psychosocial & hearing
##---------------------------------------------
# mental health contact, anxiety, loneliness, isolation, hearing

cox_m3 <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 +
    APOE4 +
    white_yn +
    deprivation_quintiles +
    tertiary_degree +
    age_menarche +
    number_births_group +
    oc_group +
    bilateral_oophorectomy +
    hysterectomy +
    Seen_GP_nerves_YNU +
    anxious_collapsed +
    loneliness_category +
    isolation_category +
    hearing_status_F,
  data = reproductive_clean_2
)

summary(cox_m3)
##---------------------------------------------
## Model 4: + Lifestyle
##---------------------------------------------
# smoking, alcohol, exercise, diet, sleep, vitamins

cox_m4 <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 +
    APOE4 +
    white_yn +
    deprivation_quintiles +
    tertiary_degree +
    age_menarche +
    number_births_group +
    oc_group +
    bilateral_oophorectomy +
    hysterectomy +
    Seen_GP_nerves_YNU +
    anxious_collapsed +
    loneliness_category +
    isolation_category +
    hearing_status_F +
    alcohol_status +
    smoker_status +
    exercise_category +
    HealthyDietScore_HDS +
    sleep_hours+
    Vitamins_YN,
  data = reproductive_clean_2
)


summary(cox_m4)
##-------------------------------------------------
## Model 5: + Health & clinical (Full model)
##-------------------------------------------------
# anthropometry, meds, cancer, cardiometabolic treatment,
# baseline cognition, HRT

cox_m5 <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 +
    APOE4 +
    white_yn +
    deprivation_quintiles +
    tertiary_degree +
    age_menarche +
    number_births_group +
    oc_group +
    bilateral_oophorectomy +
    hysterectomy +
    Seen_GP_nerves_YNU +
    anxious_collapsed +
    loneliness_category +
    isolation_category +
    hearing_status_F +
    alcohol_status +
    smoker_status +
    exercise_category +
    HealthyDietScore_HDS +
    sleep_hours +
    Vitamins_YN +
    hrt_group+
    waist_over80 +
    number_meds +
    Cancer_YNU +
    current_bp +
    current_cholesterol +
    current_insulin,
  data = reproductive_clean_2
)

summary(cox_m5)

library(car)

vif_m5 <- lm(
  age_baseline ~
    meno_group3 +
    APOE4 +
    white_yn +
    deprivation_quintiles +
    tertiary_degree +
    age_menarche +
    number_births_group +
    oc_group +
    bilateral_oophorectomy +
    hysterectomy +
    Seen_GP_nerves_YNU +
    anxious_collapsed +
    loneliness_category +
    isolation_category +
    hearing_status_F +
    alcohol_status +
    smoker_status +
    exercise_category +
    HealthyDietScore_HDS +
    sleep_hours +
    Vitamins_YN +
    hrt_group +
    waist_over80 +
    number_meds +
    Cancer_YNU +
    current_bp +
    current_cholesterol +
    current_insulin,
  data = reproductive_clean_rm
)

vif(vif_m5)


# this is your fully adjusted model


ha <- cox.zph(cox_m5)
ha

all_mods <- tbl_merge(
  tbls = list(
    tbl_regression(
      cox_m1,
      exponentiate = TRUE,
      label = list(meno_group3 ~ "Age at Menopause")
    ),
    tbl_regression(
      cox_m2,
      exponentiate = TRUE,
      label = list(meno_group3 ~ "Age at Menopause")
    ),
    tbl_regression(
      cox_m3,
      exponentiate = TRUE,
      label = list(meno_group3 ~ "Age at Menopause")
    ),
    tbl_regression(
      cox_m4,
      exponentiate = TRUE,
      label = list(meno_group3 ~ "Age at Menopause")
    ),
    tbl_regression(
      cox_m5,
      exponentiate = TRUE,
      label = list(meno_group3 ~ "Age at Menopause")
    )
  ),
  tab_spanner = c(
    "**Model 1: Crude**",
    "**Model 2: + Core Confounders**",
    "**Model 3: + Psychosocial**",
    "**Model 4: + Lifestyle Factors**",
    "**Model 5: + Clinical Mediators**"
  )
)

all_mods

library(flextable)
library(officer)

# convert to flextable
ft <- as_flex_table(all_mods)

# put into a Word doc
doc <- read_docx() |>
  body_add_flextable(ft)

print(doc, target = "all_models_age.docx")

#write.csv(reproductive_clean, "reproductive_naomit.csv", row.names = FALSE)
### Next steps, interogate missing data
# core covars in all intercation models

################## INTERACTION MODELS ########################
#### Hearing problems 


cox_hearing_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * hearing_status_F +
    APOE4 +tertiary_degree+
    white_yn +deprivation_quintiles+
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary (cox_hearing_int)


### smoking 


cox_smoke_int<- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * smoker_status +
    APOE4 +tertiary_degree+
    white_yn +deprivation_quintiles+
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary (cox_smoke_int)


### alcohol


cox_alcohol_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * alcohol_status +
    APOE4 + tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_alcohol_int)


### diet

cox_diet_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3*HealthyDietScore_HDS +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_diet_int)


### exercise


cox_exercise_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * exercise_category +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_exercise_int)


### sleep

cox_sleep_int <- coxph(
  Surv(age_baseline, age_at_event,  event_status) ~
    meno_group3 * sleep_hours +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_sleep_int)

### depression

cox_depr_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 *Seen_GP_nerves_YNU  +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_depr_int)

### anxiety

cox_anx_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 *anxious_collapsed  +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_anx_int)



### Loneliness 

cox_lone_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 *loneliness_category+
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_lone_int)


### Isolation

cox_iso_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 *isolation_category  +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group + 
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_iso_int)

#### HRT 

cox_hrt_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * hrt_group +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_hrt_int)

#### Vitamins


cox_vits_int <- coxph(
  Surv(age_baseline, age_at_event, event_status) ~
    meno_group3 * Vitamins_YN +
    APOE4 +tertiary_degree+
    white_yn + deprivation_quintiles +
    age_menarche + number_births_group + oc_group +
    bilateral_oophorectomy + hysterectomy,
  data = reproductive_clean_2
)

summary(cox_vits_int)


#write.csv(reproductive_clean_rm, 'final_1.csv')

#write.csv(reproductive_clean_imputed, 'final_imp_1.csv')
library(broom)
library(stringr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)

# Helper: extract and clean meno_group3 × exposure interaction terms
make_int_df <- function(model_list, model_order = NULL) {
  
  df <- map_df(names(model_list), function(mname) {
    tidy(model_list[[mname]]) |>
      filter(str_detect(term, "meno_group3") & str_detect(term, ":")) |>
      mutate(model = mname)
  })
  
  if (is.null(model_order)) model_order <- names(model_list)
  
  df |>
    separate(term, into = c("meno_term", "mod_term"), sep = ":", remove = FALSE) |>
    mutate(
      # Menopause group
      meno_group = case_when(
        str_detect(meno_term, "Premature") ~ "Premature (<40)",
        str_detect(meno_term, "Early")     ~ "Early (40–44)",
        str_detect(meno_term, "Late")      ~ "Late (55+)",
        TRUE                               ~ "Average (45–54)"
      ),
      # Human-readable label for the exposure level
      modifier = case_when(
        str_detect(mod_term, "^hearing_aids")        ~ str_replace(mod_term, "^hearing_aids", "Hearing aids: "),
        str_detect(mod_term, "^Seen_GP_nerves_YNU")  ~ "Seen GP nerves: Yes",
        str_detect(mod_term, "^anxious_collapsed")   ~ "Anxiety: Yes",
        str_detect(mod_term, "^loneliness_category") ~ str_replace(mod_term, "^loneliness_category", "Loneliness: "),
        str_detect(mod_term, "^isolation_category")  ~ str_replace(mod_term, "^isolation_category", "Isolation: "),
        str_detect(mod_term, "^processing_speed_100")~ "Processing speed (per 100ms slower)",
        
        str_detect(mod_term, "^smoker_status")       ~ str_replace(mod_term, "^smoker_status", "Smoking: "),
        str_detect(mod_term, "^alcohol_status")      ~ str_replace(mod_term, "^alcohol_status", "Alcohol: "),
        str_detect(mod_term, "^HealthyDietScore_HDS")~ "Healthy diet score (per unit)",
        str_detect(mod_term, "^exercise_category")   ~ str_replace(mod_term, "^exercise_category", "Exercise: "),
        
        str_detect(mod_term, "^hrt_group")           ~ str_replace(mod_term, "^hrt_group", "HRT: "),
        str_detect(mod_term, "^Vitamins_YN")         ~ "Vitamins: Yes",
        
        TRUE                                         ~ mod_term
      ),
      modifier = str_wrap(modifier, width = 20),
      
      # Interaction label for plotting
      int_label = paste0(meno_group, " × ", modifier),
      
      hr      = exp(estimate),
      hr_low  = exp(estimate - 1.96 * std.error),
      hr_high = exp(estimate + 1.96 * std.error),
      
      meno_group = factor(
        meno_group,
        levels = c("Premature (<40)", "Early (40–44)", "Late (55+)")
      ),
      model = factor(model, levels = model_order)
    )
}

# Helper: make a forest plot for a given set of models
plot_interactions <- function(int_df, title) {
  ggplot(
    int_df,
    aes(
      x = hr,
      y = fct_reorder(int_label, hr),
      colour = meno_group
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_pointrange(aes(xmin = hr_low, xmax = hr_high),
                    position = position_dodge(width = 0.6),
                    size = 0.4) +
    scale_x_log10() +
    facet_wrap(~ model, scales = "free_y", ncol = 2) +
    labs(
      x = "Hazard ratio (log scale)",
      y = "",
      title = title,
      subtitle = "Cox models for incident dementia"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 10),
      axis.text.y = element_text(size = 7)
    )
}

# 1) Psychosocial factors -----------------------------------------------

psychosocial_models <- list(
  Hearing     = cox_hearing_int,
  Depression  = cox_depr_int,
  Anxiety     = cox_anx_int,
  Loneliness  = cox_lone_int,
  Isolation   = cox_iso_int
)

ints_psych <- make_int_df(psychosocial_models)

plot_interactions(ints_psych,
                  title = "Interactions of menopause age with psychosocial factors")

# 2) Health behaviours ---------------------------------------------------

behaviour_models <- list(
  Smoking   = cox_smoke_int,
  Alcohol   = cox_alcohol_int,
  Diet      = cox_diet_int,
  Exercise  = cox_exercise_int,
  # You could optionally add Sleep / Adiposity here if you want:
  Sleep     = cox_sleep_int
)

ints_behav <- make_int_df(behaviour_models)

plot_interactions(ints_behav,
                  title = "Interactions of menopause age with health behaviours")

# 3) Medications / supplements -------------------------------------------

med_models <- list(
  HRT      = cox_hrt_int,
  Vitamins = cox_vits_int
)

ints_med <- make_int_df(med_models)

plot_interactions(ints_med,
                  title = "Interactions of menopause age with HRT and vitamins")


unique(reproductive_clean_rm$exercise_category)

summary(cox_m5)
covars_df <- tidy(cox_m5) |>
  filter(!str_detect(term, ":")) |>
  mutate(
    label = case_when(
      str_detect(term, "^meno_group3Premature") ~ "Premature menopause (<40)",
      str_detect(term, "^meno_group3Early")     ~ "Early menopause (40–44)",
      str_detect(term, "^meno_group3Late")      ~ "Late menopause (55+)",
      
      term == "exercise_category≥3000 MET-min/week (high)" ~
        "Heavy exercise vs low",
      term == "exercise_category600–3000 MET-min/week (medium)" ~
        "Moderate exercise vs low",
      
      term == "age_baseline"                    ~ "Age at baseline (per year)",
      term == "APOE4Y"                          ~ "APOE4 carrier",
      term == "tertiary_degreeY"                ~ "Tertiary degree",
      term == "smoker_statusNever"              ~ "Smoker status: Never",
      term == "smoker_statusPrevious"           ~ "Smoker status: Previous",
      term == "sleep_hours"                     ~ "Average sleep duration (hours)",
      term == "Cancer_YNUYes"                   ~ "Had cancer",
      term == "processing_speed_100"            ~ "Processing speed (seconds)",
      term == "current_bpY"                     ~ "Taking blood pressure medication",
      term == "current_insulinY"                ~ "Taking insulin",
      term == "current_cholesterolY"            ~ "Taking lipid-lowering medication",
      term == "Vitamins_YNY"                    ~ "Taking dietary supplements",
      term == "age_menarche"                    ~ "Age at menarche",
      term == "waist_over80Yes"                 ~ "High adiposity (waist >80 cm)",
      term == "HealthyDietScore_HDS"            ~ "Healthy Diet Score",
      term == "loneliness_categorymost lonely"            ~ "Very lonely",
      term == "loneliness_categorymoderately lonely"            ~ "Moderately lonely",
      term == "isolation_categorymoderately isolated"            ~ "Moderately isolated",
      term == "isolation_categorymost isolated"            ~ "Very isolated",
      term == "Seen_GP_nerves_YNUYes"            ~ "Seen GP for mental health",
      term == "hearing_aidsYes"            ~ "Uses hearing aids",
      term == "anxious_collapsedYes"            ~ "Experiences anxiety",
      term == "alcohol_statusNever"            ~ "Alcohol status: Never",
      term == "alcohol_statusPrevious"            ~ "Alcohol status: Previous",
      term == "number_meds"            ~ "Number of medications taken",
      str_detect(term, "^deprivation_quintiles") ~
        str_replace(term, "^deprivation_quintiles", "Deprivation: "),
      term == "white_ynY"                       ~ "White ethnicity",
      str_detect(term, "^number_births_group")  ~
        str_replace(term, "^number_births_group", "Parity: "),
      str_detect(term, "^oc_group")             ~
        str_replace(term, "^oc_group", "OC use: "),
      str_detect(term, "^hrt_group")            ~
        str_replace(term, "^hrt_group", "HRT use: "),
      term == "bilateral_oophorectomyY"         ~ "Bilateral oophorectomy",
      term == "hysterectomyY"                   ~ "Hysterectomy",
      TRUE                              ~ term
    ),
    hr      = exp(estimate),
    hr_low  = exp(estimate - 1.96 * std.error),
    hr_high = exp(estimate + 1.96 * std.error),
    
    # flag significance: CI not crossing 1 (equivalently p < 0.05)
    sig = case_when(
      p.value < 0.05        ~ "p < 0.05",
      p.value < 0.10        ~ "p < 0.07",   # trend
      TRUE                  ~ "≥ 0.10"
    ),
    sig = factor(sig, levels = c("p < 0.05", "p < 0.07", "≥ 0.10"))
  )
# or: sig = if_else(hr_low > 1 | hr_high < 1, "Significant", "Not significant")



bigun <- ggplot(
  covars_df,
  aes(x = hr, y = fct_reorder(label, hr), colour = sig)
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(xmin = hr_low, xmax = hr_high), size = 0.4) +
  scale_x_log10() +
  scale_colour_manual(
    values = c(
      "p < 0.05"   = "firebrick",   # dark red = significant
      "p < 0.07"  = "salmon",      # lighter red = trend
      "≥ 0.10"     = "grey60"       # grey = not significant
    ),name = ""
  ) +
  labs(
    x = "Hazard ratio (log scale)",
    y = "",
    colour = "",
    title = "Fully Adjusted Cox model"
  ) +
  theme_minimal(base_size = 11)+
  theme(
    axis.text.y = element_text(size = 9, lineheight = 1.5),  # increases spacing
    plot.margin = margin(10, 10, 10, 20)  # more room on left
  )

bigun

p_psych  <- plot_interactions(ints_psych,
                              title = "Interactions of menopause age with psychosocial factors")

p_behav  <- plot_interactions(ints_behav,
                              title = "Interactions of menopause age with health behaviours")

p_med    <- plot_interactions(ints_med,
                              title = "Interactions of menopause age with HRT and vitamins")


ggsave("meno_psychosocial_interactions.png", p_psych,
       width = 8, height = 6, dpi = 300)

ggsave("meno_behaviour_interactions.png", p_behav,
       width = 8, height = 6, dpi = 300)

ggsave("meno_medication_interactions.png", p_med,
       width = 8, height = 6, dpi = 300)

ggsave("bigun_amended.png", bigun,
       width = 8, height = 6, dpi = 300)



### interaction table 

extract_interactions <- function(model, exposure_prefix) {
  
  # tidy model output WITH confidence intervals
  df <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::filter(grepl("meno_group3", term) & grepl(exposure_prefix, term))
  
  if (nrow(df) == 0) return(df)
  
  df %>%
    dplyr::mutate(
      part1 = sub(":.*$", "", term),
      part2 = sub(".*:", "", term),
      
      # menopause group: remove prefix and leave text
      meno_group = sub("^meno_group3", "", part1),
      
      # exposure level: remove prefix from part2
      exposure_level = sub(paste0("^", exposure_prefix), "", part2),
      
      exposure = exposure_prefix,
      
      HR = exp(estimate),
      CI_low = exp(conf.low),
      CI_high = exp(conf.high),
      p_value = p.value
    ) %>%
    dplyr::select(
      exposure,
      exposure_level,
      meno_group,
      HR,
      CI_low,
      CI_high,
      p_value
    )
}


tbl_hearing <- extract_interactions(cox_hearing_int, "hearing_status_F")
tbl_smoke   <- extract_interactions(cox_smoke_int, "smoker_status")
tbl_alcohol <- extract_interactions(cox_alcohol_int, "alcohol_status")
tbl_diet    <- extract_interactions(cox_diet_int, "HealthyDietScore_HDS")
tbl_exercise<- extract_interactions(cox_exercise_int, "exercise_category")
tbl_sleep   <- extract_interactions(cox_sleep_int, "sleep_hours")
tbl_dep     <- extract_interactions(cox_depr_int, "Seen_GP_nerves_YNU")
tbl_anx     <- extract_interactions(cox_anx_int, "anxious_collapsed")
tbl_lone    <- extract_interactions(cox_lone_int, "loneliness_category")
tbl_iso     <- extract_interactions(cox_iso_int, "isolation_category")
tbl_hrt     <- extract_interactions(cox_hrt_int, "hrt_group")
tbl_vits    <- extract_interactions(cox_vits_int, "Vitamins_YN")

interaction_table <- bind_rows(
  tbl_hearing,
  tbl_smoke,
  tbl_alcohol,
  tbl_diet,
  tbl_exercise,
  tbl_sleep,
  tbl_dep,
  tbl_anx,
  tbl_lone,
  tbl_iso,
  tbl_hrt,
  tbl_vits
)

head(tbl_iso)

interaction_table_wide <- interaction_table %>%
  mutate(
    hr_ci = sprintf("%.2f (%.2f–%.2f), p=%.3f", HR, CI_low, CI_high, p_value)
  ) %>%
  dplyr::select(exposure, exposure_level, meno_group, HR, CI_low, CI_high, p_value) %>%
  tidyr::pivot_wider(
    names_from = meno_group,
    values_from = HR
  )


head(interaction_table)


interaction_table <- interaction_table %>%
  mutate(
    variable = case_when(
      grepl("hearing", exposure) ~ "Hearing Loss",
      grepl("smoker", exposure) ~ "Smoking",
      grepl("alcohol", exposure) ~ "Alcohol",
      grepl("HealthyDiet", exposure) ~ "Diet",
      grepl("exercise", exposure) ~ "Exercise",
      grepl("sleep", exposure) ~ "Sleep",
      grepl("Seen_GP_nerves", exposure) ~ "Mental Health Seeking",
      grepl("anxious", exposure) ~ "Anxious Feelings",
      grepl("loneliness", exposure) ~ "Loneliness",
      grepl("isolation", exposure) ~ "Isolation",
      grepl("hrt_group", exposure) ~ "HRT",
      grepl("Vitamins", exposure) ~ "Supplements",
      TRUE ~ "Other"
    ),
    label = paste0(variable, ": ", exposure_level)
  )


interaction_table <- interaction_table %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "Smoking", "Alcohol", "Diet", "Exercise",
        "Sleep", "Mental Health Seeking", "Anxious Feelings",
        "Loneliness", "Isolation", "Hearing Loss", "HRT", "Supplements"
      )
    )
  )

head(interaction_table)

interaction_table <- interaction_table %>%
  arrange(variable, exposure_level)

interaction_table2 <- interaction_table %>%
  mutate(
    sig_colour = case_when(
      p_value < 0.05 ~ "p < 0.05",
      p_value < 0.10 ~ "p < 0.10",
      TRUE ~ "NS"
    )
  )

library(dplyr)
library(forcats)
library(stringr)

# 1) Define a logical order for variables (as you already did)
var_levels <- c(
  "Supplements", "HRT",
  "Hearing Loss",
  "Isolation", "Loneliness",
  "Anxious Feelings", "Mental Health Seeking",
  "Sleep", "Exercise", "Diet",
  "Alcohol", "Smoking"
)

# 2) Define level order rules within each variable
#    (edit these strings to match your exposure_level values)
interaction_table2 <- interaction_table2 %>%
  mutate(
    variable = factor(variable, levels = var_levels),
    
    level_order = case_when(
      variable == "HRT" ~ case_when(
        str_detect(exposure_level, "None|No") ~ 1,
        str_detect(exposure_level, "<\\s*1|<1") ~ 2,
        str_detect(exposure_level, "1\\s*[-–]\\s*5") ~ 3,
        str_detect(exposure_level, "5\\s*[-–]\\s*10") ~ 4,
        str_detect(exposure_level, ">\\s*10|≥\\s*10|10\\+") ~ 5,
        TRUE ~ 99
      ),
      
      variable == "Supplements" ~ case_when(
        str_detect(exposure_level, "N|No") ~ 1,
        str_detect(exposure_level, "Y|Yes") ~ 2,
        TRUE ~ 99
      ),
      
      variable == "Smoking" ~ case_when(
        str_detect(exposure_level, "Never") ~ 1,
        str_detect(exposure_level, "Previous|Former|Ex") ~ 2,
        str_detect(exposure_level, "Current") ~ 3,
        TRUE ~ 99
      ),
      
      variable == "Alcohol" ~ case_when(
        str_detect(exposure_level, "Never") ~ 1,
        str_detect(exposure_level, "Previous|Former") ~ 2,
        str_detect(exposure_level, "Current") ~ 3,
        TRUE ~ 99
      ),
      
      variable == "Exercise" ~ case_when(
        str_detect(exposure_level, "Low|<") ~ 1,
        str_detect(exposure_level, "medium|600") ~ 2,
        str_detect(exposure_level, "high|≥|3000|>") ~ 3,
        TRUE ~ 99
      ),
      
      variable == "Sleep" ~ case_when(
        str_detect(exposure_level, "<\\s*6|short") ~ 1,
        str_detect(exposure_level, "6\\s*[-–]\\s*8|normal") ~ 2,
        str_detect(exposure_level, ">\\s*8|long") ~ 3,
        TRUE ~ 99
      ),
      
      variable %in% c("Loneliness", "Isolation") ~ case_when(
        str_detect(exposure_level, "Not") ~ 1,
        str_detect(exposure_level, "moderately") ~ 2,
        str_detect(exposure_level, "most") ~ 3,
        TRUE ~ 99
      ),
      
      variable == "Hearing Loss" ~ case_when(
        str_detect(exposure_level, "None|No") ~ 1,
        str_detect(exposure_level, "Subjective") ~ 2,
        str_detect(exposure_level, "Objective") ~ 3,
        TRUE ~ 99
      ),
      
      TRUE ~ 50
    ),
    
    # 3) Create a single ordering key
    order_key = as.integer(variable) * 100 + level_order,
    
    # Rebuild label after ordering fields exist
    label = paste0(as.character(variable), ": ", exposure_level)
  ) %>%
  arrange(order_key)

# 4) Freeze the label order explicitly (and reverse for nicer top-down reading)
label_levels <- rev(unique(interaction_table2$label))

plot_data <- interaction_table2 %>%
  mutate(
    meno_group = factor(meno_group,
                        levels = c("Premature (<40)", "Early (40–44)", "Average (45–55)", "Late (55+)")
    ),
    label = factor(label, levels = label_levels)
  )



plot_data <- interaction_table2 %>%
  mutate(
    meno_group = factor(
      meno_group,
      levels = c("Premature (<40)", "Early (40–44)", "Average (45–55)", "Late (55+)")
    ),
    label = fct_inorder(label)  # preserve order we created
  )

in_plt <- ggplot(
  plot_data,
  aes(
    x = HR,
    y = label,
    xmin = CI_low,
    xmax = CI_high,
    colour = sig_colour
  )
) +
  geom_point(size = 2.6) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(trans = "log10") +
  scale_colour_manual(
    values = c(
      "p < 0.05" = "darkgreen",
      "p < 0.10" = "goldenrod2",
      "NS" = "grey60"
    ),
    name = "Significance"
  ) +
  facet_wrap(~ meno_group, ncol = 2, scales = "free_y") +
  labs(
    x = "Hazard Ratio (log scale)",
    y = "Exposure (Variable: Level)",
    title = "Interaction Effects by Menopause Group",
    subtitle = "HR and 95% CI for modifiable exposures within menopause timing strata"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

in_plt

ggsave("interaction_plots.pdf", in_plt,
       width = 12, height = 10, dpi = 300)
