library(gtsummary)
library(dplyr)
library(officer)
library(flextable)

## 1. Order menopause groups (just ordering, no renaming)
reproductive_clean_rm_sum <- reproductive_clean_2 %>%
  mutate(
    meno_group3 = factor(
      meno_group3,
      levels = c(
        "Premature (<40)",
        "Early (40–44)",
        "Average (45–55)",
        "Late (55+)"
      )
    )
  )


## 3. Variables to summarise (only keep those that exist)
vars_to_summarise <- intersect(
  c(
    "meno_group3", "event_status", "time_to_event_years",
    "age_baseline", "white_yn","deprivation_quintiles",
    "tertiary_degree", "APOE4",
    "hearing_status_F", "sleep_hours",
    "smoker_status", "alcohol_status", "exercise_category",
    "HealthyDietScore_HDS", "Vitamins_YN",
    "Seen_GP_nerves_YNU", "anxious_collapsed",
    "loneliness_category", "isolation_category",
    "age_menarche", "number_births_group",
    "hrt_group", "oc_group",
    "bilateral_oophorectomy", "hysterectomy",
    "current_bp", "current_cholesterol",
    "current_insulin", "Cancer_YNU",
    "waist_over80", "number_meds"
  ),
  names(reproductive_clean_rm)
)

## 4. Build summary table
summary_table <- reproductive_clean_rm_sum %>%
  dplyr::select(all_of(vars_to_summarise)) %>%
  tbl_summary(
    by = meno_group3,
    type = list(
      all_continuous() ~ "continuous"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,   # sleep_hours etc shown as e.g. 7.12 (1.03)
    missing = "ifany",
    label = list(
      age_baseline              ~ "Baseline age",
      time_to_event_years       ~ "Follow-up time (years)",
      event_status              ~ "Dementia event",
      white_yn                  ~ "White ethnicity",
      APOE4                     ~ "APOE4 genotype",
      age_menarche              ~ "Age at menarche",
      number_births_group       ~ "Number of children",
      hrt_group                 ~ "HRT duration group",
      oc_group                  ~ "OC duration group",
      bilateral_oophorectomy    ~ "Bilateral oophorectomy",
      hysterectomy              ~ "Hysterectomy",
      hearing_status_F             ~ "Hearing loss",
      sleep_hours               ~ "Sleep duration (hours)",
      smoker_status             ~ "Smoking status",
      alcohol_status            ~ "Alcohol consumption",
      exercise_category         ~ "Exercise category",
      HealthyDietScore_HDS      ~ "Healthy Diet Score",
      Seen_GP_nerves_YNU        ~ "Mental health seeking",
      anxious_collapsed         ~ "Anxiety",
      loneliness_category       ~ "Loneliness",
      isolation_category        ~ "Social isolation",
      current_bp                ~ "Blood pressure medication",
      current_cholesterol       ~ "Cholesterol-lowering medication",
      current_insulin           ~ "Taking insulin",
      Cancer_YNU                ~ "Cancer diagnosis",
      waist_over80              ~ "Waist >80 cm",
      number_meds               ~ "Number of medications",
      Vitamins_YN               ~ "Dietary supplements"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  add_q(method = "BH") %>%
  modify_header(label ~ "Variable") %>%
  bold_labels()

summary_table


# convert to flextable
ft <- as_flex_table(summary_table)

# put into a Word doc
doc <- read_docx() |>
  body_add_flextable(ft)

print(doc, target = "summary_meno_groups.docx")