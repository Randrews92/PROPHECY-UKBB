install.packages("gtsummary")
install.packages("broom.mixed")
install.packages("tidyverse")
install.packages("glue")
library(gtsummary)
library(broom.mixed)
library(data.table)
library(tidyverse)
install.packages('car')
library(car) 
install.packages('afex')
install.packages('emmeans')
library(afex) 
library(emmeans) 
install.packages("mediation")
library(mediation)
library(survival)
install.packages('survminer')
library(survminer)
library(forcats)

## Variables in study:
# Age at menopause (Premature=<40, Early=40-44, Average=45-54, Late=55+)
# White ethnicity
# Townsend deprivation
# Education
# Number of births
# Age at menarche
# Hysterectomy 
# Bilateral oophorectomy 
# Oral contraceptive use
# Mental help seeking
# loneliness
# anxious feelings
# isolation 
# sleep duration
# diet score
# dietary supplement use
# HRT
# alcohol status
# adiposity
# smoking status
# Exercise
# high blood pressure medication
# hearing loss 
# insulin use
# cancer
# cholesterol lowering medication
# Polypharmacy
# Age at baseline
# Dementia status
# Age at followup
# Time to event
# APOE4 carriage


# Get age at menopause (Premature=<40, Early=40-44, Average=45-54, Late=55+)

reproductive <- read.csv('update25_participant.csv') #dataset with 273036 UKBB women

# Replace empty strings with NA
reproductive[reproductive == ""] <- NA


################ time to event/ event status 

#### Add new death, dementia/ loss to followup 

newdeath<- read.csv("newoutcomes25_death.csv")

newoc<- read.csv("newoutcomes25_participant.csv")

newdeath[newdeath == ""] <- NA
newoc[newoc == ""] <- NA

newdeath_earliest <- newdeath %>%
  mutate(Date.of.death = as.Date(Date.of.death)) %>%
  group_by(Participant.ID) %>%
  summarise(
    date_death25 = if (all(is.na(Date.of.death))) NA else max(Date.of.death, na.rm = TRUE),
    .groups = "drop"
  )

reproductive<- reproductive%>%
  left_join(newdeath_earliest%>%
              dplyr::select(Participant.ID, date_death25),
            by='Participant.ID')

newoc$Date.lost.to.follow.up25 <- newoc$Date.lost.to.follow.up

newoc$Date.of.all.cause.dementia.report25 <- newoc$newoc$Date.of.all.cause.dementia.report


newoc <- newoc %>%
  mutate(
    Date.of.all.cause.dementia.report =
      na_if(Date.of.all.cause.dementia.report, "Date is unknown"),
    Date.of.all.cause.dementia.report =
      na_if(Date.of.all.cause.dementia.report, "")
  )

newoc <- newoc %>%
  mutate(
    dementia_date25 = coalesce(
      as.Date(Date.of.all.cause.dementia.report),
      as.Date(Date.F00.first.reported..dementia.in.alzheimer.s.disease.),
      as.Date(Date.F01.first.reported..vascular.dementia.),
      as.Date(Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere.),
      as.Date(Date.F03.first.reported..unspecified.dementia.)
    )
  )

newoc <- newoc %>%
  mutate(
    Date.lost.to.follow.up25=
      na_if(Date.lost.to.follow.up, "")
  )


reproductive<- reproductive%>%
  left_join(newoc%>%
              dplyr::select(Participant.ID, Date.lost.to.follow.up25, dementia_date25),
            by='Participant.ID')

# time to event/ event status 

reproductive$End_Point <- as.Date("2025-07-01")

reproductive<- reproductive%>%
  mutate(
    across(c(Date.of.attending.assessment.centre...Instance.0,
             dementia_date25,
             date_death25,
             Date.lost.to.follow.up25,
             End_Point), ~ as.Date(.x)),
    
    # Determine event or censoring date
    event_date = case_when(
      !is.na(dementia_date25) ~ dementia_date25,
      is.na(dementia_date25) & !is.na(date_death25) ~ date_death25,
      is.na(dementia_date25) & is.na(date_death25) &
        !is.na(Date.lost.to.follow.up25) ~ Date.lost.to.follow.up25,
      TRUE ~ End_Point
    ),
    
    # Calculate time to event or censoring (years)
    time_to_event_years = as.numeric(
      difftime(event_date,
               Date.of.attending.assessment.centre...Instance.0,
               units = "days")
    ) / 365.25,
    
    # Dementia event indicator (1 = dementia, 0 = censored)
    event_status = if_else(!is.na(dementia_date25), 1, 0)
  )



# 273036 at start

summary(reproductive$time_to_event_years)
sum(reproductive$time_to_event_years < 0, na.rm = TRUE) # 103 people with prev dementia

reproductive<- reproductive%>%
  mutate(
    dementia_within5 = event_status == 1 & time_to_event_years <=5
  )

t <- reproductive%>%
  group_by(time_to_event_years,(event_status == 1))%>%
  summarise(cnt=n_distinct(Participant.ID))### 430 women with dementia in 5 years or less

reproductive%>%
  group_by(event_status)%>%
  summarise(cnt=n_distinct(Participant.ID))

# Remove 430 women with dementia prior to baseline or dementia within 5 years of baseline

reproductive_clean<- reproductive %>%
  filter(
    dementia_within5==FALSE,# drop early dementia
    !is.na(time_to_event_years),# drop missing time to event
    time_to_event_years >0       # drop any negative times
  )

sum(reproductive_clean$dementia_within5 < 0, na.rm = TRUE)

reproductive_clean%>%
  group_by(dementia_within5)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_clean<- reproductive_clean%>%
  mutate(
    # ensure dates are Date class
    baseline_date = as.Date(Date.of.attending.assessment.centre...Instance.0),
    event_date    = as.Date(event_date),
    age_baseline = Age.when.attended.assessment.centre...Instance.0,
    
    # compute age at event
    age_at_event = age_baseline+ as.numeric(event_date - baseline_date) / 365.25
  )

n_distinct(reproductive_clean$Participant.ID)# now we have 272606 women

## We'll remove all who said no, prefer not to answer, not sure other reason, and keep not sure hysterectomy

unique(reproductive_clean$Age.at.menopause..last.menstrual.period....Instance.0)

to_num_clean <- function(x) {
  x <- as.character(x)
  x[x %in% c("Do not know", "Prefer not to answer")] <- NA
  suppressWarnings(as.numeric(x))
}


reproductive_clean <- reproductive_clean %>%
  mutate(
    age_meno_natural = to_num_clean(`Age.at.menopause..last.menstrual.period....Instance.0`),
    age_bilateral_oophorectomy_combined = to_num_clean(`Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0`),
    age_hysterectomy_combined = to_num_clean(`Age.at.hysterectomy...Instance.0`)
  ) %>%
  mutate(
    age_menopause_combined = case_when(
      !is.na(age_meno_natural) & !is.na(age_bilateral_oophorectomy_combined) ~ 
        age_meno_natural,
      
      !is.na(age_meno_natural) ~ age_meno_natural,
      
      !is.na(age_bilateral_oophorectomy_combined) ~ 
        age_bilateral_oophorectomy_combined,
      
      TRUE ~ NA_real_
    )
  )


# recode meno age as factor

reproductive_clean<- reproductive_clean %>%
  mutate(
    meno_group3 = case_when(
      age_menopause_combined < 40 ~ "Premature (<40)",        # Premature Ovarian Insufficiency (POI)
      age_menopause_combined >= 40 & age_menopause_combined< 45 ~ "Early (40–44)", # Early menopause
      age_menopause_combined>= 45 & age_menopause_combined<= 55 ~ "Average (45–55)", # Typical menopause
      age_menopause_combined > 55 ~ "Late (55+)",             # Late menopause
      TRUE ~ NA_character_
    )
  )

reproductive_clean%>%
  group_by(meno_group3)%>%
  summarise(cnt=n_distinct(Participant.ID)) #missing: 105604


# download the diet and had_menopause variables 

diet<- read.csv('diet_participant.csv')

### Need to handle inconsistencies in those who reached menopause but no menopause age

diet$had_menopause <- diet$Had.menopause...Instance.0

reproductive_clean <- reproductive_clean %>%
  left_join(
    diet %>%
      dplyr::select(Participant.ID,
                    had_menopause),
    by = "Participant.ID"
  )


### Need to handle women who said they had not reached menopause or were not sure

reproductive_clean%>%
  group_by(had_menopause)%>%
  summarise(cnt=n_distinct(Participant.ID))

## remove No, NA, Not Sure-other reason


reproductive_rm<- reproductive_clean%>%filter(grepl("Yes|hyster", had_menopause)) 

## now we have a sample of 195977 women

# remove NA (29241) on menopause age on the remainder, leaving us with 166,736

reproductive_rm <- reproductive_rm %>%
  filter(!is.na(meno_group3))

# check groups

reproductive_rm%>%
  group_by(meno_group3)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_rm%>%
  group_by(had_menopause)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_rm%>%
  group_by(is.na(age_menopause_combined))%>%
  summarise(cnt=n_distinct(Participant.ID))

### Now we have 8286 premature menopause, 13715 late, 16768 early, 127967 average

# Add parity and durations of HRT/ OC:

# Get variable list

colnames(reproductive_rm)

## Helper for NA-like strings
na_strings <- c("Do not know", "Prefer not to answer", "")

## Helper to compute duration (start -> last), handling "Still taking" and NA-like strings
compute_duration <- function(age_start,
                             age_last,
                             age_at_assessment,
                             still_pattern = "Still taking") {
  
  start_chr <- as.character(age_start)
  last_chr  <- as.character(age_last)
  
  # If "Still taking..." then use age at assessment as last age
  last_num <- ifelse(
    !is.na(last_chr) & grepl(still_pattern, last_chr),
    as.numeric(age_at_assessment),
    suppressWarnings(as.numeric(last_chr))
  )
  
  start_num <- suppressWarnings(as.numeric(start_chr))
  
  # Remove "Do not know" / "Prefer not to answer" / "" etc.
  start_num[start_chr %in% na_strings] <- NA_real_
  last_num[last_chr  %in% na_strings]  <- NA_real_
  
  dur <- last_num - start_num
  
  # Negative durations -> NA
  dur[dur < 0] <- NA_real_
  
  dur
}

## Create years_hrt, hrt_group, years_oc, oc_group, and number_births_group:

reproductive_rm <- reproductive_rm %>%
  mutate(
    ## Years on HRT (Instance 0 only)
    years_hrt = compute_duration(
      `Age.started.hormone.replacement.therapy..HRT....Instance.0`,
      `Age.last.used.hormone.replacement.therapy..HRT....Instance.0`,
      `Age.when.attended.assessment.centre...Instance.0`,
      still_pattern = "Still taking"   # matches "Still taking HRT"
    ),
    
    hrt_group = case_when(
      years_hrt == 0                        ~ "≤1 year",
      years_hrt > 0  & years_hrt <= 5       ~ "≤5 years",
      years_hrt > 5  & years_hrt <= 10      ~ "6–10 years",
      years_hrt > 10                        ~ ">10 years",
      TRUE                                  ~ "None"
    ),
    
    ## Years on oral contraceptive (Instance 0 only)
    years_oc = compute_duration(
      `Age.started.oral.contraceptive.pill...Instance.0`,
      `Age.when.last.used.oral.contraceptive.pill...Instance.0`,
      `Age.when.attended.assessment.centre...Instance.0`,
      still_pattern = "Still taking"   # matches "Still taking the pill"
    ),
    
    oc_group = case_when(
      years_oc == 0                        ~ "≤1 year",
      years_oc > 0  & years_oc <= 5        ~ "≤5 years",
      years_oc > 5  & years_oc <= 10       ~ "6–10 years",
      years_oc > 10                        ~ ">10 years",
      TRUE                                 ~ "None"
    ),
    
    ## Number of live births (Instance 0 only)
    births0_clean = ifelse(
      `Number.of.live.births...Instance.0` %in% c("", "Prefer not to answer", "Do not know"),
      NA,
      `Number.of.live.births...Instance.0`
    ),
    births0_clean = as.numeric(births0_clean),
    
    number_live_births_max = births0_clean,
    
    number_births_group = case_when(
      number_live_births_max < 1                 ~ "No children",
      number_live_births_max == 1                ~ "One child",
      number_live_births_max == 2                ~ "Two children",
      number_live_births_max == 3                ~ "Three children",
      number_live_births_max > 3                 ~ "More than three",
      is.na(number_live_births_max)              ~ NA_character_
    ),
    
    number_births_group = factor(
      number_births_group,
      levels = c("No children","One child","Two children","Three children","More than three")
    )
  )


### Check numbers 

reproductive_rm%>%
  group_by(number_births_group)%>%
  summarise(cnt=n_distinct(Participant.ID))


reproductive_rm%>%
  group_by(hrt_group)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_rm%>%
  group_by(oc_group)%>%
  summarise(cnt=n_distinct(Participant.ID))


# Create Quals

# Create a Qualifications score with 5 highest and 0 lowest quals

reproductive_rm$Qualifications <- ifelse(
  grepl("College or University degree", reproductive_rm$Qualifications...Instance.0), 5,
  ifelse(grepl("A levels/AS levels or equivalent", reproductive_rm$Qualifications...Instance.0), 4,
         ifelse(grepl("NVQ or HND or HNC or equivalent", reproductive_rm$Qualifications...Instance.0), 3,
                ifelse(grepl("O levels/GCSEs or equivalent", reproductive_rm$Qualifications...Instance.0), 2,
                       ifelse(grepl("CSEs or equivalent", reproductive_rm$Qualifications...Instance.0), 1,
                              ifelse(grepl("Other professional qualifications eg: nursing, teaching", reproductive_rm$Qualifications...Instance.0), 5,
                                     ifelse(grepl("None of the above", reproductive_rm$Qualifications...Instance.0), 0, reproductive_rm$Qualifications...Instance.0)
                              )
                       )
                )
         )
  )
)

reproductive_rm$tertiary_degree<- ifelse(reproductive_rm$Qualifications == 5, "Y", "N")

reproductive_rm%>%
  group_by(tertiary_degree)%>%
  summarise(cnt=n_distinct(Participant.ID))

# Ethnicity 

unique(reproductive_rm$Ethnic.background...Instance.0)

reproductive_rm$Ethnicity <- ifelse(grepl("^(Mixed|White and Black Caribbean|White and Black African|White and Asian|Any other mixed background)$", reproductive_rm$Ethnic.background...Instance.0), 'Mixed',
                                 ifelse(grepl("^(White|British|Irish|Any other white background)$", reproductive_rm$Ethnic.background...Instance.0), 'White',
                                        ifelse(grepl("^(Asian or Asian British|Indian|Pakistani|Bangladeshi|Any other Asian background)$", reproductive_rm$Ethnic.background...Instance.0), "Asian",
                                               ifelse(grepl("^(Black or Black British|Caribbean|African|Any other Black background)$", reproductive_rm$Ethnic.background...Instance.0), "Black",
                                                      ifelse(grepl("^Chinese$", reproductive_rm$Ethnic.background...Instance.0), "Chinese",
                                                             ifelse(grepl("^Other ethnic group$", reproductive_rm$Ethnic.background...Instance.0), "Other ethnic group", reproductive_rm$Ethnic.background...Instance.0)
                                                      )
                                               )
                                        )
                                 )
)



reproductive_rm$white_yn <- ifelse(reproductive_rm$Ethnicity == "White", "Y", "N")


reproductive_rm%>%
  group_by(white_yn)%>%
  summarise(cnt=n_distinct(Participant.ID))

## Create new diet score Healthy Diet Score (HDS)

#dx download diet_participant.csv

colnames(diet)

# Start from your diet dataset
# diet has:
# "Cooked.vegetable.intake...Instance.0"
# "Salad...raw.vegetable.intake...Instance.0"
# "Fresh.fruit.intake...Instance.0"
# "Dried.fruit.intake...Instance.0"
# "Oily.fish.intake...Instance.0"
# "Non.oily.fish.intake...Instance.0"
# "Processed.meat.intake...Instance.0"
# "Beef.intake...Instance.0"
# "Lamb.mutton.intake...Instance.0"
# "Pork.intake...Instance.0"

## 1) Helper: turn "Less than one" into 0 and then numeric for tbsp/day
lt1_to_num <- function(x) {
  x <- as.character(x)
  x[x == "Less than one"] <- "0"
  x[x %in% c("Do not know", "Prefer not to answer", "")] <- NA
  suppressWarnings(as.numeric(x))
}

## 2) Helper: map UKB frequency categories to approx times/week
freq_to_week <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "Never"                   ~ 0,
    x == "Less than once a week"   ~ 0.5,
    x == "Once a week"             ~ 1,
    x == "2-4 times a week"        ~ 3,
    x == "5-6 times a week"        ~ 5.5,
    x == "Once or more daily"      ~ 7,
    x %in% c("Do not know",
             "Prefer not to answer",
             "")                   ~ NA_real_,
    TRUE                           ~ NA_real_   # catch anything unexpected
  )
}

## 3) Build the score inside your `diet` data frame
diet <- diet %>%
  mutate(
    # ---------- FRUIT & VEG (heaped tbsp/day) ----------
    Fresh_Fruit = lt1_to_num(Fresh.fruit.intake...Instance.0),
    Dried_Fruit = lt1_to_num(Dried.fruit.intake...Instance.0),
    Salad_tbsp  = lt1_to_num(Salad...raw.vegetable.intake...Instance.0),
    Veg_tbsp    = lt1_to_num(Cooked.vegetable.intake...Instance.0),
    
    # Total veg = raw + cooked
    Veg_total_tbsp = Salad_tbsp + Veg_tbsp,
    
    # Fruit total: fresh + half-weighted dried fruit
    Fruit_total = Fresh_Fruit + 0.5 * Dried_Fruit
  ) %>%
  # ---------- FISH & MEAT (times/week) ----------
mutate(
  across(
    c(
      "Oily.fish.intake...Instance.0",
      "Non.oily.fish.intake...Instance.0",
      "Processed.meat.intake...Instance.0",
      "Beef.intake...Instance.0",
      "Lamb.mutton.intake...Instance.0",
      "Pork.intake...Instance.0"
    ),
    freq_to_week,
    .names = "{.col}_week"
  )
) %>%
  mutate(
    # Total fish = oily + non-oily
    Fish_times_week = Oily.fish.intake...Instance.0_week +
      Non.oily.fish.intake...Instance.0_week,
    
    # Unprocessed red meat = beef + lamb + pork
    Red_meat_times_week =
      Beef.intake...Instance.0_week +
      Lamb.mutton.intake...Instance.0_week +
      Pork.intake...Instance.0_week,
    
    # If all red-meat components missing, keep NA rather than 0
    Red_meat_times_week = ifelse(
      is.na(Beef.intake...Instance.0_week) &
        is.na(Lamb.mutton.intake...Instance.0_week) &
        is.na(Pork.intake...Instance.0_week),
      NA_real_,
      Red_meat_times_week
    ),
    
    Proc_meat_week = Processed.meat.intake...Instance.0_week
  ) %>%
  # ---------- HDS COMPONENTS (0/1) ----------
mutate(
  # 1) Vegetables ≥ 4 heaped tbsp/day
  HDS_veg = case_when(
    is.na(Veg_total_tbsp) ~ NA_real_,
    Veg_total_tbsp >= 4   ~ 1,
    TRUE                  ~ 0
  ),
  
  # 2) Fruit ≥ 3 “pieces”/day (here using Fruit_total)
  HDS_fruit = case_when(
    is.na(Fruit_total) ~ NA_real_,
    Fruit_total >= 3   ~ 1,
    TRUE               ~ 0
  ),
  
  # 3) Fish ≥ 2 times/week
  HDS_fish = case_when(
    is.na(Fish_times_week) ~ NA_real_,
    Fish_times_week >= 2   ~ 1,
    TRUE                   ~ 0
  ),
  
  # 4) Unprocessed red meat ≤ 2 times/week
  HDS_unproc_red = case_when(
    is.na(Red_meat_times_week) ~ NA_real_,
    Red_meat_times_week <= 2   ~ 1,
    TRUE                       ~ 0
  ),
  
  # 5) Processed meat ≤ 2 times/week
  HDS_proc_meat = case_when(
    is.na(Proc_meat_week) ~ NA_real_,
    Proc_meat_week <= 2   ~ 1,
    TRUE                  ~ 0
  )
) %>%
  # ---------- TOTAL 0–5 SCORE + BINARY VERSION ----------
mutate(
  HealthyDietScore_HDS = ifelse(
    rowSums(is.na(cbind(
      HDS_veg,
      HDS_fruit,
      HDS_fish,
      HDS_unproc_red,
      HDS_proc_meat
    ))) > 0,
    NA_real_,
    HDS_veg + HDS_fruit + HDS_fish + HDS_unproc_red + HDS_proc_meat
  ),
  HealthyDiet_HDS_bin = case_when(
    is.na(HealthyDietScore_HDS)  ~ NA_character_,
    HealthyDietScore_HDS >= 4    ~ "Higher",
    HealthyDietScore_HDS <  4    ~ "Lower"
  ),
  HealthyDiet_HDS_bin = factor(HealthyDiet_HDS_bin,
                               levels = c("Higher", "Lower"))
)


reproductive_rm <- reproductive_rm %>%
  left_join(
    diet %>%
      dplyr::select(Participant.ID,
                    HealthyDietScore_HDS,
                    HealthyDiet_HDS_bin),
    by = "Participant.ID"
  )

reproductive_rm%>%
  group_by(HealthyDietScore_HDS)%>%
  summarise(cnt=n_distinct(Participant.ID))

############# hearing probs 

deaf <- read.csv("deaf_participant.csv")

colnames(reproductive_rm)

unique(deaf$Hearing.aid.user...Instance.0)
unique(deaf$Hearing.difficulty.problems...Instance.0)
unique(deaf$Hearing.difficulty.problems.with.background.noise...Instance.0)

reproductive_rm$hearing_loss <- NULL
reproductive_rm$deafness <- NULL
reproductive_rm$hearing_collapsed <- NULL
reproductive_rm$hearing_difficulty.x <- NULL
reproductive_rm$hearing_difficulty.y <- NULL
reproductive_rm$hearing_aids <- NULL
reproductive_rm$hearing_probs<- NULL
reproductive_rm$hearing_problems<- NULL
reproductive_rm$hearing_severity<- NULL
reproductive_rm$Hearing.difficulty.problems...Instance.0<- NULL
reproductive_rm$Hearing.aid.user...Instance.0<- NULL


reproductive_rm <- reproductive_rm %>%
  left_join(
    deaf,
    by = "Participant.ID"
  )

T <- reproductive_rm %>%
  group_by(Hearing.difficulty.problems...Instance.0, Hearing.difficulty.problems..pilot....Instance.0, Hearing.aid.user...Instance.0, Hearing.difficulty.problems.with.background.noise...Instance.0)%>%
  summarise(cnt=n_distinct(Participant.ID))

ambig_vals <- c("Do not know", "Prefer not to answer")

reproductive_rm <- reproductive_rm %>%
  mutate(
    hearing_status = case_when(
      # 2 = objective hearing loss: aid user OR completely deaf (either main or pilot)
      Hearing.aid.user...Instance.0 == "Yes" |
        Hearing.difficulty.problems...Instance.0 == "I am completely deaf" |
        Hearing.difficulty.problems..pilot....Instance.0 == "I am completely deaf" ~ 2L,
      
      # 1 = subjective hearing difficulty (but not in group 2)
      Hearing.difficulty.problems...Instance.0 == "Yes" |
        Hearing.difficulty.problems..pilot....Instance.0 == "Yes" |
        Hearing.difficulty.problems.with.background.noise...Instance.0 == "Yes" ~ 1L,
      
      # 0 = at least one definite "No" and no evidence of deafness/aid/subjective difficulty
      Hearing.aid.user...Instance.0 == "No" |
        Hearing.difficulty.problems...Instance.0 == "No" |
        Hearing.difficulty.problems..pilot....Instance.0 == "No" |
        Hearing.difficulty.problems.with.background.noise...Instance.0 == "No" ~ 0L,
      
      # Everything else (only ambiguous responses / NA) → NA
      TRUE ~ NA_integer_
    )
  )

unique(reproductive_rm$hearing_status)

reproductive_rm%>%
  group_by(hearing_status)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_rm$hearing_status_F <- as.factor(reproductive_rm$hearing_status)

levels(reproductive_rm$hearing_status_F)

levels(reproductive_rm$hearing_status_F) <- 
  c("No hearing problems", "Subjective difficulty", "Objective hearing loss")


reproductive_rm%>%
  group_by(hearing_status_F)%>%
  summarise(cnt=n_distinct(Participant.ID))

# help seeking for depression 

unique(reproductive_rm$Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0)


reproductive_rm <- reproductive_rm %>%
  mutate(
    Seen_GP_nerves_YNU = case_when(
      Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 == "Yes" ~ "Yes",
      Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 == "No"  ~ "No",
      Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 %in% 
        c("Do not know", "Prefer not to answer", "") |
        is.na(Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0) ~ NA_character_
    )
  )


reproductive_rm%>%
  group_by(Seen_GP_nerves_YNU)%>%
  summarise(c=n_distinct(Participant.ID))

# anxiety 

unique(reproductive_rm$Worrier...anxious.feelings...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    anxious_collapsed = case_when(
      Worrier...anxious.feelings...Instance.0%in% c("Yes") ~ "Yes",
      Worrier...anxious.feelings...Instance.0%in% c("No") ~ "No",
      Worrier...anxious.feelings...Instance.0%in% c("Do not know", "Prefer not to answer", "") |
        is.na(Worrier...anxious.feelings...Instance.0) ~ NA_character_
    ))

unique(reproductive_rm$anxious_collapsed)

reproductive_rm%>%
  group_by(anxious_collapsed)%>%
  summarise(c=n_distinct(Participant.ID))


# alcohol 

unique(reproductive_rm$Alcohol.drinker.status...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    alcohol_status= case_when(
      Alcohol.drinker.status...Instance.0 == "Current" ~ "Current",
      Alcohol.drinker.status...Instance.0 == "Never"  ~ "Never",
      Alcohol.drinker.status...Instance.0 == "Previous"  ~ "Previous",
      Alcohol.drinker.status...Instance.0  %in% 
        c("Do not know", "Prefer not to answer", "") |
        is.na(Alcohol.drinker.status...Instance.0) ~ NA_character_
    )
  )

reproductive_rm%>%
  group_by(alcohol_status)%>%
  summarise(c=n_distinct(Participant.ID))


# smoking 

unique(reproductive_rm$Smoking.status...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    smoker_status= case_when(
      Smoking.status...Instance.0 == "Current" ~ "Current",
      Smoking.status...Instance.0 == "Never"  ~ "Never",
      Smoking.status...Instance.0== "Previous"  ~ "Previous",
      Smoking.status...Instance.0  %in% 
        c("Do not know", "Prefer not to answer", "") |
        is.na(Smoking.status...Instance.0) ~ NA_character_
    )
  )

reproductive_rm%>%
  group_by(smoker_status)%>%
  summarise(c=n_distinct(Participant.ID))

# loneliness/ isolation score

reproductive_rm <- reproductive_rm %>%
  mutate(
    leisure_collapsed = case_when(
      Leisure.social.activities...Instance.0 %in% c("None of the above") ~ "None",
      Leisure.social.activities...Instance.0 %in% c("Do not know", "Prefer not to answer") ~ "unknown",
      is.na(Leisure.social.activities...Instance.0) ~ "unknown",
      !Leisure.social.activities...Instance.0 %in% c("None of the above", "Do not know", "Prefer not to answer") ~ "weekly engagement"
    ),
    # keep a missing level to avoid losing N
    leisure_collapsed = fct_explicit_na(leisure_collapsed, na_level = "unknown"),
    leisure_collapsed = factor(leisure_collapsed, levels = c("weekly engagement", "None", "unknown"))
  )


reproductive_rm%>%
  group_by(leisure_collapsed)%>%
  summarise(cnt=n_distinct(Participant.ID))


reproductive_rm <- reproductive_rm %>%
  mutate(
    friends_collapsed = case_when(
      Frequency.of.friend.family.visits...Instance.0 %in% c( "About once a week","2-4 times a week","Almost daily") ~ "Weekly",
      Frequency.of.friend.family.visits...Instance.0 %in% c("About once a month","Once every few months") ~ "Monthly",
      Frequency.of.friend.family.visits...Instance.0 %in% c("Never or almost never","No friends/family outside household" ) ~ "Never",
      Frequency.of.friend.family.visits...Instance.0 %in% c("Do not know", "Prefer not to answer")~ "unknown",
      TRUE ~ NA_character_
    ),
    # keep a missing level to avoid losing N
    friends_collapsed = fct_explicit_na(friends_collapsed , na_level = "unknown"),
    friends_collapsed = factor(friends_collapsed, levels = c("Weekly","Monthly","Never","unknown"))
  )

unique(reproductive_rm$friends_collapsed)

reproductive_rm%>%
  group_by(friends_collapsed)%>%
  summarise(cnt=n_distinct(Participant.ID))

unique(reproductive_rm$Number.in.household...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    house_num = as.numeric(Number.in.household...Instance.0)  # Converts text like "Do not remember" to NA
  )

reproductive_rm<- reproductive_rm %>%
  mutate(
    house_F = case_when(
      house_num < 2 ~ "alone",        # living alone
      house_num >= 2 ~ "not alone",   # Living with others
      TRUE ~ NA_character_
    )
  )
# keep a missing level to avoid losing N
reproductive_rm$house_F = fct_explicit_na(reproductive_rm$house_F , na_level = "unknown")
reproductive_rm$house_F = factor(reproductive_rm$house_F , levels = c("alone","not alone","unknown"))



reproductive_rm <- reproductive_rm %>%
  mutate(
    # high-risk: living alone
    iso_house = case_when(
      house_F == "alone" ~ 1,
      house_F == "not alone" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # high-risk: friends/family visits less than monthly
    iso_friends = case_when(
      friends_collapsed %in% c("Never") ~ 1,
      friends_collapsed %in% c("Monthly", "Weekly") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # high-risk: social activities less than weekly
    iso_leisure = case_when(
      leisure_collapsed == "None" ~ 1,
      leisure_collapsed == "weekly engagement" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # total isolation score
    isolation_score = iso_house + iso_friends + iso_leisure,
    
    # classify isolation level
    isolation_category = case_when(
      isolation_score == 0 ~ "least isolated",
      isolation_score == 1 ~ "moderately isolated",
      isolation_score %in% c(2, 3) ~ "most isolated",
      TRUE ~ NA_character_
    ),
    
    isolation_category = factor(isolation_category, 
                                levels = c("least isolated", "moderately isolated", "most isolated"))
  )


reproductive_rm%>%
  group_by(isolation_category)%>%
  summarise(cnt=n_distinct(Participant.ID))

unique(reproductive_rm$Able.to.confide...Instance.0)

reproductive_rm%>%
  group_by(Able.to.confide...Instance.0)%>%
  summarise(cnt=n_distinct(Participant.ID))

reproductive_rm%>%
  group_by(Loneliness..isolation...Instance.0)%>%
  summarise(cnt=n_distinct(Participant.ID))


## 1. Collapse confiding (Instance 0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    confide_collapsed = case_when(
      Able.to.confide...Instance.0 %in% c("Almost daily", "2-4 times a week", "About once a week") ~ "weekly+",
      Able.to.confide...Instance.0 %in% c("About once a month", "Once every few months")            ~ "monthly",
      Able.to.confide...Instance.0 %in% c("Never or almost never")                                  ~ "rare/never",
      Able.to.confide...Instance.0 %in% c("Do not know", "Prefer not to answer") |
        is.na(Able.to.confide...Instance.0)                                                         ~ "Unknown"
    ),
    confide_collapsed = factor(
      confide_collapsed,
      levels = c("weekly+", "monthly", "rare/never", "Unknown")
    )
  )

## 2. Collapse loneliness item (Instance 0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    loneliness = case_when(
      Loneliness..isolation...Instance.0 == "Yes"  ~ "Yes",
      Loneliness..isolation...Instance.0 == "No"   ~ "No",
      Loneliness..isolation...Instance.0 %in% c("Do not know", "Prefer not to answer") |
        is.na(Loneliness..isolation...Instance.0) ~ "Unknown"
    ),
    loneliness = factor(loneliness, levels = c("No", "Yes", "Unknown"))
  )

## 3. Build numeric risk components

reproductive_rm <- reproductive_rm %>%
  mutate(
    # high-risk factor 1: often/ever feel lonely
    lonely_risk = case_when(
      loneliness == "Yes"     ~ 1,
      loneliness == "No"      ~ 0,
      loneliness == "Unknown" ~ NA_real_
    ),
    
    # high-risk factor 2: rarely or never able to confide
    confide_risk = case_when(
      confide_collapsed == "rare/never"             ~ 1,
      confide_collapsed %in% c("weekly+", "monthly") ~ 0,
      confide_collapsed == "Unknown"                ~ NA_real_
    )
  )

## 4. Total loneliness score and categories

reproductive_rm <- reproductive_rm %>%
  mutate(
    # numeric score (0–2) where at least one component is known
    loneliness_score = lonely_risk + confide_risk,
    
    loneliness_category = case_when(
      is.na(loneliness_score)          ~ "Unknown",         # both components unknown
      loneliness_score == 0            ~ "least lonely",
      loneliness_score == 1            ~ "moderately lonely",
      loneliness_score == 2            ~ "most lonely"
    ),
    
    loneliness_category = factor(
      loneliness_category,
      levels = c("least lonely", "moderately lonely", "most lonely", "Unknown")
    )
  )

## 5. Quick check

reproductive_rm %>%
  count(loneliness_category)

reproductive_rm <- reproductive_rm %>%
  mutate(
    loneliness_category = na_if(loneliness_category, "Unknown")
  )


# deprivation quintiles

unique(reproductive_rm$Townsend.deprivation.index.at.recruitment)

# Get quintile cutpoints from full dataset
cut_points <- quantile(
  reproductive$Townsend.deprivation.index.at.recruitment,
  probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  na.rm = TRUE
)

reproductive_rm <- reproductive_rm %>%
  mutate(
    deprivation_quintiles = cut(
      Townsend.deprivation.index.at.recruitment,
      breaks = cut_points,
      include.lowest = TRUE,
      labels = c("Q1 (least deprived)",
                 "Q2",
                 "Q3",
                 "Q4",
                 "Q5 (most deprived)")
    )
  )


reproductive_rm %>%
  group_by(deprivation_quintiles)%>%
  summarise(count=n_distinct(Participant.ID))


# Blood pressure meds, Insulin, Cholesterol lowering meds:

na_med_vals <- c("Do not know", "Prefer not to answer", "")

reproductive_rm <- reproductive_rm %>%
  mutate(
    med_raw = Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0,
    
    med_is_na = med_raw %in% na_med_vals | is.na(med_raw),
    
    current_cholesterol = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("Cholesterol lowering medication", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_bp = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("Blood pressure medication", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_insulin = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("\\bInsulin\\b", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_HRT = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("Hormone replacement therapy", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_OC = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("Oral contraceptive pill|minipill", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_minipill = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("minipill", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_none_of_the_above_med = case_when(
      med_is_na ~ NA_character_,
      str_detect(med_raw, regex("None of the above", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    # these flags are now mostly redundant, but kept here in case you still want them
    current_prefer_not_to_answer_med = case_when(
      med_is_na &
        str_detect(coalesce(med_raw, ""), regex("Prefer not to answer", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    ),
    
    current_do_not_know_med = case_when(
      med_is_na &
        str_detect(coalesce(med_raw, ""), regex("Do not know", ignore_case = TRUE)) ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  dplyr::select(-med_raw, -med_is_na)

reproductive_rm %>%
  group_by(current_bp)%>%
  summarise(count=n_distinct(Participant.ID))

# waist over 80

reproductive_rm$Waist.circumference...Instance.0 <- as.numeric(reproductive_rm$Waist.circumference...Instance.0)


reproductive_rm$waist_over80 <- ifelse(reproductive_rm$Waist.circumference...Instance.0 > 80, "Yes", "No")

reproductive_rm %>%
  group_by(waist_over80)%>%
  summarise(count=n_distinct(Participant.ID))

# Get APOE

# Download GWAS dataset

apoe <- read.csv("gen_apoe.csv")

# Work out APOE status using ifelse statement 

apoe$APOE4 <- ifelse(
  (apoe$rs429358 == "CC" & (apoe$rs7412 == "CC" | apoe$rs7412 == "TC")) | 
    (apoe$rs429358 == "TC" & (apoe$rs7412 == "TC" | apoe$rs7412 == "CC")),
  "Y",
  "N"
)

# Need to make sure the number matches the one indicated by Jennifer Collister: 138,574 

apoe%>%
  group_by(APOE4)%>%
  summarise(count=n_distinct(ID))

# Follow the processes above for joining APOE4 onto the demo table. 

reproductive_rm <- reproductive_rm %>%
  left_join(
    apoe %>% dplyr::select(ID, APOE4),
    by = c("Participant.ID" = "ID")
  )


## Transform vitamins to yes/no

unique(reproductive_rm$Vitamin.and.mineral.supplements...Instance.0)

unique(reproductive_rm$Vitamin.supplements..pilot....Instance.0)

unique(reproductive_rm$Mineral.and.other.dietary.supplements...Instance.0)

unique(reproductive_rm$Vitamin.and.mineral.supplements..pilot....Instance.0)

# Define a function to merge pilot and non-pilot columns by combining unique values
merge_columns <- function(df, col_main, col_pilot) {
  # Apply across each row, splitting values by '|' and removing duplicates
  combined_values <- apply(df[, c(col_main, col_pilot)], 1, function(row) {
    # Split values by '|' and flatten to unique set
    combined <- unique(unlist(strsplit(paste(row, collapse = "|"), split = "\\|")))
    # Remove any empty strings and collapse back with '|'
    paste(combined[combined != ""], collapse = "|")
  })
  # Return the combined column as a new vector
  return(combined_values)
}

# Applying the function for each column pair
reproductive_rm$Combined_Mineral_Supplements <- merge_columns(reproductive_rm, 
                                                           "Mineral.and.other.dietary.supplements...Instance.0",
                                                           "Vitamin.and.mineral.supplements..pilot....Instance.0")

reproductive_rm$Combined_Vitamin_Supplements <- merge_columns(reproductive_rm,
                                                           "Vitamin.and.mineral.supplements...Instance.0",
                                                           "Vitamin.supplements..pilot....Instance.0")


# Ensure correct column names and perform the join

cat(colnames(reproductive_rm), sep = "\n")

# Get unique supplements from both columns
unique_supplements <- unique(unlist(strsplit(
  paste(reproductive_rm$Combined_Mineral_Supplements,
        reproductive_rm$Combined_Vitamin_Supplements,
        sep = "|"),
  "\\|"
)))

# Remove whitespace
unique_supplements <- trimws(unique_supplements)

# DROP empty strings and NAs so they don't become column names
unique_supplements <- unique_supplements[unique_supplements != "" & !is.na(unique_supplements)]


for (supp in unique_supplements) {
  reproductive_rm[[supp]] <- ifelse(
    grepl(supp, reproductive_rm$Combined_Mineral_Supplements, fixed = TRUE) |
      grepl(supp, reproductive_rm$Combined_Vitamin_Supplements, fixed = TRUE),
    "Y",
    "N"
  )
}

nm <- names(reproductive_rm)
bad <- is.na(nm) | nm == ""
nm[bad] <- paste0("unnamed_", seq_len(sum(bad)))
names(reproductive_rm) <- nm


# Combine the two fish oil columns into the correctly named one
reproductive_rm$`Fish oil (including cod liver oil)` <- ifelse(
  reproductive_rm$`Fish oil (including cod liver oil)` == "Y" |
    reproductive_rm$`Fish oil (including cod liver oil` == "Y",
  "Y",
  ifelse(
    reproductive_rm$`Fish oil (including cod liver oil)` == "N" &
      reproductive_rm$`Fish oil (including cod liver oil` == "N",
    "N",
    NA
  )
)

# Drop the broken-name column
reproductive_rm$`Fish oil (including cod liver oil` <- NULL


# One combined PNA flag (TRUE if either column says Y)
pna_vits <- (reproductive_rm$`Prefer not to answer` == "Y")

vitamin_cols <- c(
  "Fish oil (including cod liver oil)",
  "Glucosamine",
  "Folic acid or Folate (Vit B9)",
  "Multivitamins +/- minerals",
  "Iron",
  "Calcium",
  "Vitamin E",
  "Vitamin C",
  "Evening primrose oil",
  "Vitamin B",
  "Vitamin D",
  "Vitamin A",
  "Zinc",
  "Selenium",
  "Garlic",
  "Ginkgo",
  "Other supplements, vitamins or minerals"
)

setdiff(vitamin_cols, names(reproductive_rm))

# Extract the vitamin columns as a matrix/data frame
vit_mat <- reproductive_rm[ , vitamin_cols, drop = FALSE]

# 1) Any supplement ticked?
any_vit <- rowSums(vit_mat == "Y", na.rm = TRUE) > 0

# 2) All supplement columns missing?
all_vit_na <- rowSums(!is.na(vit_mat)) == 0

# 3) Use the pna_vits logical we built above
prefer_not <- pna_vits

# 4) Combine into Vitamins_YN
reproductive_rm$Vitamins_YN <- ifelse(
  prefer_not, "Unknown",
  ifelse(
    any_vit, "Y",
    ifelse(
      all_vit_na, NA_character_, "N"
    )
  )
)

unique(reproductive_rm$Vitamins_YN)

reproductive_rm <- reproductive_rm %>%
  mutate(
    Vitamins_YN = na_if(Vitamins_YN, "Unknown")
  )

reproductive_rm%>%
  group_by(Vitamins_YN)%>%
  summarise(c=n_distinct(Participant.ID))


## Cancer 
colnames(reproductive_rm)

unique(reproductive_rm$Cancer.diagnosed.by.doctor...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    Cancer_YNU = case_when(
      Cancer.diagnosed.by.doctor...Instance.0%in% c("Yes - you will be asked about this later by an interviewer") ~ "Yes",
      Cancer.diagnosed.by.doctor...Instance.0%in% c("No") ~ "No",
      Cancer.diagnosed.by.doctor...Instance.0%in% c("Do not know", "Prefer not to answer", "") |
        is.na(Cancer.diagnosed.by.doctor...Instance.0) ~ NA_character_
    ))



reproductive_rm%>%
  group_by(Cancer_YNU)%>%
  summarise(c=n_distinct(Participant.ID))

## Polypharmacy 

reproductive_rm$number_meds <- reproductive_rm$Number.of.treatments.medications.taken...Instance.0

## Age menarche

reproductive_rm$age_menarche <- reproductive_rm$Age.when.periods.started..menarche....Instance.0

reproductive_rm <- reproductive_rm %>%
  mutate(
    age_menarche = na_if(age_menarche, "Do not know"),
    age_menarche = na_if(age_menarche, "Prefer not to answer"),
    age_menarche = as.numeric(age_menarche)
  )

class(reproductive_rm$age_menarche)


## Sleep

reproductive_rm$sleep_hours<- reproductive_rm$Sleep.duration...Instance.0

reproductive_rm <- reproductive_rm %>%
  mutate(
    sleep_hours = na_if(sleep_hours, "Do not know"),
    sleep_hours = na_if(sleep_hours, "Prefer not to answer"),
    sleep_hours = as.numeric(sleep_hours)
  )

class(reproductive_rm$sleep_hours)

# bilateral oophorectomy 

reproductive_rm <- reproductive_rm %>%
  mutate(
    bilateral_oophorectomy = if_else(
      !is.na(age_bilateral_oophorectomy_combined),
      "Y",
      "N"
    )
  )

reproductive_rm%>%
  group_by(bilateral_oophorectomy)%>%
  summarise(c=n_distinct(Participant.ID))


# hysterectomy 

reproductive_rm <- reproductive_rm %>%
  mutate(
    hysterectomy = if_else(
      !is.na(age_hysterectomy_combined),
      "Y",
      "N"
    )
  )

## Exercise

# Exercise mins per week (transform TO LOW MEDIUM HIGH)

unique(reproductive_rm$Summed.MET.minutes.per.week.for.all.activity...Instance.0)

reproductive_rm <- reproductive_rm %>%
  mutate(
    exercise_category = case_when(
      is.na(Summed.MET.minutes.per.week.for.all.activity...Instance.0) ~NA_character_,
      Summed.MET.minutes.per.week.for.all.activity...Instance.0 < 600 ~ "<600 MET-min/week (low)",
      Summed.MET.minutes.per.week.for.all.activity...Instance.0 >= 600 &
        Summed.MET.minutes.per.week.for.all.activity...Instance.0 < 3000 ~ "600–3000 MET-min/week (medium)",
      Summed.MET.minutes.per.week.for.all.activity...Instance.0 >= 3000 ~ "≥3000 MET-min/week (high)"
    )
  )

reproductive_rm%>%
  group_by(exercise_category)%>%
  summarise(c=n_distinct(Participant.ID))

write.csv(reproductive_rm, "final_version.csv")
