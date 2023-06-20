
# Project title : Analytic - Ebola Data Descriptor Paper
# Start date    : 2022 December 12
# End date      : 2023 January 26
# Contact person: Laura Merson



# load packages -------------------------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load(rio, 
               here, 
               tidyverse, 
               janitor, 
               readxl, 
               gtsummary, 
               epikit, 
               table1)

# import data ---------------------------------------------------------------------------------

# data requests can be made to IDDO by emailing: info@iddo.org


# initial standardization ---------------------------------------------------------------------

dm_clean <- dm_raw %>% 
  clean_names() %>%                              # transform the variable names to lower characters 
  mutate(
    sex = recode(sex,
                 "F"  = "Female",  
                 "M"  = "Male", 
                 "U"  = "Unknown"
    )) %>%  
  mutate(                                        # re-determine the age and age text 
    age = case_when(
      is.na(age) & agetxt == "95+"  ~ 95,
      is.na(age) & agetxt == "<1"   ~ 1,
      age                 == -1     ~ 0,
      TRUE                          ~ age
    ),
    ageu = na_if(ageu, ""),                      # change blank cells to NA and re-determine the age units
    age_year = case_when(
      ageu  == "YEARS"  ~ round(age),
      ageu  == "MONTHS" ~ round(age/12),
      is.na(ageu)       ~ round(age)
    ), 
    age_group = age_categories(                  # group the ages
      age_year, 
      breakers = c(0, 6, 16, 46)
    )) %>% 
  mutate(country = recode(                       # recode the country variable
    country,
    "GIN" = "Guinea",
    "LBR" = "Liberia",
    "SLE" = "Sierra Leone",
    "NGA" = "Nigeria"
  )) %>% 
  mutate(siteid = na_if(siteid, ""),
         siteid = replace_na(siteid, "UNKNOWN"),
    site = recode(
    siteid,
    "COYAH"         = "Coyah ETC",
    "CTE-NZEREKORE" = "ALIMA Nzérékoré ETC",
    "Macenta"       = "MSF Macenta ETC",
    "Gueckedou"     = "MSF Guéckédou ETC",
    "DONKA"         = "Donka Hospital ETC",
    "ELWA3"         = "ELWA3 ETC",
    "1-Bong"        = "IMC Bong ETC",
    "3-Margibi"     = "IMC Margibi ETC",
    "MSF-Foya"      = "MSF Foya ETC",
    "NIGERIA"       = "Nigeria CDC",
    "GOAL-Port Loko"= "GOAL Port Loko ETC",
    "Goderich"      = "Goderich ETC",
    "Lakka"         = "Lakka Hospital ETC",
    "MSF-Kissy"     = "MSF Methodist Boys High School ETC",
    "MSF-Mag"       = "MSF Magburaka ETC",
    "POW"           = "MSF Prince of Wales ETC",
    "4-Kambia"      = "IMC Kambia ETC",
    "MSF-PTS3"      = "MSF Police Training School ETC",
    "KERRY TOWN"    = "SCI Kerry Town ETC",
    "MSF-Bo"        = "MSF Bo Town ETC",
    "2-Lunsar"      = "IMC Lunsar ETC",
    "5-Makeni"      = "IMC Makeni ETC",
    "MSF-Kail"      = "MSF Kailahun ETC"
  )) %>% 
  select(studyid, usubjid, siteid, site, sex, country, age,    # select interested variables for the analysis
         age_group)

ds_clean <- ds_raw %>% 
  clean_names() %>%                                       # transform the variable names to lower characters 
  mutate(dsdecod = na_if(dsdecod, ""),                    # change blank cells to NA 
    outcome = case_when(                                  # re-determine the data elements in DSDECOD
    dsdecod == "DEATH"                   ~ "Dead",
    dsdecod == "DISCHARGED"              ~ "Recovered",
    dsdecod == "COMPLETED"             | 
    dsdecod == "UNKNOWN"               |
    dsdecod == "STILL IN HOSPITAL"     | 
    dsdecod == "WITHDRAWAL BY SUBJECT" |
    dsdecod == "LOST TO FOLLOW-UP"     | 
    dsdecod == "TRANSFERRED"           |
    is.na(dsdecod)                       ~ "Unknown"
  )) %>% 
  select(studyid, usubjid, epoch, outcome)                # select interested variables for the analysis

mb_clean <- mb_raw %>% 
  clean_names() %>%                                       # transform the variable names to lower characters 
  mutate(
    mbstresc = na_if(mbstresc, ""),                       # change blank cells to NA
    mbstresc2 = case_when(                                # re-determine the data elements in MBSTRESC & MBTEST
    mbstresc == "POSITIVE" | 
    mbstresc < 40                     ~ "Positive",
    mbstresc == "NEGATIVE" | 
    mbstresc >= 40         | 
    mbstresc == ">40"      | 
    mbstresc == ">45"                 ~ "Negative",
    TRUE                              ~ "Unknown"),
    mbtest2 = case_when(
    mbtest == "Dengue Flavivirus"     ~ "Dengue",
    mbtest == "Lassa Mammarenavirus"  ~ "Lassa fever",
    mbtest == "Measles Morbillivirus" ~ "Measles",
    mbtest == "Plasmodium" | 
    mbtest == "Plasmodium falciparum" ~ "Malaria", 
    mbtest == "Zaire Ebolavirus"      ~ "Ebola Virus Disease",
    TRUE                              ~ mbtest
  )) %>% 
  select(studyid, usubjid, mbtest2, mbstresc2)            # select interested variables for the analysis

sa_clean <- sa_raw %>% 
  clean_names() %>% 
  mutate(
    samodify = na_if(samodify, "")) %>% 
  mutate(
    samodify2 = case_when(
      samodify == "FEVER" |
       samodify_lm == "FEVER"                                           ~ "Fever",
      samodify == "HEADACHE" |
       samodify_lm == "HEADACHE"                                        ~ "Headache",
      samodify == "ANOREXIA" | 
      samodify == "LOSS OF APPETITE/ANOREXIA" |
       samodify_lm == "ANOREXIA" | 
       samodify_lm == "LOSS OF APPETITE/ANOREXIA"                       ~ "Anorexia",
      samodify == "STOMACH PAIN" |
      samodify == "ABDOMINAL BLOATING (HEPATO/GASTRO/ENTEROLOGY)"|
      samodify == "ABDOMINAL PAIN" |
      samodify == "SENSITIVE ABDOMEN" |
      samodify == "UMBILICAL PAIN" |
      samodify == "UPPER QUADRANT PAIN" |
      samodify == "ABDOMINAL PAIN (HEPATO/GASTRO/ENTEROLOGY)" |
      samodify == "EPIGASTRIC PAIN" |
      samodify == "ABDOMINAL BLOATING" | 
        samodify_lm == "STOMACH PAIN" |
        samodify_lm == "ABDOMINAL BLOATING (HEPATO/GASTRO/ENTEROLOGY)"|
        samodify_lm == "ABDOMINAL PAIN" |
        samodify_lm == "SENSITIVE ABDOMEN" |
        samodify_lm == "UMBILICAL PAIN" |
        samodify_lm == "UPPER QUADRANT PAIN" |
        samodify_lm == "ABDOMINAL PAIN (HEPATO/GASTRO/ENTEROLOGY)" |
        samodify_lm == "EPIGASTRIC PAIN" |
        samodify_lm == "ABDOMINAL BLOATING"                             ~ "Stomach pain",
      samodify == "VOMITING" |
      samodify == "VOMITING/NAUSEA" |
      samodify == "NAUSEA"  | 
        samodify_lm == "VOMITING" |
        samodify_lm == "VOMITING/NAUSEA" |
        samodify_lm == "NAUSEA"                                         ~ "Vomiting",
      samodify == "DIARRHOEA"  |
      samodify == "DIGESTIVE LOSS" | 
        samodify_lm == "DIARRHOEA"  |
        samodify_lm == "DIGESTIVE LOSS"                                 ~ "Diarrhoea",
      samodify == "MUSCLE PAIN"| 
      samodify == "ARTHRALGIA" |
      samodify == "JOINT PAIN" |
      samodify == "MYALGIA"    |
      samodify == "PAIN"       |
      samodify == "RHEUMATISM" | 
      samodify == "BACK PAIN (RHEUMATOLOGY)" | 
      samodify == "JOINT PAIN (RHEUMATOLOGY)"| 
      samodify == "LOWER BACK PAIN" |
      samodify == "LOWER BACK PAIN (RHEUMATOLOGY)"|
      samodify == "NECK PAIN" |
      samodify == "NECK PAIN (RHEUMATOLOGY)"|
      samodify == "STIFFNESS" |
      samodify == "BACK PAIN" | 
        samodify_lm == "MUSCLE PAIN"| 
        samodify_lm == "ARTHRALGIA" |
        samodify_lm == "JOINT PAIN" |
        samodify_lm == "MYALGIA"    |
        samodify_lm == "PAIN"       |
        samodify_lm == "RHEUMATISM" | 
        samodify_lm == "BACK PAIN (RHEUMATOLOGY)" | 
        samodify_lm == "JOINT PAIN (RHEUMATOLOGY)"| 
        samodify_lm == "LOWER BACK PAIN" |
        samodify_lm == "LOWER BACK PAIN (RHEUMATOLOGY)"|
        samodify_lm == "NECK PAIN" |
        samodify_lm == "NECK PAIN (RHEUMATOLOGY)"|
        samodify_lm == "STIFFNESS" |
        samodify_lm == "BACK PAIN"                                     ~ "aching muscles or joints",
      samodify == "DIFFICULTY SWALLOWING"|
      samodify == "SORE THROAT" |
      samodify == "DYSPHAGIA"|
      samodify == "DYSPHAGIA (HEPATO/GASTRO/ENTEROLOGY)" |
        samodify_lm == "DIFFICULTY SWALLOWING"|
        samodify_lm == "SORE THROAT" |
        samodify_lm == "DYSPHAGIA"|
        samodify_lm == "DYSPHAGIA (HEPATO/GASTRO/ENTEROLOGY)"          ~ "Difficulty swallowing/Sore throat",
      samodify == "DIFFICULTY BREATHING"|
      samodify == "SHORTNESS OF BREATH" |
      samodify == "RAPID BREATHING"     |
      samodify == "DYSPNEA" |
      samodify == "DYSPNEA (CARDIOLOGY)"|   
      samodify == "DYSPNEA (PNEUMOLOGY)"|
        samodify_lm == "DIFFICULTY BREATHING"|
        samodify_lm == "SHORTNESS OF BREATH" |
        samodify_lm == "RAPID BREATHING"     |
        samodify_lm == "DYSPNEA" |
        samodify_lm == "DYSPNEA (CARDIOLOGY)" |   
        samodify_lm == "DYSPNEA (PNEUMOLOGY)"                            ~ "Difficulty breathing",
      samodify == "HICCOUGHS" |
        samodify_lm == "HICCOUGHS"                                         ~ "Hiccups",
      samodify == "RASH" | 
      samodify == "SKIN RASH" |
      samodify == "RASH/REDNESS" |
        samodify_lm == "RASH" | 
        samodify_lm == "SKIN RASH" |
        samodify_lm == "RASH/REDNESS"                                    ~ "Rash",
      samodify == "ANURIA"   |
      samodify == "ANURIA (UROLOGY/NEPHROLOGY)" |
      samodify == "DIURESIS" |
      samodify == "HEPATOMEGALY" |
      samodify == "JANUDICE" |
      samodify == "JAUNDICE" |
      samodify == "JAUNDICE (YELLOW EYES)"|
      samodify == "PAINFUL URINATION"|
      samodify == "PAINFUL URINATION AND ODOUR"|
      samodify == "POLYDIPSIA (UROLOGY/NEPHROLOGY)"|
      samodify == "POLYURIA (UROLOGY/NEPHROLOGY)"  |
      samodify == "ACUTE KIDNEY INJURY" |
        samodify_lm == "ANURIA"   |
        samodify_lm == "ANURIA (UROLOGY/NEPHROLOGY)" |
        samodify_lm == "DIURESIS" |
        samodify_lm == "HEPATOMEGALY" |
        samodify_lm == "JANUDICE" |
        samodify_lm == "JAUNDICE" |
        samodify_lm == "JAUNDICE (YELLOW EYES)"|
        samodify_lm == "PAINFUL URINATION"|
        samodify_lm == "PAINFUL URINATION AND ODOUR"|
        samodify_lm == "POLYDIPSIA (UROLOGY/NEPHROLOGY)"|
        samodify_lm == "POLYURIA (UROLOGY/NEPHROLOGY)"  |
        samodify_lm == "ACUTE KIDNEY INJURY"                          ~ "Impaired kidney and liver function", 
      samodify == "EPISTAXIS"|
      samodify == "UNEXPLAINED BLEEDING"|
      samodify == "VAGINAL BLEEDING" |
      samodify == "BLACK STOOLS OR MELENA"|
      samodify == "BLACK STOOLS/MELENA"|
      samodify == "BLACK STOOL/MELENA"|
      samodify == "BLEEDING AT INJECTION"|
      samodify == "BLEEDING GUMS"|
      samodify == "BLEEDING"|
      samodify == "BLEEDING VAGINA"|
      samodify == "BLOODY DIARRHOEA"|
      samodify == "BLOOD IN VOMIT"|
      samodify == "BRUISING"|
      samodify == "COUGHING UP BLOOD"|
      samodify == "EYE HEMORRHAGE"|
      samodify == "HEMATEMESIS"|
      samodify == "HEMATEMESIS (HEPATO/GASTRO/ENTEROLOGY)"|
      samodify == "HEMATURIA"|
      samodify == "HEMATURIA (UROLOGY/NEPHROLOGY)"|
      samodify == "HEMOPTYSIS"|
      samodify == "INJECTION SITE BLEEDING"|
      samodify == "MELENA"|
      samodify == "NOSE BLEED/EPISTAXIS"|
      samodify == "OTHER HEMORRHAGE"|
      samodify == "PETECHIAE"|
      samodify == "SPITTING UP BLOOD"|
      samodify == "VOMITING BLOOD"|
        samodify_lm == "EPISTAXIS"|
        samodify_lm == "UNEXPLAINED BLEEDING"|
        samodify_lm == "VAGINAL BLEEDING" |
        samodify_lm == "BLACK STOOLS OR MELENA"|
        samodify_lm == "BLACK STOOLS/MELENA"|
        samodify_lm == "BLACK STOOL/MELENA"|
        samodify_lm == "BLEEDING AT INJECTION"|
        samodify_lm == "BLEEDING GUMS"|
        samodify_lm == "BLEEDING"|
        samodify_lm == "BLEEDING VAGINA"|
        samodify_lm == "BLOODY DIARRHOEA"|
        samodify_lm == "BLOOD IN VOMIT"|
        samodify_lm == "BRUISING"|
        samodify_lm == "COUGHING UP BLOOD"|
        samodify_lm == "EYE HEMORRHAGE"|
        samodify_lm == "HEMATEMESIS"|
        samodify_lm == "HEMATEMESIS (HEPATO/GASTRO/ENTEROLOGY)"|
        samodify_lm == "HEMATURIA"|
        samodify_lm == "HEMATURIA (UROLOGY/NEPHROLOGY)"|
        samodify_lm == "HEMOPTYSIS"|
        samodify_lm == "INJECTION SITE BLEEDING"|
        samodify_lm == "MELENA"|
        samodify_lm == "NOSE BLEED/EPISTAXIS"|
        samodify_lm == "OTHER HEMORRHAGE"|
        samodify_lm == "PETECHIAE"|
        samodify_lm == "SPITTING UP BLOOD"|
        samodify_lm == "VOMITING BLOOD"                             ~ "Internal and external bleeding",
      samodify == "ASTHENIA" |
      samodify == "FATIGUE/MALAISE" |
      samodify == "INTENSE GENERAL FATIGUE"|
        samodify_lm == "ASTHENIA" |
        samodify_lm == "FATIGUE/MALAISE" |
        samodify_lm == "INTENSE GENERAL FATIGUE"                 ~ "Lethargy/Fatigue",
      TRUE                                                          ~ "No symptoms"
    ))

rp_clean <- rp_raw %>% 
  clean_names() %>% 
  mutate(
    rpstresc = na_if(rpstresc, "")) %>% 
  mutate(rpstresc = recode(
    rpstresc,
    "11" = "B", "12" = "B", "15" = "B", 
    "20" = "B", "28" = "B", "33" = "B",
    "37" = "B", "4"  = "B", "41" = "B", 
    "45" = "B", "7"  = "B", "8"  = "B",
    "2014-07" = "B"
  ),
  rpstresc = replace_na(rpstresc, "Z")) %>% 
  select(studyid, usubjid, rpstresc, rptest)

in_clean <- in_raw %>% clean_names() 
lb_clean <- lb_raw %>% clean_names()
dd_clean <- dd_raw %>% clean_names()
rs_clean <- rs_raw %>% clean_names()
sc_clean <- sc_raw %>% clean_names()
ti_clean <- ti_raw %>% clean_names()
ts_clean <- ts_raw %>% clean_names()
tv_clean <- tv_raw %>% clean_names()
vs_clean <- vs_raw %>% clean_names()
er_clean <- er_raw %>% clean_names()
ho_clean <- ho_raw %>% clean_names()
sv_clean <- sv_raw %>% clean_names()
po_clean <- po_raw %>% clean_names()


# Analyses ------------------------------------------------------------------------------------

## COMPLETE TABLE 1: sex, age group, country and laboratory confirmed infections ----

mb_ebola <- mb_clean %>% 
  filter(!(studyid %in% "ESBMRS")) %>%
  filter(mbstresc2 == "Positive" & 
           mbtest2 == "Ebola Virus Disease") %>% 
  distinct(usubjid, .keep_all = TRUE) 

mb_malaria <- mb_clean %>% 
  filter(!(studyid %in% "ESBMRS")) %>%
  filter(mbstresc2 == "Positive" & 
           mbtest2 == "Malaria") %>% 
  distinct(usubjid, .keep_all = TRUE)

mal_ebol <- mb_ebola %>% 
  full_join(mb_malaria, 
            by = c("usubjid" = "usubjid",
                   "studyid" = "studyid")) %>% 
  select(studyid, usubjid, mbtest2.x, mbtest2.y) %>% 
  mutate(combine = case_when(
    mbtest2.x == "Ebola Virus Disease" &
    mbtest2.y == "Malaria"               ~ "EVD and Malaria", 
    is.na(mbtest2.x)                     ~ "Malaria",
    is.na(mbtest2.y)                     ~ "Ebola Virus Disease"
  ))

dm_mb_join <- dm_clean %>%
  filter(!(studyid %in% c("ESBMRS", "EFFVXT"))) %>%
  full_join(mal_ebol, by = c("usubjid" = "usubjid",
                             "studyid" = "studyid"))

label(dm_mb_join$sex)       <- "Sex"
label(dm_mb_join$age_group) <- "Age Group"
label(dm_mb_join$country)   <- "Country"
label(dm_mb_join$combine)   <- "Laboratory confirmed infections"

table1(~ sex + age_group + country + combine, data = dm_mb_join, 
       topclass = "Rtable1-zebra")

## COMPLETE TABLE 2: outcome, pregnancy, and signs & symptoms ----

# outcome
outcome_ebola_positive <- ds_clean %>%
  filter(!(studyid %in% c("ESBMRS", "EFFVXT"))) %>%
  distinct(usubjid, .keep_all = T) 

ebola_posit_outcome <- mb_ebola %>% 
  left_join(outcome_ebola_positive, 
            by = "usubjid", "studyid") %>% 
  distinct(usubjid, .keep_all = T)

table1(~ outcome, data = ebola_posit_outcome, 
       topclass = "Rtable1-zebra")

# Pregnancy
unique_pregnant_patient <- rp_clean %>% 
  distinct() %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  filter(rptest == "Pregnant Indicator") %>% 
  left_join(mb_ebola, by = "usubjid", "studyid")

pregn_denominator <- unique_pregnant_patient %>% 
  group_by(usubjid, rptest) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

pregn_numerator <- unique_pregnant_patient %>% 
  filter(rpstresc == "Y") %>%
  group_by(usubjid, rptest) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

pregnancy_join <- full_join(
  pregn_denominator, pregn_numerator, 
  by = "usubjid", "rptest")

label(pregnancy_join$rptest.y) <- "pregnancy numerator"
label(pregnancy_join$rptest.x) <- "pregnancy denominator"
table1(~ rptest.y + rptest.x, data = pregnancy_join, 
       topclass = "Rtable1-zebra")


# signs & symptoms
signs_symptoms <- sa_clean %>% 
  filter(!(studyid %in% "ESBMRS")) %>%
  filter(samodify2 != "No symptoms") %>% 
  filter(saoccur == "Y" | saoccur == "N") %>% 
  select(studyid, usubjid, samodify2, sapresp, saoccur) 

ebola_signs_symptoms <- mb_ebola %>% 
  full_join(signs_symptoms, by = "usubjid", "studyid")

lethargy_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Lethargy/Fatigue")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

lethargy_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Lethargy/Fatigue")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

lethargy_join <- full_join(
  lethargy_denominator, lethargy_numerator, 
  by = "usubjid", "samodify2")

label(lethargy_join$samodify2.y) <- "lethargy numerator"
label(lethargy_join$samodify2.x) <- "lethargy denominator"
table1(~ samodify2.y + samodify2.x, data = lethargy_join, 
       topclass = "Rtable1-zebra")


fever_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Fever")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>% 
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup()

fever_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Fever")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>% 
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup()

fever_join <- full_join(
  fever_denominator, fever_numerator, 
  by = "usubjid", "samodify2")

label(fever_join$samodify2.y) <- "fever numerator"
label(fever_join$samodify2.x) <- "fever denominator"
table1(~ samodify2.y + samodify2.x, data = fever_join, 
       topclass = "Rtable1-zebra")


anorexia_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Anorexia")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

anorexia_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Anorexia")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

anorexia_join <- full_join(
  anorexia_denominator, anorexia_numerator, 
  by = "usubjid", "samodify2")

label(anorexia_join$samodify2.y) <- "anorexia numerator"
label(anorexia_join$samodify2.x) <- "anorexia denominator"
table1(~ samodify2.y + samodify2.x, data = anorexia_join, 
       topclass = "Rtable1-zebra")


headache_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Headache")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup()

headache_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Headache")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup()

headache_join <- full_join(
  headache_denominator, headache_numerator, 
  by = "usubjid", "samodify2")

label(headache_join$samodify2.y) <- "headache numerator"
label(headache_join$samodify2.x) <- "headache denominator"
table1(~ samodify2.y + samodify2.x, data = headache_join, 
       topclass = "Rtable1-zebra")


aching_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "aching muscles or joints")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

aching_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "aching muscles or joints")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

aching_join <- full_join(
  aching_denominator, aching_numerator, 
  by = "usubjid", "samodify2")

label(aching_join$samodify2.y) <- "aching numerator"
label(aching_join$samodify2.x) <- "aching denominator"
table1(~ samodify2.y + samodify2.x, data = aching_join, 
       topclass = "Rtable1-zebra")


vomiting_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Vomiting")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup()

vomiting_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Vomiting")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup()

vomiting_join <- full_join(
  vomiting_denominator, vomiting_numerator, 
  by = "usubjid", "samodify2")

label(vomiting_join$samodify2.y) <- "vomiting numerator"
label(vomiting_join$samodify2.x) <- "vomiting denominator"
table1(~ samodify2.y + samodify2.x, data = vomiting_join, 
       topclass = "Rtable1-zebra")


diarrhoea_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Diarrhoea")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

diarrhoea_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Diarrhoea")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

diarrhoea_join <- full_join(
  diarrhoea_denominator, diarrhoea_numerator, 
  by = "usubjid", "samodify2")

label(diarrhoea_join$samodify2.y) <- "diarrhoea numerator"
label(diarrhoea_join$samodify2.x) <- "diarrhoea denominator"
table1(~ samodify2.y + samodify2.x, data = diarrhoea_join, 
       topclass = "Rtable1-zebra")


stomachpain_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Stomach pain")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

stomachpain_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Stomach pain")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

stomachpain_join <- full_join(
  stomachpain_denominator, stomachpain_numerator, 
  by = "usubjid", "samodify2")

label(stomachpain_join$samodify2.y) <- "stomachpain numerator"
label(stomachpain_join$samodify2.x) <- "stomachpain denominator"
table1(~ samodify2.y + samodify2.x, data = stomachpain_join, 
       topclass = "Rtable1-zebra")


bleeding_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Internal and external bleeding")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

bleeding_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Internal and external bleeding")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

bleeding_join <- full_join(
  bleeding_denominator, bleeding_numerator, 
  by = "usubjid", "samodify2")

label(bleeding_join$samodify2.y) <- "bleeding numerator"
label(bleeding_join$samodify2.x) <- "bleeding denominator"
table1(~ samodify2.y + samodify2.x, data = bleeding_join, 
       topclass = "Rtable1-zebra")


throat_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Difficulty swallowing/Sore throat")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

throat_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Difficulty swallowing/Sore throat")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

throat_join <- full_join(
  throat_denominator, throat_numerator, 
  by = "usubjid", "samodify2")

label(throat_join$samodify2.y) <- "throat numerator"
label(throat_join$samodify2.x) <- "throat denominator"
table1(~ samodify2.y + samodify2.x, data = throat_join, 
       topclass = "Rtable1-zebra")


hiccups_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Hiccups")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

hiccups_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Hiccups")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

hiccups_join <- full_join(
  hiccups_denominator, hiccups_numerator, 
  by = "usubjid", "samodify2")

label(hiccups_join$samodify2.y) <- "hiccups numerator"
label(hiccups_join$samodify2.x) <- "hiccups denominator"
table1(~ samodify2.y + samodify2.x, data = hiccups_join, 
       topclass = "Rtable1-zebra")


breathing_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Difficulty breathing")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

breathing_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Difficulty breathing")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

breathing_join <- full_join(
  breathing_denominator, breathing_numerator, 
  by = "usubjid", "samodify2")

label(breathing_join$samodify2.y) <- "breathing numerator"
label(breathing_join$samodify2.x) <- "breathing denominator"
table1(~ samodify2.y + samodify2.x, data = breathing_join, 
       topclass = "Rtable1-zebra")


kidney_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Impaired kidney and liver function")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%  
  summarise(n_cases = n()) %>% 
  ungroup() 

kidney_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Impaired kidney and liver function")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>% 
  summarise(n_cases = n()) %>% 
  ungroup() 

kidney_join <- full_join(
  kidney_denominator, kidney_numerator, 
  by = "usubjid", "samodify2")

label(kidney_join$samodify2.y) <- "kidney numerator"
label(kidney_join$samodify2.x) <- "kidney denominator"
table1(~ samodify2.y + samodify2.x, data = kidney_join, 
       topclass = "Rtable1-zebra")


rash_denominator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Rash")%>%
  filter(sapresp == "Y" & (saoccur == "Y" | 
                           saoccur == "N")) %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

rash_numerator <- ebola_signs_symptoms %>% 
  filter(samodify2 == "Rash")%>%
  filter(sapresp == "Y" & saoccur == "Y") %>%
  group_by(usubjid, samodify2) %>%   
  summarise(n_cases = n()) %>% 
  ungroup() 

rash_join <- full_join(
  rash_denominator, rash_numerator, 
  by = "usubjid", "samodify2")

label(rash_join$samodify2.y) <- "rash numerator"
label(rash_join$samodify2.x) <- "rash denominator"
table1(~ samodify2.y + samodify2.x, data = rash_join, 
       topclass = "Rtable1-zebra")


## COMPLETE TABLE 3: domain characteristics ----

dm_unique_patient <- dm_clean %>%
  filter(!(studyid %in% c("ESBMRS", 
                          "EFFVXT"))) %>%
  distinct()

sc_unique_patient <- sc_clean %>%
  distinct(studyid, usubjid)

er_unique_patient <- er_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

sv_unique_patient <- sv_clean %>%
  distinct(studyid, usubjid)

ho_unique_patient <- ho_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

rp_unique_patient <- rp_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

po_unique_patient <- po_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

vs_unique_patient <- vs_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

sa_unique_patient <- sa_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

lb_unique_patient <- lb_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

mb_unique_patient <- mb_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

in_unique_patient <- in_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

rs_unique_patient <- rs_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)

ds_unique_patient <- ds_clean %>%
  filter(!(studyid %in% c("ESBMRS", 
                          "EFFVXT"))) %>%
  distinct(studyid, usubjid)

dd_unique_patient <- dd_clean %>%
  distinct(studyid, usubjid)

ts_unique_patient <- ts_clean %>%
  filter(!(studyid %in% c("ESBMRS", 
                          "EFFVXT"))) %>%
  distinct(studyid, usubjid)

ti_unique_patient <- ti_clean %>%
  filter(!(studyid %in% "ESBMRS")) %>%
  distinct(studyid, usubjid)




# End of code

