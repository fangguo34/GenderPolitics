# Basic Setup -------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readr)

library(lubridate)
library(stringr)
library(RecordLinkage)

library(readstata13)
library(foreign)

library(geosphere)
library(rgdal)
library(rgeos)
library(sp)
library(sf)

library(rdrobust)
library(fixest)

library(ggplot2)

library(data.table)
library(feather)
library(dplyr)
library(dtplyr)
library(broom)
library(xtable)
library(texreg)

library(RecordLinkage)
library(MatchIt)

path <- "~/zfs/projects/students/fangg-voters/data/"

gender <- paste0(path, "gender_politics/")
import <- paste0(path, "import/")
export <- paste0(path, "export/")
tmp <- paste0(path, "tmp/")
shp <- paste0(path, "shapefiles/")
contrib <- paste0(import, "dime/contribDB/")

# Functions ---------------------------------------------------------------
merge_and_clean <- function(bio_data) {
  bio_data_output <- bio_data %>% 
    left_join(fundraising_data, by = c("general_election_id", "primary_election_id", "bonica.rid", "candidate", "candidate_lname",
                                       "state", "cycle")) %>% 
    group_by(general_election_id) %>%
    mutate(n_i = sum(Incum.Chall == "I"),
           n_c = sum(Incum.Chall == "C"),
           n_o = sum(Incum.Chall == "O"),
           n_blank = sum(Incum.Chall == "")) %>%
    ungroup() %>%
    mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                                ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)))
  
  return(bio_data_output)
  
}

create_var <- function(data = bio_data_tmp, colname = "degree", varname = "has_jd", keyword = "JD") {
  data_tmp <- data[c(paste0(colname, 1), paste0(colname, 2), paste0(colname, 3))] 
  colnames(data_tmp) <- c("col1", "col2", "col3")
  
  data_output <- data %>% 
    bind_cols(data_tmp) %>% 
    mutate(!!varname := ifelse(grepl(keyword, col1), 1, 
                               ifelse(grepl(keyword, col2), 1,
                                      ifelse(grepl(keyword, col3), 1, 0)))) %>% 
    select(-col1, -col2, -col3)
  
  return(data_output)
  
}

prepare_reg_data <- function(data = bio_data_final) {
  reg_data = data %>% 
    filter(!is.na(recipient.cfscore)) %>%
    mutate(y_p = ifelse(is.na(y_p), 0, y_p),
           y_p_f = ifelse(is.na(y_p_f), 0, y_p_f),
           y_p_m = ifelse(is.na(y_p_m), 0, y_p_m),
           y_p_fcc = ifelse(is.na(y_p_fcc), 0, y_p_fcc)) %>% 
    mutate(FemaleCand = ifelse(gender == 1, 1, 0),
           PrimaryMargin = win_margin_primary,
           NumberCands = n_cands_primary,
           OpenSeat = ifelse(Incum.Chall == "O", 1, 0),
           Incumbent = ifelse(Incum.Chall == "I", 1, 0),
           CFScore = recipient.cfscore,
           Extreme = abs(recipient.cfscore),
           district_party = paste0(district, "_", party)) %>% 
    mutate(log_y_p = log1p(y_p),
           log_y_p_f = log1p(y_p_f),
           log_y_p_m = log1p(y_p_m),
           log_y_p_fcc = log1p(y_p_fcc))
  
}

# Data ---------------------------------------------------------------
bio_data <- read.csv(paste0(gender, "11_bio_detail_all.csv"), stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  filter((degree1 != "" | degree2 != "" | degree3 != "") 
         & (prof_title1 != "" | prof_title2 != "" | prof_title3 != "")) %>% 
  mutate(across(c(30:47), tolower)) %>% 
  mutate(age = cycle - as.numeric(str_extract(birthdate, "\\d{4}")))

# write.csv()

fundraising_data <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id, primary_election_id, bonica.rid, candidate) %>%
  slice(which.max(candpct)) %>%
  filter(cycle >= 1992) %>% 
  mutate(win_primary = ifelse(candpct == p0, 1, 0))

# test <- fundraising_data %>% 
#   filter(gender == 1) %>% 
#   replace(is.na(.), 0) 
# 
# summary(lm(log1p(y_p_f) ~ log1p(y_p_fc), test))
# 
# indep_vars = c("log1p(y_p_fc)", "factor(Incum.Chall)", "factor(party)")
# fe_vars = c("district", "cycle")
# outcome_var = "log1p(y_p_f)"
# 
# fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
# 
# model = feols(fml, cluster = "primary_election_id", test)
# summary(model)

# fundraising_data <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather")) %>% 
#   group_by(general_election_id, primary_election_id, bonica.rid, candidate) %>%
#   slice(which.max(candpct)) %>%
#   filter(cycle >= 1992) 

# # Cands with missing edu are less likely to win ---------------------------
# data_availability <- fundraising_data %>%
#   select(general_election_id, primary_election_id, bonica.rid, candidate, gender, candpct, win_primary) %>%
#   left_join(bio_data, by = c("general_election_id", "primary_election_id", "bonica.rid", "candidate")) %>%
#   mutate(available = ifelse(!is.na(votesmart_id), 1, 0))
# 
# data_summary <- data_availability %>%
#   group_by(available) %>%
#   summarise(n = n(),
#             mean_candpct = mean(candpct),
#             median_candpct = median(candpct),
#             mean_win = mean(win_primary),
#             median_win = median(win_primary),
#             mean_women = mean(gender))

# Analyze Education ----------------------------------------------------------------
bio_data_final <- bio_data %>% 
  mutate(children = as.numeric(ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+Children)")), str_extract(family, "(\\d+)(?=\\s+Children)"),
                                      ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+Child)")), str_extract(family, "(\\d+)(?=\\s+Child)"),
                                             ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+children)")), str_extract(family, "(\\d+)(?=\\s+children)"),
                                                    ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+child)")), str_extract(family, "(\\d+)(?=\\s+child)"),
                                                           0)))))) %>% 
  create_var("degree", "bachelor_flag", "BA|BS") %>% 
  create_var("degree", "jd_flag", "JD") %>% 
  create_var("degree", "mba_flag", "MBA") %>% 
  create_var("degree", "master_flag", "MA|MS|MPA|MFA|Master") %>% 
  create_var("degree", "md_flag", "MD|DDS") %>% 
  create_var("degree", "phd_flag", "PhD") %>% 
  create_var("prof_title", "law_flag", "lawyer|law|attorney") %>% 
  create_var("prof_title", "doctor_flag", "physician|doctor|dentist|medicine|medical") %>% 
  create_var("prof_title", "business_flag", "president|owner|executive|ceo|partner|manager|chairman|director|banker|principal|entrepreneur|business") %>% 
  create_var("prof_title", "teacher_flag", "teacher") %>% 
  create_var("political_organization", "state_leg_flag", "state senate|state house") 
  # mutate(ability_index = bachelor_flag + jd_flag + mba_flag + master_flag + md_flag + phd_flag + law_flag + doctor_flag + business_flag) 

bio_data_merged <- merge_and_clean(bio_data_final)

write_feather(bio_data_merged, paste0(gender, "12_bio_data_merged_with_primary_funding.feather"))

# Summary Stats -----------------------------------------------------------
data_summary <- bio_data_merged %>%
  group_by(gender) %>%
  summarise(n = n(),
            mean_bachelor = mean(bachelor_flag),
            mean_jd = mean(jd_flag),
            mean_md = mean(md_flag),
            mean_mba = mean(mba_flag),
            mean_master = mean(master_flag),
            mean_phd = mean(phd_flag),
            mean_law = mean(law_flag),
            mean_doctor = mean(doctor_flag),
            mean_business = mean(business_flag),
            mean_teacher = mean(teacher_flag),
            mean_children = mean(children),
            mean_age = mean(age))

data_summary_year <- bio_data_merged %>%
  group_by(cycle, gender) %>%
  summarise(n = n(),
            mean_bachelor = mean(bachelor_flag),
            mean_jd = mean(jd_flag),
            mean_md = mean(md_flag),
            mean_mba = mean(mba_flag),
            mean_master = mean(master_flag),
            mean_phd = mean(phd_flag),
            mean_law = mean(law_flag),
            mean_doctor = mean(doctor_flag),
            mean_business = mean(business_flag),
            mean_teacher = mean(teacher_flag),
            mean_children = mean(children),
            mean_age = mean(age)) %>% 
  mutate(gender = ifelse(gender == 0, "M", "F"))


# Summary Plots -----------------------------------------------------------
for (i in colnames(data_summary_year)[14]) {
  ggplot(data_summary_year, aes_string(x = "cycle", y = i, color = "gender")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1992, 2018, by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(i)
  ggsave(paste0(gender, "plots/12_edu_prof_by_cycle_gender_", i, ".png"))
} 

# outcomes_list <- colnames(bio_data_merged)[c(48:57)]
# 
# # Use logit???
# for (i in outcomes_list) {
#   print(i)
#   fml <- as.formula(paste0(i, "~gender"))
#   fit <- lm(fml, bio_data_merged)
#   print(summary(fit))
# }


# summary(lm(business_flag~gender, bio_data_merged))

test <- bio_data_final %>% 
  filter(master_flag == 1)
female_data <- bio_data_merged %>% 
  filter(gender == 1) %>% 
  select(general_election_id:candidate, gender, degree1:span3)

# Simple OLS --------------------------------------------------------------  
bio_data_reg <- prepare_reg_data(bio_data_merged) %>% 
  filter(cycle < 2010)

i = "log_y_p"

indep_vars = c("FemaleCand",
               "Incumbent", "FemaleCand:Incumbent",
               "Extreme", "FemaleCand:Extreme",
               "bachelor_flag", "FemaleCand:bachelor_flag",
               "jd_flag", "FemaleCand:jd_flag",
               "md_flag", "FemaleCand:md_flag",
               "mba_flag", "FemaleCand:mba_flag",
               "master_flag", "FemaleCand:master_flag",
               "phd_flag", "FemaleCand:phd_flag",
               "law_flag", "FemaleCand:law_flag",
               "business_flag", "FemaleCand:business_flag",
               "doctor_flag", "FemaleCand:doctor_flag",
               "state_leg_flag", "FemaleCand:state_leg_flag")

indep_vars = c("FemaleCand",
               "Incumbent", "FemaleCand:Incumbent",
               "Extreme",
               "bachelor_flag",
               "jd_flag",
               "md_flag",
               "mba_flag",
               "master_flag",
               "phd_flag",
               "law_flag",
               "business_flag",
               "doctor_flag",
               "state_leg_flag")


# indep_vars = c("FemaleCand",
#                "Incumbent", "FemaleCand:Incumbent",
#                "Extreme", 
#                "bachelor_flag", "FemaleCand:bachelor_flag",
#                "jd_flag", "FemaleCand:jd_flag",
#                "md_flag", "FemaleCand:md_flag",
#                "mba_flag", "FemaleCand:mba_flag",
#                "master_flag", "FemaleCand:master_flag",
#                "phd_flag", "FemaleCand:phd_flag",
#                "law_flag", "FemaleCand:law_flag")
# indep_vars = c("FemaleCand",
#                "Incumbent", "FemaleCand:Incumbent",
#                "Extreme")
# 
# indep_vars = c("FemaleCand")


# indep_vars = c("FemaleCand","Incumbent", 
#                "state_leg_flag")


fe_vars = c("primary_election_id")
outcome_var = i

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))

model = feols(fml, cluster = "primary_election_id", bio_data_reg)
summary(model)


# Battle of sexes ---------------------------------------------------------
fundraising_data <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>% 
  group_by(general_election_id, primary_election_id, bonica.rid) %>% 
  mutate(n_check = n()) %>% 
  ungroup() %>% 
  filter(n_check == 1)

bio <- read.csv(paste0(gender, "12_bio_data_clean.csv")) %>% 
  select(general_election_id:bonica.rid, votesmart_id:law_flag)

final_data <- fundraising_data %>% 
  left_join(bio, by = c("general_election_id", "primary_election_id", "bonica.rid")) %>% 
  group_by(general_election_id, primary_election_id) %>% 
  mutate(n_na_votesmart = sum(is.na(votesmart_id))) %>% 
  ungroup() %>% 
  filter(n_na_votesmart == 0) %>% 
  mutate(ability_index = bachelor_flag + jd_flag + mba_flag + master_flag + md_flag + phd_flag + law_flag) %>% 
  group_by(general_election_id, primary_election_id) %>% 
  mutate(female_ability_advantage = ability_index[gender == 1] - ability_index[gender == 0])

# test_summary <- final_data %>% 
#   group_by(gender) %>% 
#   summarise(n = n(),
#             n_na_votesmart = sum(is.na(votesmart_id)))

test <- final_data %>% 
  select(general_election_id:candpct, primary_winner:female_primary_vote_margin, ability_index, female_ability_advantage)

# Battle of Sexes Regression --------------------------------------------------------------
run_reg <- function(outcome = "log_y_p", drop_na = 0, m = -10, l = 10) {
  if (drop_na == 1) {
    reg_data = final_data %>% 
      filter(!is.na(y_p)) 
    # filter(!is.na(recipient.cfscore))
    
  } else if (drop_na == 0) {
    reg_data = final_data %>% 
      # filter(!is.na(recipient.cfscore)) %>% 
      replace(is.na(.), 0)
    
  }
  
  reg_data <- reg_data %>% 
    filter(female_ability_advantage >= m
           & female_ability_advantage <= l) %>% 
    mutate(log_y_p = log1p(y_p),
           log_y_p_f = log1p(y_p_f),
           log_y_p_m = log1p(y_p_m),
           log_y_p_fcc = log1p(y_p_fcc),
           extreme = abs(recipient.cfscore)) 
  
  # indep_vars = c("gender", "ability_index")
  indep_vars = c("gender")
  fe_vars = c("primary_election_id")
  outcome_var = outcome
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "primary_election_id", reg_data) # when use cluster se, don't use se = hetero
  
  return(model)
  
}

run_reg("log_y_p", 0, -10, 10)

# run_reg("ability_index", 0, -10, 10)

run_reg("log_y_p", 0, 1, 10)
run_reg("log_y_p", 0, -10, -1)
run_reg("log_y_p", 0, -1, 1)

final_data_win <- final_data %>% 
  filter(primary_winner == 1) 

mean(final_data_win$female_ability_advantage) # pretty small


final_data_win <- final_data %>% 
  filter(primary_winner == 1 & abs(female_primary_vote_margin) <= 0.1) 

mean(final_data_win$female_ability_advantage) # quite sizable


summary_data <- final_data %>% 
  group_by(gender) %>% 
  summarise(mean_ability = mean(ability_index))

