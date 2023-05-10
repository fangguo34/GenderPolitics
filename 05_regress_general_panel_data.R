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

path <- "~/zfs/projects/students/fangg-voters/data/"

gender <- paste0(path, "gender_politics/")
import <- paste0(path, "import/")
export <- paste0(path, "export/")
tmp <- paste0(path, "tmp/")
shp <- paste0(path, "shapefiles/")
contrib <- paste0(import, "dime/contribDB/")


# # Cook PVI ----------------------------------------------------------------
# pvi_data <- read.csv(paste0(import, "cook/pvi_2016.csv")) %>% 
#   mutate(Dist = gsub("-", "", Dist)) %>% 
#   mutate(PVI_party = str_extract(PVI, "[A-Z]+"),
#          PVI_magnitude = ifelse(str_extract(PVI, "[A-Z]+") == "EVEN", 0, str_extract(PVI, "[[:digit:]]+"))) %>% 
#   mutate(PVI_final = ifelse(PVI_party == "R", as.numeric(PVI_magnitude), -as.numeric(PVI_magnitude))) %>% 
#   select(Dist, PVI, PVI_party, PVI_final)
#   
                                 
# Import general Data -------------------------------------------------------
general_data_raw <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)))


  # replace(is.na(.), 0)
  # mutate_at(vars(y_g_f, y_g_m), ~replace_na(., 0))

# Subset Data -------------------------------------------------------------
# general_data_has_female <- general_data_raw %>% 
#   group_by(general_election_id) %>% 
#   mutate(n_f_cands_general = sum(gender),
#          n_cands_general = n()) %>% 
#   ungroup() %>% 
#   filter(n_f_cands_general >= 1 & n_cands_general >= 1)
# 
# general_data_mix_gender <- general_data_raw %>% 
#   group_by(general_election_id) %>% 
#   mutate(n_f_cands_general = sum(gender),
#          n_m_cands_general = sum(gender == 0)) %>% 
#   ungroup() %>% 
#   filter(n_f_cands_general >= 1 & n_m_cands_general >= 1)

general_panel_data <- general_data_raw %>% 
  replace(is.na(.), 0) %>% 
  arrange(general_election_id, desc(voteshare_general)) %>% 
  group_by(general_election_id, district, cycle) %>% 
  summarise(total_y_g_f = sum(y_g_f),
            total_y_g_m = sum(y_g_m),
            total_y_g_fcc = sum(y_g_fcc),
            total_y_g = sum(y_g),
            pct_y_g_f = sum(y_g_f)/(sum(y_g_f) + sum(y_g_m)),
            pct_y_g_m = sum(y_g_m)/(sum(y_g_f) + sum(y_g_m)),
            total_n_g_f = sum(n_g_f),
            total_n_g_m = sum(n_g_m),
            total_n_g_fcc = sum(n_g_fcc),
            total_n_g = sum(n_g),
            pct_n_g_f = sum(n_g_f)/(sum(n_g_f) + sum(n_g_m)),
            pct_n_g_m = sum(n_g_m)/(sum(n_g_f) + sum(n_g_m)),
            n_f_cands_general = sum(gender),
            OpenSeat = as.numeric(sum(Incum.Chall == "I") == 0),
            NumberCands = n(),
            GeneralMargin = voteshare_general[1] - ifelse(is.na(voteshare_general[2]), 0, voteshare_general[2])) %>% 
  ungroup() %>%  
  mutate(FemaleCand = as.numeric(n_f_cands_general > 0),
         total_y_g_ind = total_y_g_f + total_y_g_m,
         log_total_y_g = log1p(total_y_g),
         log_total_y_g_f = log1p(total_y_g_f),
         log_total_y_g_m = log1p(total_y_g_m),
         log_total_y_g_fcc = log1p(total_y_g_fcc),
         log_total_n_g = log1p(total_n_g),
         log_total_n_g_f = log1p(total_n_g_f),
         log_total_n_g_m = log1p(total_n_g_m),
         log_total_n_g_fcc = log1p(total_n_g_fcc)) 

write.dta(general_panel_data, paste0(gender, "05_general_panel_data.dta"))


# %>% 
#   left_join(pvi_data, by = c("district" = "Dist")) %>% 
#   mutate(NonCompetitive = ifelse(is.na(PVI_final), NA, abs(PVI_final)))

# Summary Stats -----------------------------------------------------------
general_person_cycle_summary_1 <- general_data_raw %>% 
  replace(is.na(.), 0) %>% 
  group_by(gender) %>% 
  summarise(mean_y_g = mean(y_g),
            mean_y_g_f = mean(y_g_f),
            mean_y_g_m = mean(y_g_m),
            mean_y_g_fcc = mean(y_p_fcc),
            mean_general_voteshr = mean(voteshare_general),
            prob_win_general = mean(general_winner),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(general_person_cycle_summary_1, paste0(gender, "05_general_person_cycle_summary.csv"), row.names = FALSE)

general_district_cycle_summary_1 <- general_panel_data %>% 
  group_by(FemaleCand) %>% 
  summarise(mean_total_y_g = mean(total_y_g),
            mean_total_y_g_f = mean(total_y_g_f),
            mean_total_y_g_m = mean(total_y_g_m),
            mean_total_y_g_fcc = mean(total_y_g_fcc),
            mean_general_margin = mean(GeneralMargin),
            mean_n_cands = mean(NumberCands),
            mean_open_seats = mean(OpenSeat),
            n_district_cycle = n())
write.csv(general_district_cycle_summary_1, paste0(gender, "05_general_district_cycle_summary.csv"), row.names = FALSE)

# within the general have female districts 
general_data_has_female_summary <- general_data_has_female %>% 
  group_by(gender) %>% 
  summarise(mean_y_g = mean(y_g),
            mean_y_g_f = mean(y_g_f),
            mean_y_g_m = mean(y_g_m),
            mean_y_g_fcc = mean(y_p_fcc),
            mean_general_voteshr = mean(voteshare_general),
            prob_win_general = mean(general_winner),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(general_data_has_female_summary, paste0(gender, "05_general_data_candidate_cycle_has_female_summary.csv"), row.names = FALSE)

# mix gender
general_data_mix_gender_summary <- general_data_mix_gender %>% 
  group_by(gender) %>% 
  summarise(mean_y_g = mean(y_g),
            mean_y_g_f = mean(y_g_f),
            mean_y_g_m = mean(y_g_m),
            mean_y_g_fcc = mean(y_p_fcc),
            mean_general_voteshr = mean(voteshare_general),
            prob_win_general = mean(general_winner),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(general_data_mix_gender_summary, paste0(gender, "05_general_data_candidate_cycle_mix_gender_summary.csv"), row.names = FALSE)


# Summary Plots -----------------------------------------------------------
general_person_cycle_summary_2 <- general_data_raw %>% 
  group_by(cycle, gender) %>% 
  summarise(mean_y_g = mean(y_g),
            mean_y_g_f = mean(y_g_f),
            mean_y_g_m = mean(y_g_m),
            mean_y_g_fcc = mean(y_g_fcc),
            mean_general_voteshr = mean(voteshare_general),
            prob_win_general = mean(general_winner),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n()) %>% 
  arrange(cycle, gender) %>% 
  mutate(gender = ifelse(gender == 0, "M", "F"))

for (i in colnames(general_person_cycle_summary_2)[3:12]) {
  ggplot(general_person_cycle_summary_2, aes_string(x = "cycle", y = i, color = "gender")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1980, 2018, by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(i)
  ggsave(paste0(gender, "plots/05_general_person_cycle_summary_by_year_", i, ".png"))
} 


general_district_cycle_summary_2 <- general_panel_data %>% 
  group_by(cycle, FemaleCand) %>% 
  summarise(n_races = n(),
            mean_total_y_g = mean(total_y_g),
            mean_total_y_g_f = mean(total_y_g_f),
            mean_total_y_g_m = mean(total_y_g_m),
            mean_total_y_g_fcc = mean(total_y_g_fcc),
            mean_general_margin = mean(GeneralMargin),
            mean_n_cands = mean(NumberCands),
            mean_open_seats = mean(OpenSeat)) %>% 
  arrange(cycle, FemaleCand) %>% 
  mutate(FemaleCand = ifelse(FemaleCand == 0, "M", "F"))

for (i in colnames(general_district_cycle_summary_2)[3:10]) {
  ggplot(general_district_cycle_summary_2, aes_string(x = "cycle", y = i, color = "FemaleCand")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1980, 2018, by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(i)
  ggsave(paste0(gender, "plots/05_general_district_cycle_summary_by_year_", i, ".png"))
} 




# Specific Election-Year (District-Year) Level Regression -------------------------------------------------------------
reg_data = general_panel_data 

# %>% 
#   filter(cycle == 2016)
# %>% 
#   replace(is.na(.), 0)

run_general_election_year_reg <- function(outcome, reg_data) {
  indep_vars = c("FemaleCand",
                 "GeneralMargin",
                 "NumberCands",
                 "OpenSeat")
  
  # indep_vars = c("FemaleCand",
  #                "GeneralMargin", "FemaleCand:GeneralMargin",
  #                "NumberCands", "FemaleCand:NumberCands",
  #                "OpenSeat", "FemaleCand:OpenSeat")
  # 
  # indep_vars = c("FemaleCand",
  #                "NumberCands", "FemaleCand:NumberCands",
  #                "OpenSeat", "FemaleCand:OpenSeat")
  
  # indep_vars = c("FemaleCand",
  #                "NumberCands", 
  #                "OpenSeat")
  
  
  fe_vars = c("district", "cycle")
  outcome_var = outcome
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  # fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+")))
  model = feols(fml, cluster = "district", reg_data)
  
  return(model)
  
}

model.1 <- run_general_election_year_reg("log_total_y_g", reg_data)
model.2 <- run_general_election_year_reg("log_total_y_g_f", reg_data)
model.3 <- run_general_election_year_reg("log_total_y_g_m", reg_data)
model.4 <- run_general_election_year_reg("log_total_y_g_fcc", reg_data)
texreg(list(model.1, model.2, model.3, model.4))

run_general_election_year_reg("pct_y_g_f", reg_data)

# model.1 <- run_general_election_year_reg("total_n_g", reg_data)
# model.2 <- run_general_election_year_reg("total_n_g_f", reg_data)
# model.3 <- run_general_election_year_reg("total_n_g_m", reg_data)
# model.4 <- run_general_election_year_reg("total_n_g_fcc", reg_data)
# texreg(list(model.1, model.2, model.3, model.4))

model.1 <- run_general_election_year_reg("log_total_n_g", reg_data)
model.2 <- run_general_election_year_reg("log_total_n_g_f", reg_data)
model.3 <- run_general_election_year_reg("log_total_n_g_m", reg_data)
model.4 <- run_general_election_year_reg("log_total_n_g_fcc", reg_data)
texreg(list(model.1, model.2, model.3, model.4))

run_general_election_year_reg("pct_n_g_f", reg_data)

# texreg(list(run_general_election_year_reg("total_y_g_m", reg_data),
#             run_general_election_year_reg("log_total_y_g_m", reg_data),
#             run_general_election_year_reg("pct_y_g_m", reg_data)))
# 
# 
# run_general_election_year_reg("log_total_y_g_fcc", reg_data)

#
reg_data = general_panel_data %>% 
  mutate(state = str_extract(district, "[A-Z]+")) %>% 
  filter(cycle == 2014)

indep_vars = c("FemaleCand", 
               "GeneralMargin", "FemaleCand:GeneralMargin",
               "NumberCands", "FemaleCand:NumberCands",
               "OpenSeat", "FemaleCand:OpenSeat")
# indep_vars = c("FemaleCand")
outcome_var = "log_total_y_g_f"
fe_vars = c("state")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
model = feols(fml, cluster = "state", reg_data)
summary(model)


# Specific Candidate-Year Level Regression ----------------------------------------------------------------
outcome = "y_g"
ideology = "CFScore"
first_year = 1980
last_year = 2018
raw_data = general_data_raw

run_general_candidate_year_reg <- function(outcome, ideology = "None", first_year, last_year, raw_data) {
  reg_data = raw_data %>% 
    arrange(general_election_id, desc(voteshare_general)) %>% 
    mutate(FemaleCand = ifelse(gender == 1, 1, 0),
           NumberCands = n_cands_primary,
           OpenSeat = ifelse(Incum.Chall == "O", 1, 0),
           Incumbent = ifelse(Incum.Chall == "I", 1, 0),
           CFScore = recipient.cfscore,
           Extreme = abs(recipient.cfscore),
           pct_y_g_m_ind = y_g_m/(y_g_m + y_g_f)) %>%
    group_by(general_election_id) %>% 
    mutate(GeneralMargin = voteshare_general[1] - ifelse(is.na(voteshare_general[2]), 0, voteshare_general[2])) %>% 
    filter(!is.na(CFScore) & cycle >= first_year & cycle <= last_year)
  
  if (ideology == "None") {
    indep_vars = c("FemaleCand", 
                   "GeneralMargin", "FemaleCand:GeneralMargin",
                   "NumberCands", "FemaleCand:NumberCands",
                   "OpenSeat", "FemaleCand:OpenSeat",
                   "Incumbent", "FemaleCand:Incumbent")
    
  } else {
    indep_vars = c("FemaleCand", 
                   "GeneralMargin", "FemaleCand:GeneralMargin",
                   "NumberCands", "FemaleCand:NumberCands",
                   "OpenSeat", "FemaleCand:OpenSeat",
                   "Incumbent", "FemaleCand:Incumbent",
                   ideology, paste0("FemaleCand:", ideology)) 
  }
  
  fe_vars = c("district", "cycle")
  outcome_var = outcome
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "district", reg_data)
  
  return(model) 
  
}

# ??? How to deal with NA???


summary(run_general_candidate_year_reg("pct_y_g_m_ind", "Extreme", 1980, 2018, general_data_raw))


summary(run_general_candidate_year_reg("y_g", "Extreme", 1980, 2018, general_data_raw))
summary(run_general_candidate_year_reg("y_g", "CFScore", 1980, 2018, general_data_raw))

summary(run_general_candidate_year_reg("log(y_g_f)", "Extreme", 1980, 2018, general_data_raw))
summary(run_general_candidate_year_reg("log(y_g_f)", "CFScore", 1980, 2018, general_data_raw))

summary(run_general_candidate_year_reg("log(y_g_m)", "Extreme", 1980, 2018, general_data_raw))
summary(run_general_candidate_year_reg("log(y_g_m)", "CFScore", 1980, 2018, general_data_raw))

summary(run_general_candidate_year_reg("log(y_g_fcc)", "Extreme", 1980, 2018, general_data_raw))
summary(run_general_candidate_year_reg("log(y_g_fcc)", "CFScore", 1980, 2018, general_data_raw))

# Specific Candidate-Year Level Regression district-year FE ----------------------------------------------------------------
reg_data <- general_data_raw %>% 
  arrange(general_election_id, desc(voteshare_general)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(FemaleCand = ifelse(gender == 1, 1, 0),
         DemCand = ifelse(party == "100", 1, 0),
         NumberCands = n_cands_primary,
         OpenSeat = ifelse(Incum.Chall == "O", 1, 0),
         Incumbent = ifelse(Incum.Chall == "I", 1, 0),
         CFScore = recipient.cfscore,
         Extreme = abs(recipient.cfscore)) %>% 
  mutate(log_y_g = log1p(y_g),
         log_y_g_f = log1p(y_g_f),
         log_y_g_m = log1p(y_g_m),
         log_y_g_fcc = log1p(y_g_fcc))

run_reg <- function(outcome = "log_y_g") {
  # indep_vars = c("FemaleCand", "Incumbent", "FemaleCand:Incumbent")
  indep_vars = c("FemaleCand", "Incumbent", "FemaleCand:Incumbent",
                 "DemCand", "FemaleCand:DemCand", "voteshare_general", "FemaleCand:voteshare_general")
  
  fe_vars = c("general_election_id")
  outcome_var = outcome
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "general_election_id", reg_data)
  
  return(model)
  
}

model_1 <- run_reg("log_y_g")
model_2 <- run_reg("log_y_g_f")
model_3 <- run_reg("log_y_g_m")
model_4 <- run_reg("log_y_g_fcc")
texreg(list(model_1, model_2, model_3, model_4))



# Test --------------------------------------------------------------------
# indep_vars = c("FemaleCand", "OpenSeat", "Incumbent", 
#                "FemaleCand:OpenSeat", "FemaleCand:Incumbent")
# fe_vars = c("general_election_id")
# outcome_var = "log_y_g_f"
# 
# fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
# model = feols(fml, cluster = "general_election_id", reg_data)
# summary(model)
# 
# 
# indep_vars = c("FemaleCand", "OpenSeat", "Incumbent", 
#                "FemaleCand:OpenSeat", "FemaleCand:Incumbent")
# fe_vars = c("general_election_id")
# outcome_var = "log_y_g_m"
# 
# fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
# model = feols(fml, cluster = "general_election_id", reg_data)
# summary(model)
# 
# 
# indep_vars = c("FemaleCand", "OpenSeat", "Incumbent", 
#                "FemaleCand:OpenSeat", "FemaleCand:Incumbent")
# fe_vars = c("general_election_id")
# outcome_var = "log(y_g_m)"
# 
# fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
# model = feols(fml, cluster = "general_election_id", reg_data)
# summary(model)
# 
